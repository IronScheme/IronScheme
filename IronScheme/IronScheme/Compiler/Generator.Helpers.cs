#region License
/* ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 
 *
 * This source code is subject to terms and conditions of the Microsoft Public License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Public License.
 *
 * You must not remove this notice, or any other, from this software.
 * ***************************************************************************/
#endregion

using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting;
using Microsoft.Scripting.Ast;
using System.Reflection;
using IronScheme.Runtime;
using Microsoft.Scripting.Hosting;
using IronScheme.Hosting;
using Microsoft.Scripting.Actions;

namespace IronScheme.Compiler
{
  public abstract class BaseHelper
  {
    static readonly IronSchemeScriptEngine se;
    internal static CodeContext cc;
    internal static readonly ActionBinder binder;
    static readonly LanguageProvider lp = ScriptDomainManager.CurrentManager.GetLanguageProvider(
          typeof(Hosting.IronSchemeLanguageProvider));

    protected static LanguageProvider LanguageProvider
    {
      get { return BaseHelper.lp; }
    } 

    protected static ScriptEngine ScriptEngine
    {
      get { return BaseHelper.se; }
    }

    protected static CodeContext Context
    {
      get { return cc; }
    }

    protected static ActionBinder Binder
    {
      get { return BaseHelper.binder; }
    } 

    
    static BaseHelper()
    {
      se = lp.GetEngine() as IronSchemeScriptEngine;

      ModuleContext mc = new ModuleContext(ScriptDomainManager.CurrentManager.CreateModule("CompileTime"));

      mc.CompilerContext = new CompilerContext(SourceUnit.CreateSnippet(se, ""));

      cc = se.CreateContext(mc);

      binder = new IronScheme.Actions.IronSchemeActionBinder(cc);
    }
  }

  partial class Generator : BaseHelper
  {
    readonly static Dictionary<CodeBlock, Scope> scopemap = new Dictionary<CodeBlock, Scope>();

    internal static CodeBlock evalblock;

    static void Initialize()
    {
      // builtin methods
      AddGenerators(Context, typeof(Generator).Assembly);
      // HACK: clean up needed
      SymbolId s = SymbolTable.StringToId("call-with-values");
      cc.Scope.SetName(s, new BuiltinMethod(s.ToString(), ReflectionCache.GetMethodGroup(typeof(OptimizedBuiltins), "CallWithValues")));

      Closure.AssertionViolation = Builtins.AssertionViolation;

      AddBuiltins(Context, typeof(Builtins));
      AddInlineEmitters(typeof(BuiltinEmitters));

      AddBuiltins(Context, typeof(Runtime.psyntax.AnnotatedReader));
      AddBuiltins(Context, typeof(Runtime.psyntax.Serialization));

      AddBuiltins(Context, typeof(Runtime.R6RS.Records));
      AddBuiltins(Context, typeof(Runtime.R6RS.Hashtables));
      AddBuiltins(Context, typeof(Runtime.R6RS.Unicode));
      AddBuiltins(Context, typeof(Runtime.R6RS.ByteVectors));
      AddBuiltins(Context, typeof(Runtime.R6RS.Sorting));
      AddBuiltins(Context, typeof(Runtime.R6RS.Enums));
      AddBuiltins(Context, typeof(Runtime.R6RS.IO));
      AddBuiltins(Context, typeof(Runtime.R6RS.Lists));
      AddBuiltins(Context, typeof(Runtime.R6RS.Exceptions));
      AddBuiltins(Context, typeof(Runtime.R6RS.Conditions));
      AddBuiltins(Context, typeof(Runtime.R6RS.Programs));

      AddBuiltins(Context, typeof(Runtime.R6RS.Arithmetic.Flonums));
      AddBuiltins(Context, typeof(Runtime.R6RS.Arithmetic.Fixnums));
      AddBuiltins(Context, typeof(Runtime.R6RS.Arithmetic.Bitwise));

      AddInlineEmitters(typeof(Runtime.R6RS.Arithmetic.FlonumsInlineEmitters));
      AddInlineEmitters(typeof(Runtime.R6RS.Arithmetic.FixnumsInlineEmitters));

    }

    public static void AddBuiltins(CodeContext cc, Type builtinstype)
    {
      BuiltinMethod.binder = binder;
      BuiltinMethod.context = cc;

      Dictionary<string, List<MethodBase>> all = new Dictionary<string, List<MethodBase>>();

      foreach (MethodInfo mi in builtinstype.GetMethods(BindingFlags.DeclaredOnly | BindingFlags.Public | BindingFlags.Static))
      {
        foreach (BuiltinAttribute ba in mi.GetCustomAttributes(typeof(BuiltinAttribute), false))
        {
          string name = ba.Name ?? mi.Name.ToLower();
          List<MethodBase> meths;
          if (!all.TryGetValue(name, out meths))
          {
            all[name] = meths = new List<MethodBase>();
          }
          meths.Add(mi);
        }
      }

      foreach (string mn in all.Keys)
      {
        SymbolId s = SymbolTable.StringToId(mn);
        cc.Scope.SetName(s, new BuiltinMethod(mn, ReflectionCache.GetMethodGroup(mn, all[mn].ToArray())));
      }
    }

    protected static CodeBlock GetTopLevel(CodeBlock cb)
    {
      while (cb.Parent != null)
      {
        cb = cb.Parent;
      }
      return cb;
      //if (cb.IsGlobal)
      //{
      //  return cb;
      //}
      //else
      //{
      //  return null;
      //}
    }

    static Expression Read(SymbolId name, CodeBlock cb, Type type)
    {
      SymbolId sname = name;

      Scope scope = GetScope(cb);

      object cvalue;
      if (!scope.TryLookupName(sname, out cvalue))
      {
        scope.SetName(sname, cvalue = (type ?? typeof(object)));
      }

      Type t = cvalue as Type;

      Variable v = FindVar(cb, sname);

      if (v == null)
      {
        CodeBlock tl = GetTopLevel(cb);
        v = tl.CreateVariable(sname, Variable.VariableKind.Global, typeof(object), Ast.Read(sname));
        return Ast.Read(v);
      }

      if (t.IsAssignableFrom(v.Type))
      {
        return Ast.Read(v);
      }
      else
      {
        return Ast.ConvertHelper(Ast.Read(v), t);
      }
    }

    protected static Variable Create(SymbolId sname, CodeBlock cb, Type type)
    {
      if (cb.Name == "__toploop__")
      {
        if (evalblock == null)
        {
          evalblock = cb;
        }
        else
        {
          cb = evalblock;
        }
      }

      Scope scope = GetScope(cb);

      object cvalue;
      if (!scope.TryGetName(sname, out cvalue))
      {
        scope.SetName(sname, cvalue = (type ?? typeof(object)));
      }

      Type t = cvalue as Type;

      if (t != type)
      {
        object ot;
        if (scope.TryGetName(sname, out ot))
        {
          scope.SetName(sname, type);
        }
      }

      Variable v = MakeVar(cb, sname, typeof(object));
      return v;
    }


    static Variable CreateParameter(SymbolId sname, CodeBlock cb, Type type)
    {
      Scope scope = GetScope(cb);

      object cvalue;
      if (!scope.TryGetName(sname, out cvalue))
      {
        scope.SetName(sname, cvalue = (type ?? typeof(object)));
      }

      Type t = cvalue as Type;

      if (t != type)
      {
        object ot;
        if (scope.TryGetName(sname, out ot))
        {
          scope.SetName(sname, type);
        }
      }

      Variable v = Variable.Parameter(cb, sname, typeof(object));
      cb.Parameters.Add(v);
      return v;
    }

    static Scope GetScope(CodeBlock key)
    {
      if (key.Name == "__toploop__")
      {
        if (evalblock == null)
        {
          evalblock = key;
        }
        else
        {
          key = evalblock;
        }
      }
      Scope cs = null;
      if (!scopemap.TryGetValue(key, out cs))
      {
        Scope parent = null;
        if (key.Parent != null)
        {
          parent = GetScope(key.Parent);
        }
        scopemap[key] = cs = new Scope(parent, null);
      }
      return cs;
    }

    internal static Variable.VariableKind variablelocation = Variable.VariableKind.Local;

    static Variable MakeVar(CodeBlock cb, SymbolId name, Type type)
    {
      //using globals instead of locals does improve performance a lot, compiling is easier too
      //unfortunately they dont play well
      //should really investigate better closure structure
      return cb.CreateVariable(name, cb.IsGlobal ? Variable.VariableKind.Global : variablelocation
        , type ?? typeof(object));
    }

    protected static Variable FindVar(CodeBlock cb, SymbolId name)
    {
      if (cb.Name == "__toploop__")
      {
        if (evalblock == null)
        {
          evalblock = cb;
        }
        else
        {
          cb = evalblock;
        }
      }

      // variables take precidence
      foreach (Variable v in cb.Variables)
      {
        if (v.Name == name)
        {
          return v;
        }
      }

      foreach (Variable v in cb.Parameters)
      {
        if (v.Name == name)
        {
          return v;
        }
      }

      if (cb.Parent != null)
      {
        return FindVar(cb.Parent, name);
      }
      return null;
    }

    readonly static SymbolId list = SymbolTable.StringToId("list");
    readonly static SymbolId liststar = SymbolTable.StringToId("list*");


    protected static MethodInfo MakeList(Expression[] args, bool proper)
    {
      Type[] types = Array.ConvertAll<Expression, Type>(args,
        delegate(Expression e) { return e.Type; });

      MethodBinder listbinder = ((BuiltinMethod) Context.Scope.LookupName(proper ? list : liststar)).Binder;

      return listbinder.MakeBindingTarget(CallType.None, types).Target.Method as MethodInfo;
    }

    protected static string GetLambdaName(CodeBlock cb)
    {
      string fn = GetFullname(NameHint, cb);

      string[] tokens = fn.Split('#');

      string c = null;
      int j = 0;

      for (int i = 0; i < tokens.Length; i++)
      {
        if (tokens[i] != c)
        {
          if (int.TryParse(tokens[i], out j))
          {
            j = i - j;
          }
          else
          {
            c = tokens[i];
            j = i;
          }
        }
        else
        {
          tokens[i] = (i - j).ToString();
        }
      }
      return string.Join("#", tokens);
    }

    protected static Expression MakeCaseClosure(string name, List<CodeBlockDescriptor> cases)
    {
      List<Expression> targets = new List<Expression>();
      List<Expression> arities = new List<Expression>();

      foreach (CodeBlockDescriptor c in cases)
      {
        targets.Add(c.codeblock);
        arities.Add(Ast.Constant(c.arity));
      }
      
      return Ast.SimpleCallHelper(Closure_MakeCase, Ast.CodeContext(),
        Ast.NewArrayHelper(typeof(Delegate[]), targets), Ast.NewArrayHelper(typeof(int[]), arities));
    }

    protected static Expression MakeClosure(CodeBlock cb, bool varargs)
    {
      if (varargs)
      {
        return Ast.SimpleCallHelper(Closure_MakeVarArgsX, Ast.CodeContext(), Ast.CodeBlockExpression(cb, false, false),
          Ast.Constant(cb.Parameters.Count));
      }
      else
      {
        return Ast.SimpleCallHelper(Closure_Make, Ast.CodeContext(), Ast.CodeBlockExpression(cb, false, false));
      }
    }

    protected class CodeBlockDescriptor
    {
      public int arity;
      public CodeBlockExpression codeblock;
    }

    protected internal static void FillBody(CodeBlock cb, List<Statement> stmts, Cons body, bool allowtailcall)
    {

      // declare all define at start of body, then change the define to 'set!'
      // similar to letrec* behaviour; also expand1 the defines
      Cons defcheck = body;
      while (defcheck != null)
      {
        Cons h = defcheck.car as Cons;

        if (h == null)
        {
          break;
        }

        defcheck.car = h = SyntaxExpander.Expand1(defcheck.car) as Cons;

        if (h != null && (bool)Builtins.IsEqual(h.car, define))
        {
          Variable v = Create((SymbolId)Builtins.Second(h), cb, typeof(object));
          //stmts.Add(Ast.Write(v, Ast.ReadField(null, Unspecified)));
          h.car = set;

          assigns[v.Name] = true;
        }
        else
        {
          break;
        }

        defcheck = defcheck.cdr as Cons;
      }

      Cons c = body;
      while (c != null)
      {
        Expression e = GetAst(c.car, cb);
        Statement s = null;
        if (c.cdr == null)
        {
          s = MakeTailCallReturn(allowtailcall, e);
        }
        else
        {
          s = Ast.Statement(e);
        }

        if (c.car is Cons && Parser.sourcemap.ContainsKey(c.car))
        {
          s.SetLoc(Parser.sourcemap[c.car]);
        }
        else
        {
          ;
        }

        stmts.Add(s);
        c = c.cdr as Cons;
      }

      if (stmts.Count == 0)
      {
        stmts.Add(Ast.Return(Ast.ReadField(null, Unspecified)));
      }

      if (cb.Body != null)
      {
        stmts.InsertRange(0, (cb.Body as BlockStatement).Statements);
      }

      cb.Body = Ast.Block(stmts);
    }

    protected static void FillBody(CodeBlock cb, List<Statement> stmts, Cons body, Variable result)
    {

      Cons c = body;
      while (c != null)
      {
        Expression e = GetAst(c.car, cb);
        Statement s = Ast.Statement(e);

        if (c.car is Cons && Parser.sourcemap.ContainsKey(c.car))
        {
          s.SetLoc(Parser.sourcemap[c.car]);
        }
        else
        {
          ;
        }

        stmts.Add(s);
        c = c.cdr as Cons;
      }

      if (stmts.Count == 0)
      {
        stmts.Add(Ast.Write(result, Ast.ReadField(null, Unspecified)));
      }
      else
      {
        stmts.Add(Ast.Write(result, Ast.ConvertHelper((stmts[stmts.Count - 1] as ExpressionStatement).Expression, typeof(object))));
      }

      if (cb.Body != null)
      {
        stmts.InsertRange(0, (cb.Body as BlockStatement).Statements);
      }

      cb.Body = Ast.Block(stmts);
    }

    static Statement MakeTailCallReturn(bool allowtailcall, Expression e)
    {
      if (allowtailcall)
      {
        if (e is MethodCallExpression)
        {
          ((MethodCallExpression)e).TailCall = true;
        }
        else if (e is ConditionalExpression)
        {
          ConditionalExpression ce = (ConditionalExpression)e;
          Statement truestmt = MakeTailCallReturn(allowtailcall, ce.IfTrue);
          Statement falsestmt = MakeTailCallReturn(allowtailcall, ce.IfFalse);

          return Ast.IfThenElse(ce.Test, truestmt, falsestmt);
        }
        else if (e is CommaExpression)
        {
          CommaExpression ce = (CommaExpression)e;
          if (ce.ValueIndex + 1 == ce.Expressions.Count)
          {
            List<Statement> ss = new List<Statement>();
            for (int i = 0; i < ce.Expressions.Count - 1; i++)
            {
              ss.Add(Ast.Statement(ce.Expressions[i]));
            }
            ss.Add(MakeTailCallReturn(allowtailcall, ce.Expressions[ce.Expressions.Count - 1]));
            return Ast.Block(ss);
          }
        }
      }

      return Ast.Return(e);
    }

    static Expression[] GetAstVector(object[] v, CodeBlock cb)
    {
      List<Expression> e = new List<Expression>();
      foreach (object var in v)
      {
        e.Add(GetAst(var, cb));
      }
      return e.ToArray();
    }

    protected static Expression[] GetAstList(Cons c, CodeBlock cb)
    {
      return GetAstList(c, cb, true);
    }

    protected static Expression[] GetAstListNoCast(Cons c, CodeBlock cb)
    {
      return GetAstList(c, cb, false);
    }


    protected static Expression[] GetAstList(Cons c, CodeBlock cb, bool castdown)
    {
      List<Expression> e = new List<Expression>();
      while (c != null)
      {
        if (c.cdr != null && !(c.cdr is Cons))
        {
          Builtins.SyntaxError("GetAstList", "improper list cant be used as an expression", c, false);
        }
        Expression ex = GetAst(c.car, cb);
        if (castdown && ex.Type.IsValueType)
        {
          ex = Ast.ConvertHelper(ex, typeof(object));
        }
        e.Add(ex);
        c = c.cdr as Cons;
      }
      return e.ToArray();
    }

    static Expression GetConsVector(object[] v, CodeBlock cb)
    {
      List<int> splices = new List<int>();
      List<Expression> e = new List<Expression>();
      foreach (object var in v)
      {
        Cons c = var as Cons;
        if (c != null && (bool)Builtins.IsEqual(c.car, unquote_splicing))
        {
          nestinglevel--;
          try
          {
            if (nestinglevel == 0)
            {
              Cons l = c.cdr as Cons;
              splices.Add(e.Count);
              Expression uqse = GetAst(l.car, cb);
              // its a cons, so it needs to become a vector
              uqse = Ast.SimpleCallHelper(Builtins_ListToVector, uqse);
              e.Add(uqse);
            }
            else
            {
              e.Add(GetCons(c, cb));
            }
          }
          finally
          {
            nestinglevel++;
          }
        }
        else
        {
          e.Add(GetCons(var, cb));
        }
      }
      for (int i = 0; i < e.Count; i++)
      {
        if (e[i].Type.IsValueType)
        {
          e[i] = Ast.ConvertHelper(e[i], typeof(object));
        }
      }
      if (splices.Count == 0)
      {
        return Ast.NewArray(typeof(object[]), e.ToArray());
      }
      else
      {
        for (int i = 0; i < e.Count; i++)
        {
          if (!splices.Contains(i))
          {
            e[i] = Ast.NewArray(typeof(object[]), e[i]);
          }
        }
        return Ast.ComplexCallHelper(Builtins_VectorAppend, e.ToArray());
      }
    }

    static Expression GetConsList(Cons c, CodeBlock cb)
    {
      List<int> splices = new List<int>();
      List<Expression> e = new List<Expression>();
      bool proper = true;

      while (c != null)
      {
        if (c.car is Cons && (bool)Builtins.IsEqual(Builtins.Car(Builtins.Car(c)), unquote_splicing))
        {
          nestinglevel--;
          try
          {
            if (nestinglevel == 0)
            {
              Cons l = Builtins.Cdr(Builtins.Car(c)) as Cons;
              splices.Add(e.Count);
              e.Add(GetAst(l.car, cb));
            }
            else
            {
              e.Add(GetCons(c.car, cb));
            }
          }
          finally
          {
            nestinglevel++;
          }
        }
        else
        {
          e.Add(GetCons(c.car, cb));
        }
        if (c.cdr != null && !(c.cdr is Cons))
        {
          e.Add(GetCons(c.cdr, cb));
          proper = false;
          break;
        }
        c = c.cdr as Cons;
        // check for possible unquote in dotted list
        // TODO: find out is unquotesplicing is valid
        if (c != null && (bool)Builtins.IsEqual(c.car, unquote))
        {
          nestinglevel--;
          try
          {
            if (nestinglevel == 0)
            {
              Cons l = Builtins.Second(c) as Cons;
              e.Add(GetAst(l, cb));
            }
            else
            {
              e.Add(GetCons(c.car, cb));
            }
          }
          finally
          {
            nestinglevel++;
          }

          proper = false;
          break;
        }
      }

      Expression r = null;

      if (splices.Count == 0)
      {
        Expression[] aa = e.ToArray();
        MethodInfo lm = MakeList(aa, proper);
        r = Ast.ComplexCallHelper(lm, aa);
      }
      else
      {
        for (int i = 0; i < e.Count; i++)
        {
          if (!splices.Contains(i))
          {
            e[i] = Ast.SimpleCallHelper(Builtins_Cons, e[i]);
          }
        }
        MethodInfo append = null;
        switch (e.Count)
        {
          case 0:
            append = Builtins_Append0;
            break;
          case 1:
            append = Builtins_Append1;
            break;
          case 2:
            append = Builtins_Append2;
            break;
          default:
            append = Builtins_AppendX;
            break;
        }
        r = Ast.ComplexCallHelper(append, e.ToArray());
        if (!proper)
        {
          r = Ast.SimpleCallHelper(Builtins_ToImproper, r);
        }
      }

      return r;
    }

    protected static string GetFullname(SymbolId id, CodeBlock parent)
    {
      if (parent == null || parent.IsGlobal)
      {
        return SymbolTable.IdToString(id);
      }
      else
      {
        return parent.Name + "#" + SymbolTable.IdToString(id);
      }
    }


    protected static bool AssignParameters(CodeBlock cb, object arg)
    {
      bool isrest = false;
      Cons cargs = arg as Cons;
      if (cargs != null)
      {
        while (cargs != null)
        {
          SymbolId an = (SymbolId)Builtins.First(cargs);
          CreateParameter(an, cb, typeof(object));

          Cons r = cargs.cdr as Cons;

          if (r == null && cargs.cdr != null)
          {
            SymbolId ta = (SymbolId)cargs.cdr;
            CreateParameter(ta, cb, typeof(object));
            isrest = true;
            break;
          }
          else
          {
            cargs = r;
          }
        }
      }
      else if (arg != null) // empty 
      {
        SymbolId an = (SymbolId)arg;
        isrest = true;
        CreateParameter(an, cb, typeof(object));
      }
      return isrest;
    }

    static Type[] GetExpressionTypes(Expression[] expr)
    {
      return Array.ConvertAll<Expression, Type>(expr, delegate(Expression e) { return e.Type; });
    }

    static SymbolId namehint = SymbolId.Invalid;

    protected static SymbolId NameHint
    {
      get
      {
        if (namehint == SymbolId.Invalid)
        {
          return Anonymous;
        }
        return namehint;
      }
      set { namehint = value; }
    }
  }
}
