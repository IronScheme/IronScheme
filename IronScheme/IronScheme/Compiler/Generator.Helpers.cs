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


    readonly static Dictionary<SymbolId, BuiltinMethod> builtinmap =
      new Dictionary<SymbolId, BuiltinMethod>();

    internal static Dictionary<SymbolId, BuiltinMethod> MethodGroups
    {
      get { return Generator.builtinmap; }
    }

    readonly static Dictionary<CodeBlock, Scope> scopemap = new Dictionary<CodeBlock, Scope>();

    internal static CodeBlock evalblock;

    static void Initialize()
    {
      // builtin methods
      AddGenerators(Context, typeof(Generator).Assembly);
      // HACK: clean up needed
      SymbolId s = SymbolTable.StringToId("call-with-values");
      cc.Scope.SetName(s, builtinmap[s] = new BuiltinMethod(s.ToString(), ReflectionCache.GetMethodGroup(typeof(OptimizedBuiltins), "CallWithValues")));


      AddBuiltins(Context, typeof(Builtins));
    }

    public static void AddBuiltins(CodeContext cc, Type builtinstype)
    {
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
        cc.Scope.SetName(s, builtinmap[s] = new BuiltinMethod(mn, ReflectionCache.GetMethodGroup(mn, all[mn].ToArray())));
      }
    }

    protected static CodeBlock GetTopLevel(CodeBlock cb)
    {
      while (cb.Parent != null)
      {
        cb = cb.Parent;
      }
      if (cb.IsGlobal)
      {
        return cb;
      }
      else
      {
        return null;
      }
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
        return Ast.DynamicConvert(Ast.Read(v), t);
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

    static Variable MakeVar(CodeBlock cb, SymbolId name, Type type)
    {
      return cb.CreateVariable(name, cb.IsGlobal ? Variable.VariableKind.Global : Variable.VariableKind.Local, type ?? typeof(object));
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
        if (v.Name.CaseInsensitiveEquals(name))
        {
          return v;
        }
      }

      foreach (Variable v in cb.Parameters)
      {
        if (v.Name.CaseInsensitiveEquals(name))
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


    static MethodInfo MakeList(Expression[] args, bool proper)
    {
      Type[] types = Array.ConvertAll<Expression, Type>(args,
        delegate(Expression e) { return e.Type; });

      MethodBinder listbinder = builtinmap[proper ? list : liststar].Binder;

      return listbinder.MakeBindingTarget(CallType.None, types).Target.Method as MethodInfo;
    }

    protected static string GetLambdaName(CodeBlock cb)
    {
      string fn = GetFullname(NameHint, cb);

      string[] tokens = fn.Split('.');

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
      return string.Join(".", tokens);
    }

    protected static Expression MakeClosure(CodeBlock cb, bool varargs)
    {
      if (varargs)
      {
        return Ast.SimpleCallHelper(Closure_MakeVarArgsX, Ast.CodeContext(), Ast.CodeBlockExpression(cb, false, false),
          Ast.Constant(cb.Parameters.Count), Ast.Constant(cb.Name));
      }
      else
      {
        return Ast.SimpleCallHelper(Closure_Make, Ast.CodeContext(), Ast.CodeBlockExpression(cb, false, false), Ast.Constant(cb.Name));
      }
    }

    static bool canallowtailcall = false;

    internal static bool CanAllowTailCall
    {
      get { return Generator.canallowtailcall; }
      set { Generator.canallowtailcall = value; }
    }


    internal static void InitGlobal(Cons defcheck, CodeBlock cb, List<Statement> stmts)
    {
      while (defcheck != null)
      {
        Cons h = defcheck.Car as Cons;

        defcheck.Car = SyntaxExpander.Expand1(defcheck.Car);

        h = defcheck.Car as Cons;

        if (h != null && (bool)Builtins.IsEqual(h.Car, define))
        {
          Variable v = Create((SymbolId)Builtins.Second(h), cb, typeof(object));
          stmts.Add(Ast.Write(v, Ast.ReadField(null, Unspecified)));
          h.Car = set;
        }

        defcheck = defcheck.Cdr as Cons;
      }
    }

    protected static void FillBody(CodeBlock cb, List<Statement> stmts, Cons body, bool allowtailcall)
    {
      // declare all define at start of body, then change the define to 'set!'
      // similar to letrec* behaviour; also expand1 the defines
      Cons defcheck = body;
      while (defcheck != null)
      {
        Cons h = defcheck.Car as Cons;

        if (h == null)
        {
          break;
        }

        defcheck.Car = h = SyntaxExpander.Expand1(defcheck.Car) as Cons;

        if (h != null && (bool)Builtins.IsEqual(h.Car, define))
        {
          Variable v = Create((SymbolId)Builtins.Second(h), cb, typeof(object));
          stmts.Add(Ast.Write(v, Ast.ReadField(null, Unspecified)));
          h.Car = set;
        }
        else
        {
          break;
        }

        defcheck = defcheck.Cdr as Cons;
      }
      Cons c = body;
      while (c != null)
      {
        Expression e = GetAst(c.Car, cb);
        Statement s = null;
        if (c.Cdr == null)
        {
          MakeTailCall(allowtailcall, e);

          s = Ast.Return(e);
        }
        else
        {
          s = Ast.Statement(e);
        }

        if (c.Car is Cons && Parser.sourcemap.ContainsKey(c.Car as Cons))
        {
          s.SetLoc(Parser.sourcemap[c.Car as Cons]);
        }
        else
        {
          ;
        }

        stmts.Add(s);
        c = c.Cdr as Cons;
      }

      if (stmts.Count == 0)
      {
        stmts.Add(Ast.Return(Ast.ReadField(null, Unspecified)));
      }



      cb.Body = Ast.Block(stmts.ToArray());
    }

    static void MakeTailCall(bool allowtailcall, Expression e)
    {
      if (canallowtailcall && allowtailcall)
      {
        if (e.Type != typeof(void))
        {
          if (e is MethodCallExpression)
          {
            ((MethodCallExpression)e).TailCall = true;
          }
          else if (e is ConditionalExpression)
          {
            ConditionalExpression ce = (ConditionalExpression)e;
            //MakeTailCall(allowtailcall, ce.IfTrue);
            MakeTailCall(allowtailcall, ce.IfFalse);
          }
        }
        
      }
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
      List<Expression> e = new List<Expression>();
      while (c != null)
      {
        if (c.Cdr != null && !(c.Cdr is Cons))
        {
          throw new NotSupportedException("improper list cant be used as an expression");
        }
        Expression ex = GetAst(c.Car, cb);
        if (ex.Type.IsValueType)
        {
          ex = Ast.DynamicConvert( ex, typeof(object));
        }
        e.Add(ex);
        c = c.Cdr as Cons;
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
        if (c != null && (bool)Builtins.IsEqual(c.Car, unquote_splicing))
        {
          nestinglevel--;
          try
          {
            if (nestinglevel == 0)
            {
              Cons l = c.Cdr as Cons;
              splices.Add(e.Count);
              Expression uqse = GetAst(l.Car, cb);
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
          e[i] = Ast.DynamicConvert(e[i], typeof(object));
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
        if (c.Car is Cons && (bool)Builtins.IsEqual(Builtins.Caar(c), unquote_splicing))
        {
          nestinglevel--;
          try
          {
            if (nestinglevel == 0)
            {
              Cons l = Builtins.Cdar(c) as Cons;
              splices.Add(e.Count);
              e.Add(GetAst(l.Car, cb));
            }
            else
            {
              e.Add(GetCons(c.Car, cb));
            }
          }
          finally
          {
            nestinglevel++;
          }
        }
        else
        {
          e.Add(GetCons(c.Car, cb));
        }
        if (c.Cdr != null && !(c.Cdr is Cons))
        {
          e.Add(GetCons(c.Cdr, cb));
          proper = false;
          break;
        }
        c = c.Cdr as Cons;
        // check for possible unquote in dotted list
        // TODO: find out is unquotesplicing is valid
        if (c != null && (bool)Builtins.IsEqual(c.Car, unquote))
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
              e.Add(GetCons(c.Car, cb));
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
        r = Ast.ComplexCallHelper(Builtins_Append, e.ToArray());
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
        return parent.Name + "." + SymbolTable.IdToString(id);
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

          Cons r = cargs.Cdr as Cons;

          if (r == null && cargs.Cdr != null)
          {
            SymbolId ta = (SymbolId)cargs.Cdr;
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
