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
    readonly internal static CodeContext cc;
    readonly internal static ScriptModule scriptmodule;
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

      scriptmodule = ScriptDomainManager.CurrentManager.Host.DefaultModule as ScriptModule;

      ModuleContext mc = new ModuleContext(scriptmodule);

      mc.CompilerContext = new CompilerContext(SourceUnit.CreateSnippet(se, ""));

      cc = new CodeContext(scriptmodule.Scope, se.GetLanguageContext(), mc);

      binder = new IronScheme.Actions.IronSchemeActionBinder(cc);

      Generator.initme = true;
    }
  }

  partial class Generator : BaseHelper
  {
    protected static MethodBase[] GetMethods(Type t, string name)
    {
      MemberInfo[] m = t.GetMember(name, BindingFlags.Static | BindingFlags.Public);
      MethodBase[] mb = new MethodBase[m.Length];
      Array.Copy(m, mb, m.Length);

      return mb;
    }

    internal static bool initme;


    static void Initialize()
    {
      // builtin methods
      AddGenerators(Context, typeof(Generator).Assembly);
#if !CPS
      // HACK: clean up needed
      SymbolId s = SymbolTable.StringToId("call-with-values");
      BuiltinMethod cwv = new BuiltinMethod(s.ToString(), GetMethods(typeof(OptimizedBuiltins), "CallWithValues"));
      cc.Scope.SetName(s, cwv);
#else

      ICallable values = Closure.MakeVarArgX(null, (CallTarget2)OptimizedBuiltins.Values, 2);
      cc.Scope.SetName(SymbolTable.StringToId("values"), values);

      ICallable cpsvoid = Closure.Make(null, (CallTarget0) Builtins.Void);
      cc.Scope.SetName(SymbolTable.StringToId("cps-void"), cpsvoid);

      ICallable cwv = Closure.Make(null, (CallTarget3)OptimizedBuiltins.CallWithValues);
      cc.Scope.SetName(SymbolTable.StringToId("call-with-values"), cwv);

      ICallable dw = Closure.Make(null, (CallTarget4)OptimizedBuiltins.DynamicWind);
      cc.Scope.SetName(SymbolTable.StringToId("dynamic-wind"), dw);

      ICallable cwcc = Closure.Make(null, (CallTarget2)OptimizedBuiltins.CallWithCurrentContinuation);
      cc.Scope.SetName(SymbolTable.StringToId("call-with-current-continuation"), cwcc);
      cc.Scope.SetName(SymbolTable.StringToId("call/cc"), cwcc);

      ICallable id4cps = Closure.Make(null, (CallTargetN)Builtins.Values);
      cc.Scope.SetName(SymbolTable.StringToId("identity-for-cps"), id4cps);

      Closure.IdentityForCPS = id4cps;

      cc.Scope.SetName(SymbolTable.StringToId("letrec-identity"), Closure.Make(null, (CallTarget1) Builtins.LetrecIdentity));
      cc.Scope.SetName(SymbolTable.StringToId("letrec*-identity"), Closure.Make(null, (CallTarget1) Builtins.LetrecStarIdentity));
      cc.Scope.SetName(SymbolTable.StringToId("library-letrec*-identity"), Closure.Make(null, (CallTarget1) Builtins.LibraryLetrecIdentity));


#endif
      
      RuntimeHelpers.Assert = Builtins.AssertionViolation;
      Closure.AssertionViolation = Builtins.AssertionViolation;
      Closure.Cons = Builtins.Cons;

      AddBuiltins(Context, typeof(Builtins));
      AddInlineEmitters(typeof(BuiltinEmitters));

      AddBuiltins(Context, typeof(Runtime.psyntax.AnnotatedReader));
      AddBuiltins(Context, typeof(Runtime.psyntax.Serialization));

      AddBuiltins(Context, typeof(Runtime.R6RS.Records));
      AddBuiltins(Context, typeof(Runtime.R6RS.Hashtables));
      AddBuiltins(Context, typeof(Runtime.R6RS.Unicode));
      AddBuiltins(Context, typeof(Runtime.R6RS.ByteVectors));
      AddBuiltins(Context, typeof(Runtime.R6RS.Sorting));
      AddBuiltins(Context, typeof(Runtime.R6RS.IO));
      AddBuiltins(Context, typeof(Runtime.R6RS.Exceptions));
      AddBuiltins(Context, typeof(Runtime.R6RS.Conditions));
      AddBuiltins(Context, typeof(Runtime.R6RS.Programs));

      AddBuiltins(Context, typeof(Runtime.R6RS.Arithmetic.Flonums));
      AddBuiltins(Context, typeof(Runtime.R6RS.Arithmetic.Fixnums));
      AddBuiltins(Context, typeof(Runtime.R6RS.Arithmetic.Bitwise));

      AddInlineEmitters(typeof(Runtime.R6RS.Arithmetic.FlonumsInlineEmitters));
      AddInlineEmitters(typeof(Runtime.R6RS.Arithmetic.FixnumsInlineEmitters));

      cc.Scope.SetName(SymbolTable.StringToId("uninitialized"), Uninitialized.Instance);

#if CPS
      
      cc.Scope.SetName(SymbolTable.StringToId("apply"), Closure.MakeVarArgX(null, (CallTarget4) OptimizedBuiltins.Apply, 4));

      OptimizedBuiltins.SymbolValue = Builtins.SymbolValue;

#else
      Closure.IdentityForCPS = Runtime.Builtins.SymbolValue(SymbolTable.StringToId("values")) as BuiltinMethod;
#endif
      

    }

    static bool CheckParams(MethodInfo mi)
    {
#if DEBUG
      return mi.ReturnType == typeof(object)
        && Array.TrueForAll(mi.GetParameters(), 
        pi => (pi.Position == 0 && pi.ParameterType == typeof(CodeContext)) || 
          pi.ParameterType == typeof(object) ||
          (pi.ParameterType == typeof(object[]) && pi.IsDefined(typeof(ParamArrayAttribute), false)));
#else
      return true;
#endif
    }

    public static void AddBuiltins(CodeContext cc, Type builtinstype)
    {
      BuiltinMethod.binder = binder;
      BuiltinMethod.context = cc;

      Dictionary<string, List<MethodBase>> all = new Dictionary<string, List<MethodBase>>();
      Dictionary<string, List<MethodBase>> cpsfree = new Dictionary<string, List<MethodBase>>();

      foreach (MethodInfo mi in builtinstype.GetMethods(BindingFlags.DeclaredOnly | BindingFlags.Public | BindingFlags.Static))
      {
        foreach (BuiltinAttribute ba in mi.GetCustomAttributes(typeof(BuiltinAttribute), false))
        {
          if (CheckParams(mi))
          {
            string name = ba.Name ?? mi.Name.ToLower();
            List<MethodBase> meths;

            if (ba.AllowCPS)
            {
              if (!all.TryGetValue(name, out meths))
              {
                all[name] = meths = new List<MethodBase>();
              }
              meths.Add(mi);
            }
            else
            {
              if (!cpsfree.TryGetValue(name, out meths))
              {
                cpsfree[name] = meths = new List<MethodBase>();
              }
              meths.Add(mi);
            }
          }
          else
          {
            throw new NotSupportedException("all arguments must be of type object, method: " + mi);
          }
        }
      }

      foreach (string mn in all.Keys)
      {
        SymbolId s = SymbolTable.StringToId(mn);
#if CPS
        cc.Scope.SetName(s, OptimizedBuiltins.MakeCPSCallable(new BuiltinMethod(mn, all[mn].ToArray())));
#else
        cc.Scope.SetName(s,  new BuiltinMethod(mn, all[mn].ToArray()));
#endif
      }

      foreach (string mn in cpsfree.Keys)
      {
        SymbolId s = SymbolTable.StringToId(mn);
        cc.Scope.SetName(s, new BuiltinMethod(mn, cpsfree[mn].ToArray()));
      }

    }

    protected static CodeBlock GetTopLevel(CodeBlock cb)
    {
      while (cb.Parent != null)
      {
        cb = cb.Parent;
      }
      return cb;
    }

    static Expression Read(SymbolId name, CodeBlock cb, Type type)
    {
      SymbolId sname = name;

      Variable v = cb.Lookup(sname);

      if (v == null)
      {
        if (assigns.ContainsKey(sname))
        {
          return Ast.Read(sname);
        }
        else
        {
          CodeBlock tl = GetTopLevel(cb);
          v = tl.CreateVariable(sname, Variable.VariableKind.Global, typeof(object), Ast.Read(sname));
          return Ast.Read(v);
        }
      }

      return Ast.ReadDefined(v);
    }

    protected static Variable Create(SymbolId sname, CodeBlock cb, Type type)
    {
      Variable v = MakeVar(cb, sname, typeof(object));
      return v;
    }


    static Variable CreateParameter(SymbolId sname, CodeBlock cb, Type type)
    {
      Variable v = Variable.Parameter(cb, sname, typeof(object));
      cb.AddParameter(v);
      return v;
    }

    static Variable MakeVar(CodeBlock cb, SymbolId name, Type type)
    {
      //using globals instead of locals does improve performance a lot, compiling is easier too
      //unfortunately they dont play well
      //should really investigate better closure structure
      return cb.CreateVariable(name, cb.IsGlobal ? Variable.VariableKind.Global : Variable.VariableKind.Local
        , type ?? typeof(object));
    }

    readonly static SymbolId list = SymbolTable.StringToId("list-prim");
    readonly static SymbolId liststar = SymbolTable.StringToId("list*");

    protected static MethodInfo MakeList(Expression[] args, bool proper)
    {
      Type[] types = Array.ConvertAll<Expression, Type>(args,
        delegate(Expression e) { return e.Type.IsArray ? e.Type.GetElementType() : e.Type; });

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
          Ast.Constant(cb.ParameterCount));
      }
      else
      {
        return Ast.SimpleCallHelper(Closure_Make, Ast.CodeContext(), Ast.CodeBlockExpression(cb, false, false));
      }
    }

    protected internal class CodeBlockDescriptor
    {
      public int arity;
      public CodeBlockExpression codeblock;
      public bool varargs;
    }

    protected internal static void FillBody(CodeBlock cb, List<Statement> stmts, Cons body, bool allowtailcall)
    {
      Cons c = body;
      while (c != null)
      {
        Expression e = GetAst(c.car, cb, c.cdr == null);
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
      cb.Body = OptimizeBody(cb.Body);

      if (cb.Parent == null || (cb.Parent.IsGlobal && cb.ParameterCount < 6))
      {
        cb.ExplicitCodeContextExpression = Ast.CodeContext();
      }
    }

    static bool IsUnspecified(Expression e)
    {
      if (e is MemberExpression)
      {
        if (((MemberExpression)e).Member == Unspecified)
        {
          return true;
        }
      }
      return false;
    }



    static Statement OptimizeBody(Statement cbbody)
    {
      if (cbbody is BlockStatement)
      {
        BlockStatement bs = cbbody as BlockStatement;

        List<Statement> newstmts = new List<Statement>();
        
        int i = 0;
        
        for (; i < bs.Statements.Count - 1; i++)
        {
          Statement s = bs.Statements[i];
          if (s is ExpressionStatement)
          {
            Expression e = (s as ExpressionStatement).Expression;

            if (e is ConstantExpression)
            {
              continue;
            }

            if (e is BoundExpression)
            {
              continue;
            }

            if (e is MethodCallExpression)
            {
              //remove methods without side effects
            }

            if (IsUnspecified(e))
            {
              continue;
            }

            if (e is ConditionalExpression)
            {
              ConditionalExpression ce = e as ConditionalExpression;

              if (IsUnspecified(ce.IfFalse))
              {
                newstmts.Add(Ast.If(ce.Test, Ast.Statement(ce.IfTrue)));
              }
              else
              {
                newstmts.Add(Ast.If(ce.Test, Ast.Statement(ce.IfTrue)).Else(Ast.Statement(ce.IfFalse)));
              }

              continue;
            }

          }

          newstmts.Add(OptimizeBody(s));
          
        }

        newstmts.Add(OptimizeBody(bs.Statements[i]));

        if (newstmts.Count == 1)
        {
          cbbody = newstmts[0];
        }
        else
        {
          cbbody = Ast.Block(newstmts);
        }
      }
      return cbbody;
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
          Statement truestmt = OptimizeBody(MakeTailCallReturn(allowtailcall, ce.IfTrue));
          Statement falsestmt = OptimizeBody(MakeTailCallReturn(allowtailcall, ce.IfFalse));

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
            return OptimizeBody(Ast.Block(ss));
          }
        }
        else if (e is NewExpression || e is BoundExpression || e is ConstantExpression || e is MemberExpression)
        {
          // ignore
        }
        else if (e is UnaryExpression)
        {
          ;
        }
        else
        {
          ;
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

    static bool iscontinuation = false;

    protected static Expression[] GetAstList(Cons c, CodeBlock cb, bool castdown)
    {
      List<Expression> e = new List<Expression>();
      while (c != null)
      {
        if (c.cdr != null && !(c.cdr is Cons))
        {
          Builtins.SyntaxError("GetAstList", "improper list cant be used as an expression", c, false);
        }
        iscontinuation = e.Count == 0;
        Expression ex = GetAst(c.car, cb);
        iscontinuation = false;

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
      List<Expression> e = new List<Expression>();

      foreach (object var in v)
      {
        Cons c = var as Cons;
        e.Add(GetCons(var, cb));
      }

      for (int i = 0; i < e.Count; i++)
      {
        if (e[i].Type.IsValueType)
        {
          e[i] = Ast.ConvertHelper(e[i], typeof(object));
        }
      }

      return Ast.NewArray(typeof(object[]), e.ToArray());
    }

    public static Expression GetConsList(Cons c, CodeBlock cb)
    {
      List<int> splices = new List<int>();
      List<Expression> e = new List<Expression>();
      bool proper = true;

      while (c != null)
      {
        e.Add(GetCons(c.car, cb));

        if (c.cdr != null && !(c.cdr is Cons))
        {
          e.Add(GetCons(c.cdr, cb));
          proper = false;
          break;
        }
        c = c.cdr as Cons;
      }

      Expression r = null;

      Expression[] aa = e.ToArray();
      MethodInfo lm = MakeList(aa, proper);
      r = Ast.ComplexCallHelper(lm, aa);

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

    //[ThreadStatic]
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
