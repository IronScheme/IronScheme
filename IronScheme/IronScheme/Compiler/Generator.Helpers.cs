#region License
/* Copyright (c) 2007,2008,2009,2010,2011 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Collections.Generic;
using System.Reflection;
using IronScheme.Hosting;
using IronScheme.Runtime;
using Microsoft.Scripting;
using Microsoft.Scripting.Actions;
using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Hosting;

namespace IronScheme.Compiler
{
  public abstract class BaseHelper 
#if USE_GLUE
    : Glue
#endif
  {
    static IronSchemeScriptEngine se;
    internal static CodeContext cc;
    internal static ScriptModule scriptmodule;
    internal static ActionBinder binder;
    static LanguageProvider lp;

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

    }


    internal static void Initialize(IronSchemeLanguageProvider ironSchemeLanguageProvider)
    {
      lp = ironSchemeLanguageProvider;
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
      object s = SymbolTable.StringToObject("call-with-values");
      BuiltinMethod cwv = new BuiltinMethod(s.ToString(), GetMethods(typeof(OptimizedBuiltins), "CallWithValues"));
      cc.Scope.SetName((SymbolId)s, cwv);
#else

      Callable values = Closure.MakeVarArgX(null, (CallTarget2)OptimizedBuiltins.Values, 2);
      cc.Scope.SetName(SymbolTable.StringToObject("values"), values);

      Callable cpsvoid = Closure.Make(null, (CallTarget0) Builtins.Void);
      cc.Scope.SetName(SymbolTable.StringToObject("cps-void"), cpsvoid);

      Callable cwv = Closure.Make(null, (CallTarget3)OptimizedBuiltins.CallWithValues);
      cc.Scope.SetName(SymbolTable.StringToObject("call-with-values"), cwv);

      Callable dw = Closure.Make(null, (CallTarget4)OptimizedBuiltins.DynamicWind);
      cc.Scope.SetName(SymbolTable.StringToObject("dynamic-wind"), dw);

      Callable cwcc = Closure.Make(null, (CallTarget2)OptimizedBuiltins.CallWithCurrentContinuation);
      cc.Scope.SetName(SymbolTable.StringToObject("call-with-current-continuation"), cwcc);
      cc.Scope.SetName(SymbolTable.StringToObject("call/cc"), cwcc);

      Callable id4cps = Closure.Make(null, (CallTargetN)Builtins.Values);
      cc.Scope.SetName(SymbolTable.StringToObject("identity-for-cps"), id4cps);

      Closure.IdentityForCPS = id4cps;

      cc.Scope.SetName(SymbolTable.StringToObject("letrec-identity"), Closure.Make(null, (CallTarget1) Builtins.LetrecIdentity));
      cc.Scope.SetName(SymbolTable.StringToObject("letrec*-identity"), Closure.Make(null, (CallTarget1) Builtins.LetrecStarIdentity));
      cc.Scope.SetName(SymbolTable.StringToObject("library-letrec*-identity"), Closure.Make(null, (CallTarget1) Builtins.LibraryLetrecIdentity));


#endif
      
      RuntimeHelpers.Assert = Builtins.AssertionViolation;
      Closure.AssertionViolation = Builtins.AssertionViolation;
      Closure.Cons = Builtins.Cons;
      MethodCallExpression.MakeList = MakeList;

      AddBuiltins(Context, typeof(Builtins));
      AddInlineEmitters(typeof(BuiltinEmitters));
      AddInlineEmitters(typeof(BuiltinEmitters.Unsafe));

      AddBuiltins(Context, typeof(Runtime.R6RS.Records));
      AddBuiltins(Context, typeof(Runtime.R6RS.Hashtables));
      AddBuiltins(Context, typeof(Runtime.R6RS.IO));
      AddBuiltins(Context, typeof(Runtime.R6RS.Exceptions));
      AddBuiltins(Context, typeof(Runtime.R6RS.Conditions));

      cc.Scope.SetName((SymbolId)SymbolTable.StringToObject("uninitialized"), Uninitialized.Instance);

#if CPS
      
      cc.Scope.SetName(SymbolTable.StringToObject("apply"), Closure.MakeVarArgX(null, (CallTarget4) OptimizedBuiltins.Apply, 4));

      OptimizedBuiltins.SymbolValue = Builtins.SymbolValue;

#else
      Closure.IdentityForCPS = Runtime.Builtins.SymbolValue(SymbolTable.StringToObject("values")) as BuiltinMethod;
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
      Dictionary<string, bool> foldable = new Dictionary<string, bool>();

      foreach (MethodInfo mi in builtinstype.GetMethods(BindingFlags.DeclaredOnly | BindingFlags.Public | BindingFlags.Static))
      {
        ObsoleteAttribute obs = Attribute.GetCustomAttribute(mi, typeof(ObsoleteAttribute)) as ObsoleteAttribute;
        if (obs != null && obs.IsError)
        {
          continue;
        }

        foreach (BuiltinAttribute ba in mi.GetCustomAttributes(typeof(BuiltinAttribute), false))
        {
          if (CheckParams(mi))
          {
            string name = ba.Name ?? mi.Name.ToLower();
            List<MethodBase> meths;

            foldable[name] = ba.AllowConstantFold;

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
        object s = SymbolTable.StringToObject(mn);
#if CPS
        cc.Scope.SetName(s, OptimizedBuiltins.MakeCPSCallable(new BuiltinMethod(mn, all[mn].ToArray())));
#else
        cc.Scope.SetName((SymbolId)s, new BuiltinMethod(mn, all[mn].ToArray(), foldable[mn]));
#endif
      }

      foreach (string mn in cpsfree.Keys)
      {
        object s = SymbolTable.StringToObject(mn);
        cc.Scope.SetName((SymbolId)s, new BuiltinMethod(mn, cpsfree[mn].ToArray()));
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
      Variable v = MakeVar(cb, sname, type);
      return v;
    }


    static Variable CreateParameter(SymbolId sname, CodeBlock cb, Type type)
    {
      Variable v = Variable.Parameter(cb, sname, type);
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

    readonly static object list = SymbolTable.StringToObject("list-prim");
    readonly static object liststar = SymbolTable.StringToObject("list*");

    protected internal static MethodInfo MakeList(Expression[] args, bool proper)
    {
      Type[] types = Array.ConvertAll<Expression, Type>(args,
        delegate(Expression e) { return e.Type.IsArray ? e.Type.GetElementType() : e.Type; });

      MethodBinder listbinder = ((BuiltinMethod) Context.Scope.LookupName(((SymbolId) (proper ? list : liststar)))).Binder;

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

    protected static Expression MakeTypedCaseClosure(string name, List<CodeBlockDescriptor> cases)
    {
      List<Expression> targets = new List<Expression>();
      List<Expression> arities = new List<Expression>();

      foreach (CodeBlockDescriptor c in cases)
      {
        targets.Add(c.callable);
        arities.Add(Ast.Constant(c.arity));
      }

      return Ast.SimpleCallHelper(Closure_MakeTypedCase, Ast.CodeContext(),
        Ast.NewArrayHelper(typeof(Callable[]), targets), Ast.NewArrayHelper(typeof(int[]), arities));
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
      
      return Ast.SimpleCallHelper(Closure_MakeCase,
        Ast.NewArrayHelper(typeof(Delegate[]), targets), Ast.NewArrayHelper(typeof(int[]), arities));
    }

    protected static Expression MakeClosure(CodeBlock cb, bool varargs)
    {
      return MakeClosure(cb, varargs, false);
    }

    protected static Expression MakeClosure(CodeBlock cb, bool varargs, bool typed)
    {
      if (varargs)
      {
        return Ast.SimpleCallHelper(Closure_MakeVarArgsX, Ast.CodeBlockExpression(cb, false, typed),
          Ast.Constant(cb.ParameterCount));
      }
      else
      {
        return Ast.SimpleCallHelper(Closure_Make, Ast.CodeBlockExpression(cb, false, typed), Ast.Constant(cb.ParameterCount));
      }
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
          if (e.Type != cb.ReturnType)
          {
            Expression ee = e;
            while (ee is UnaryExpression)
            {
              var ue = ee as UnaryExpression;
              if (ue.NodeType != AstNodeType.Convert)
              {
                break;
              }
              if (ue.Operand.Type == cb.ReturnType)
              {
                e = ue.Operand;
                break;
              }
              ee = ue.Operand;
            }
            if (!(e is VoidExpression))
            {
              e = Ast.ConvertHelper(e, cb.ReturnType);
            }
          }
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

      if (cb.Parent == null || (cb.Parent.IsGlobal && cb.ParameterCount < 9))
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

    // interesting flow control... :S
    static Expression OptimizeExpression(Expression e, List<Statement> stmts)
    {
      if (e is ConstantExpression)
      {
        return null;
      }

      if (e is BoundExpression)
      {
        // fails for (begin wtf 1) where wtf is undefined
        //return null;
      }

      if (e is MethodCallExpression)
      {
        //remove methods without side effects
      }

      if (IsUnspecified(e) && !ScriptDomainManager.Options.LightweightDebugging)
      {
        return null;
      }

      if (e is ConditionalExpression)
      {
        ConditionalExpression ce = e as ConditionalExpression;

        if (IsUnspecified(ce.IfFalse))
        {
          stmts.Add(Ast.If(ce.Test, OptimizeBody(Ast.Statement(ce.IfTrue))));
        }
        else
        {
          stmts.Add(Ast.If(ce.Test, OptimizeBody(Ast.Statement(ce.IfTrue))).Else(OptimizeBody(Ast.Statement(ce.IfFalse))));
        }

        return null;
      }

      return e;
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
            e = OptimizeExpression(e, newstmts);

            if (e == null)
            {
              continue;
            }
          }
          if (s != null)
          {
            newstmts.Add(OptimizeBody(s));
          }
        }

        newstmts.Add(OptimizeBody(bs.Statements[i]));

        cbbody = FlattenStatements(newstmts);
      }

      // NB!!! NEVER OPTIMIZE THE TAIL CALL WITH THIS
      return cbbody;
    }

    static Statement MakeTailCallReturn(bool allowtailcall, Expression e)
    {
      if (allowtailcall)
      {
        if (e is MethodCallExpression)
        {
          MethodCallExpression mce = ((MethodCallExpression)e);
          mce.TailCall = !mce.Method.ReturnType.IsValueType;
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
        else if (e is VoidExpression)
        {
          return Ast.Return(Ast.Comma(e, Ast.ReadField(null, Unspecified)));
        }
        else
        {

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
        if (id.IsEmpty)
        {
          return "anon";
        }
        return SymbolTable.IdToString(id);
      }
      else
      {
        while (parent.Inlined)
        {
          parent = parent.Parent;
        }
        if (parent.IsGlobal)
        {
          if (id.IsEmpty)
          {
            return "anon";
          }
          return SymbolTable.IdToString(id);
        }
        if (id.IsEmpty)
        {
          return parent.Name + "#" + "anon";
        }

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

    protected static bool AssignParameters(CodeBlock cb, object arg, object types)
    {
      bool isrest = false;
      Cons cargs = arg as Cons;
      Cons ctypes = types as Cons;
      if (cargs != null)
      {
        while (cargs != null)
        {
          SymbolId an = (SymbolId)Builtins.First(cargs);
          SymbolId type = (SymbolId)Builtins.First(ctypes);

          Type clrtype = ClrGenerator.ExtractTypeInfo(Builtins.List(quote, type));

          CreateParameter(an, cb, clrtype);

          Cons r = cargs.cdr as Cons;
          Cons rt = ctypes.cdr as Cons;

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
            ctypes = rt;
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

    internal static SymbolId VarHint { get; set; }
    internal static SymbolId VarHint2 { get; set; }

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
