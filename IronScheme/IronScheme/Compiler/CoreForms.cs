using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting.Ast;
using IronScheme.Runtime;
using Microsoft.Scripting;
using IronScheme.Hosting;
using Microsoft.Scripting.Types;
using Microsoft.Scripting.Actions;
using System.Reflection;
using Microsoft.Scripting.Hosting;
using Microsoft.Scripting.Utils;

namespace IronScheme.Compiler
{
  static class Generator
  {
    #region Helpers

    delegate Expression GeneratorHandler(object args, CodeBlock cb);

    readonly static Dictionary<SymbolId, GeneratorHandler> generators = new Dictionary<SymbolId, GeneratorHandler>();

    readonly static Dictionary<SymbolId, BuiltinFunction> builtinmap =
      new Dictionary<SymbolId, BuiltinFunction>();

    public static Dictionary<SymbolId, BuiltinFunction> BuiltinFunctions
    {
      get { return Generator.builtinmap; }
    } 


    readonly static Dictionary<CodeBlock, Scope> scopemap = new Dictionary<CodeBlock, Scope>();

    static CodeBlock evalblock;

    static Generator()
    {
      Add("set!", Set);
      Add("quote", Quote);
      Add("lambda", Lambda);
      Add("if", If);
      Add("define", Define);
      Add("macro", Macro);
      Add("quasiquote", Quasiquote);
    

      IronSchemeScriptEngine se = ScriptDomainManager.CurrentManager.GetLanguageProvider(
          typeof(Hosting.IronSchemeLanguageProvider)).GetEngine() as IronSchemeScriptEngine;
      ModuleContext mc = new ModuleContext(ScriptDomainManager.CurrentManager.CreateModule("CompileTime"));
      mc.CompilerContext = new CompilerContext(SourceUnit.CreateSnippet(se, ""));


      CC = se.CreateContext(mc);

      // builtin methods

      Dictionary<string, List<MethodBase>> all = new Dictionary<string, List<MethodBase>>();

      foreach (MethodInfo mi in typeof(Builtins).GetMethods())
      {
        foreach (BuiltinAttribute ba in mi.GetCustomAttributes(typeof(BuiltinAttribute), false))
        {
          string mn = ba.Name ?? mi.Name.ToLower();
          List<MethodBase> meths;
          if (!all.TryGetValue(mn, out meths))
          {
            all[mn] = meths = new List<MethodBase>();
          }
          meths.Add(mi);
        }
      }

      foreach (string mn in all.Keys)
      {
        builtinmap[SymbolTable.StringToId(mn)] = BuiltinFunction.MakeMethod(mn, all[mn].ToArray(), FunctionType.Function);
      }
    }


    readonly static CodeContext CC;

    internal static CodeContext Compiler
    {
      get { return CC; }
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
        return Ast.Read(sname);
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

    static Variable Create(SymbolId sname, CodeBlock cb, Type type)
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

    static Variable GetParameter(SymbolId sname, CodeBlock cb)
    {
      foreach (Variable par in cb.Parameters)
      {
        if (par.Name.CaseInsensitiveEquals(sname))
        {
          return par;
        }
      }
      return null;
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

    static Type GetVariableType(SymbolId sname, CodeBlock cb)
    {
      Variable par = GetParameter(sname, cb);
      if (par != null)
      {
        return typeof(object);
      }

      Scope scope = GetScope(cb);

      object cvalue;
      if (scope.TryGetName(sname, out cvalue))
      {
        return cvalue as Type;
      }

      if (cb.Parent != null)
      {
        return GetVariableType(sname, cb.Parent);
      }

      return null;

    }

    static Variable MakeVar(CodeBlock cb, SymbolId name, Type type)
    {
      return cb.CreateVariable(name, cb.IsGlobal ? Variable.VariableKind.Global : Variable.VariableKind.Local, type ?? typeof(object));
    }

    static Variable FindOrMakeVar(CodeBlock cb, SymbolId name, Type type)
    {
      Variable v = FindVar(cb, name);
      return v ?? cb.CreateVariable(name, cb.IsGlobal ? Variable.VariableKind.Global : Variable.VariableKind.Local, type ?? typeof(object));
    }

    static Variable FindVar(CodeBlock cb, SymbolId name)
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

    static MethodBinder listbinder;

    static MethodInfo MakeList(params Expression[] args)
    {
      Type[] types = Array.ConvertAll<Expression, Type>(args,
        delegate(Expression e) { return e.Type; });
      if (listbinder == null)
      {
        BuiltinFunction l = builtinmap[SymbolTable.StringToId("list")];
        listbinder = MethodBinder.MakeBinder(BINDER, l.Name, l.Targets, BinderType.Normal);
      }

      return listbinder.MakeBindingTarget(CallType.None, types).Target.Method as MethodInfo;
    }

    static readonly ActionBinder BINDER = new IronScheme.Actions.IronSchemeActionBinder(null);

    static MethodBase[] GetMethods(Type type, string name)
    {
      List<MethodBase> meths = new List<MethodBase>();
      foreach (MemberInfo memi in type.GetMember(name, BindingFlags.Public | BindingFlags.Static |
        BindingFlags.FlattenHierarchy | BindingFlags.IgnoreCase))
      {
        if (memi is MethodInfo)
        {
          meths.Add(memi as MethodBase);
        }
      }

      return meths.ToArray();
    }

    static MethodInfo GetMethod(Type type, string name)
    {
      return type.GetMethod(name, BindingFlags.Static | BindingFlags.Public);
    }

    static readonly MethodInfo closure_make = typeof(Closure).GetMethod("Make");
    static readonly MethodInfo closure_varargs = typeof(Closure).GetMethod("MakeVarArgX");

    static Expression MakeClosure(CodeBlock cb, bool varargs)
    {
      return Ast.Call(null,varargs ? closure_varargs : closure_make
        , Ast.CodeContext(), Ast.CodeBlockExpression(cb, false, false), Ast.Constant(cb.Name));
    }
    

    static void FillBody(CodeBlock cb, List<Statement> stmts, Cons body, bool allowtailcall)
    {
      Cons c = body;
      while (c != null)
      {
        Expression e = GetAst(c.Car, cb);
        if (c.Cdr == null)
        {
          if (allowtailcall)
          {
            if (e is MethodCallExpression && e.Type != typeof(void))
            {
              ((MethodCallExpression)e).TailCall = true;
            }
            if (e is ActionExpression && e.Type != typeof(void))
            {
              ((ActionExpression)e).TailCall = true;
            }
          }

          stmts.Add(Ast.Return(e));
        }
        else
        {
          stmts.Add(Ast.Statement(e));
        }
        c = c.Cdr as Cons;
      }

      cb.Body = Ast.Block(stmts.ToArray());
    }


    static void Add(string name, GeneratorHandler handler)
    {
      generators.Add(SymbolTable.StringToId(name), handler);
    }


    static Expression[] GetAstList(Cons c, CodeBlock cb)
    {
      List<Expression> e = new List<Expression>();
      while (c != null)
      {
        if (c.Cdr != null && !(c.Cdr is Cons))
        {
          throw new NotSupportedException("improper list cant be used as an expression");
        }
        e.Add(GetAst(c.Car, cb));
        c = c.Cdr as Cons;
      }
      return e.ToArray();
    }

    static readonly MethodInfo list = typeof(Builtins).GetMethod("List", new Type[] { typeof(object[]) });
    static readonly MethodInfo cons1 = typeof(Builtins).GetMethod("Cons", new Type[] { typeof(object) });
    static readonly MethodInfo append = typeof(Builtins).GetMethod("Append");
    static readonly MethodInfo toimproper = typeof(Builtins).GetMethod("ToImproper");

    static Expression GetConsList(Cons c, CodeBlock cb)
    {
      List<int> splices = new List<int>();
      List<Expression> e = new List<Expression>();
      bool proper = true;
      while (c != null)
      {
        if (nestinglevel == 1 && c.Car is Cons && Builtins.IsEqual(Builtins.Caar(c), unquote_splicing))
        {
          Cons l = Builtins.Cdar(c) as Cons;
          splices.Add(e.Count);
          e.Add(GetAst(l.Car, cb));
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
      }

      Expression r = null;

      if (splices.Count == 0)
      {
        r = Ast.Call(null, list, e.ToArray());
      }
      else
      {
        for (int i = 0; i < e.Count; i++)
        {
          if (!splices.Contains(i))
          {
            e[i] = Ast.Call(null, cons1, e[i]);
          }
        }
        r = Ast.Call(null, append, e.ToArray());
      }
      if (!proper)
      {
        r = Ast.Call(null, toimproper, r);
      }
      return r;
    }

    static bool AssignParameters(CodeBlock cb, object arg)
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
      else
      {
        SymbolId an = (SymbolId)arg;
        isrest = true;
        CreateParameter(an, cb, typeof(object));
      }
      return isrest;
    }

    static readonly SymbolId quote = SymbolTable.StringToId("quote");
    static readonly SymbolId unquote_splicing = SymbolTable.StringToId("unquote-splicing");
    static readonly SymbolId quasiquote = SymbolTable.StringToId("quasiquote");
    static readonly SymbolId unquote = SymbolTable.StringToId("unquote");
    
    static SymbolId namehint = SymbolId.Invalid;

    public static SymbolId NameHint
    {
      get
      {
        if (namehint == SymbolId.Invalid)
        {
          return SymbolId.Empty;
        }
        return namehint;
      }
      set { namehint = value; }
    }

    #endregion
    
    public static Expression GetCons(object args, CodeBlock cb)
    {
      Cons c = args as Cons;
      if (c != null)
      {
        if (nestinglevel == 1 &&
            Builtins.IsSymbol(c.Car) &&
            Builtins.IsEqual(c.Car, unquote))
        {
          return GetAst(Builtins.Second(c), cb);
        }
        return GetConsList(c, cb);
      }
      else
      {
        return Ast.Constant(args);
      }

    }

    static Type[] GetExpressionTypes(Expression[] expr)
    {
      return Array.ConvertAll<Expression, Type>(expr, delegate(Expression e) { return e.Type; });
    }

    public static Expression GetAst(object args, CodeBlock cb)
    {
      Cons c = args as Cons;
      if (c != null)
      {
        object first = Builtins.First(args);
        if (Builtins.IsSymbol(first))
        {
          SymbolId f = (SymbolId)first;
          object m;

          if (Compiler.Scope.TryLookupName(f, out m))
          {
            Runtime.Macro macro = m as Runtime.Macro;
            if (macro != null)
            {
              object result = macro.Invoke(Compiler, c.Cdr);
              return GetAst(result, cb);
            }
          }

          GeneratorHandler gh;
          if (generators.TryGetValue(f, out gh))
          {
            return gh(c.Cdr, cb);
          }

          BuiltinFunction bf;
          if (builtinmap.TryGetValue(f, out bf))
          {
            MethodBinder mb = MethodBinder.MakeBinder(BINDER, SymbolTable.IdToString(f), bf.Targets, BinderType.Normal);
            Expression[] pars = GetAstList(c.Cdr as Cons, cb);
            Type[] types = GetExpressionTypes(pars);
            MethodCandidate mc = mb.MakeBindingTarget(CallType.None, types);
            if (mc == null)
            {
              throw new Exception("No match for " + f);
            }
            if (mc.Target.NeedsContext)
            {
              pars = ArrayUtils.Insert<Expression>(Ast.CodeContext(), pars);
            }
            return Ast.Call(null, mc.Target.Method as MethodInfo, pars);
          }
        }
        return Ast.Action.Call(typeof(object), GetAstList(c, cb));
      }
      else
      {
        if (Builtins.IsSymbol(args))
        {
          return Read((SymbolId)args, cb, typeof(object));
        }
        return Ast.Constant(args);
      }
    }

    // quote
    public static Expression Quote(object args, CodeBlock cb)
    {
      int t = nestinglevel;
      nestinglevel = int.MaxValue/2;
      try
      {
        return GetCons(Builtins.Car(args), cb);
      }
      finally
      {
        nestinglevel = t;
      }
    }

    // set!
    public static Expression Set(object args, CodeBlock cb)
    {
      SymbolId s = (SymbolId)Builtins.First(args);

      namehint = s;

      Expression value = GetAst(Builtins.Second(args), cb);

      Variable v = FindVar(cb, s);

      if (v == null)
      {
        throw new MissingMemberException(string.Format("name '{0}' not defined", SymbolTable.IdToString(s)));
      }

      if (value.Type.IsValueType)
      {
        value = Ast.DynamicConvert(value, typeof(object));
      }

      return Ast.Assign(v, value);
    }
    
    // define
    public static Expression Define(object args, CodeBlock cb)
    {
      SymbolId s = (SymbolId)Builtins.First(args);

      namehint = s;

      Expression value = GetAst(Builtins.Second(args), cb);

      Variable v = Create(s, cb, typeof(object));

      if (value.Type.IsValueType)
      {
        value = Ast.DynamicConvert(value, typeof(object));
      }
      return Ast.Assign(v, value);
    }

    readonly static MethodInfo macro_vararg = typeof(Runtime.Macro).GetMethod("MakeVarArgX");
    readonly static MethodInfo macro_make = typeof(Runtime.Macro).GetMethod("Make");

    // macro
    public static Expression Macro(object args, CodeBlock c)
    {
      CodeBlock cb = Ast.CodeBlock(NameHint);
      cb.Parent = c;

      object arg = Builtins.First(args);
      Cons body = Builtins.Cdr(args) as Cons;

      bool isrest = AssignParameters(cb, arg);

      List<Statement> stmts = new List<Statement>();
      FillBody(cb, stmts, body, true);

      CodeBlockExpression cbe = Ast.CodeBlockExpression(cb, false);

      Expression ex = Ast.Call(null, isrest ? macro_vararg : macro_make, Ast.CodeContext(), cbe, 
        Ast.Constant(cb.Parameters.Count), Ast.Constant(cb.Name));

      cb.BindClosures();

      Delegate md = cb.GetDelegateForInterpreter(Compiler, false) as Delegate;

      if (isrest)
      {
        Compiler.Scope.SetName(NameHint, Runtime.Macro.MakeVarArgX(Compiler, md, cb.Parameters.Count, cb.Name));
      }
      else
      {
        Compiler.Scope.SetName(NameHint, Runtime.Macro.Make(Compiler, md, cb.Parameters.Count, cb.Name));
      }

      return ex;
    }
    
    // lambda
    public static Expression Lambda(object args, CodeBlock c)
    {
      CodeBlock cb = Ast.CodeBlock(NameHint);
      cb.Parent = c;

      object arg = Builtins.First(args);
      Cons body = Builtins.Cdr(args) as Cons;

      bool isrest = AssignParameters(cb, arg);

      List<Statement> stmts = new List<Statement>();
      FillBody(cb, stmts, body, true);

      Expression ex = MakeClosure(cb, isrest);

      //if (NameHint != SymbolId.Empty)
      //{
      //  //cb.Parent = null;
      //  cb.BindClosures();
      //  Delegate md = cb.GetDelegateForInterpreter(Compiler, false) as Delegate;
      //  if (isrest)
      //  {
      //    Compiler.Scope.SetName(NameHint, Closure.MakeVarArgX(Compiler, md, cb.Parameters.Count, cb.Name));
      //  }
      //  else
      //  {
      //    Compiler.Scope.SetName(NameHint, Closure.Make(Compiler, md, cb.Name));
      //  }
      //}

      return ex;
    }

    static readonly MethodInfo istrue = typeof(Builtins).GetMethod("IsTrue");

    // if
    public static Expression If(object args, CodeBlock cb)
    {
      object test = Builtins.First(args);
      object trueexp = Builtins.Second(args);
      object falseexp = Builtins.Third(args);
      
      Expression e = null;
      if (falseexp != null)
      {
        e = GetAst(falseexp, cb);
      }
      else
      {
        e = Ast.Null();
      }

      Expression testexp = Ast.Call(null, istrue, GetAst(test,cb));

      return Ast.Condition(testexp, GetAst(trueexp, cb), e, true);
    }

    static int nestinglevel = 0;

    public static Expression Quasiquote(object args, CodeBlock cb)
    {
      nestinglevel++;
      try
      {
        return GetCons(Builtins.First(args), cb);
      }
      finally
      {
        nestinglevel--;
      }
    }
  }

}
