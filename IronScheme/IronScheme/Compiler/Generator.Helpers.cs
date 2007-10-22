using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting;
using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Types;
using System.Reflection;
using IronScheme.Runtime;
using Microsoft.Scripting.Hosting;
using IronScheme.Hosting;

namespace IronScheme.Compiler
{
  static partial class Generator
  {
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

    static void Initialize()
    {
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


    static CodeContext CC;

    public static CodeContext Compiler
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

    static readonly Microsoft.Scripting.Actions.ActionBinder BINDER = new IronScheme.Actions.IronSchemeActionBinder(null);

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


    static Expression MakeClosure(CodeBlock cb, bool varargs)
    {
      return Ast.SimpleCallHelper(varargs ? Closure_MakeVarArgsX : Closure_Make
        , Ast.CodeContext(), Ast.CodeBlockExpression(cb, false, false), Ast.Constant(cb.Name));
    }

    static void FillBody(CodeBlock cb, List<Statement> stmts, Cons body, bool allowtailcall)
    {
      Cons c = body;
      while (c != null)
      {
        Expression e = GetAst(c.Car, cb);
        Statement s = null;
        if (c.Cdr == null)
        {
          if (allowtailcall)
          {
            if (e is MethodCallExpression && e.Type != typeof(void))
            {
              // ((MethodCallExpression)e).TailCall = true;
            }
            if (e is ActionExpression && e.Type != typeof(void))
            {
              //((ActionExpression)e).TailCall = true;
            }
          }

          s = Ast.Return(e);
        }
        else
        {
          s = Ast.Statement(e);
        }

        if (Parser.sourcemap.ContainsKey(c.Car as Cons))
        {
          s.SetLoc(Parser.sourcemap[c.Car as Cons]);
        }

        stmts.Add(s);
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
        Expression[] aa = e.ToArray();
        MethodInfo lm = MakeList(aa);
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
      }

      if (!proper)
      {
        r = Ast.SimpleCallHelper(Builtins_ToImproper, r);
      }

      return r;
    }

    static string GetFullname(SymbolId id, CodeBlock parent)
    {
      if (parent == null || parent.IsGlobal)
      {
        return SymbolTable.IdToString(id);
      }
      else
      {
        return parent.Name + "::" + SymbolTable.IdToString(id);
      }
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

    public static SymbolId NameHint
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
