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
using System.Diagnostics;

[assembly: Extension(GeneratorType=typeof(Generator), BuiltinsType=typeof(Builtins))]

namespace IronScheme.Compiler
{
  [AttributeUsage(AttributeTargets.Method, AllowMultiple=true)]
  public sealed class GeneratorAttribute : Attribute
  {
    string name;

    public string Name
    {
      get { return name; }
      set { name = value; }
    }

    public GeneratorAttribute()
    {
    }

    public GeneratorAttribute(string name)
    {
      this.name = name;
    }
  }

  public partial class Generator
  {
    static Generator()
    {
      
      
      Initialize();
    }

    public readonly static FieldInfo Unspecified = typeof(Builtins).GetField("Unspecified");

    public static Expression GetCons(object args, CodeBlock cb)
    {
      Cons c = args as Cons;
      if (c != null)
      {
        if (nestinglevel == 1 && Builtins.IsEqual(c.Car, unquote))
        {
          nestinglevel--;
          try
          {
            return GetAst(Builtins.Second(c), cb);
          }
          finally
          {
            nestinglevel++;
          }
        }
        return GetConsList(c, cb);
      }
      object[] v = args as object[];
      if (v != null)
      {
        return GetConsVector(v, cb);
      }
      else
      {
        return Ast.Constant(args);
      }
    }

    static bool macrotrace = false;

    public static bool MacroTrace
    {
      get { return Generator.macrotrace; }
      set { Generator.macrotrace = value; }
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
              if (macrotrace)
              {
                Debug.WriteLine(Builtins.DisplayFormat(c), "macro::in ");
              }
              object result = SyntaxExpander.Expand(macro.Invoke(Compiler, c.Cdr));
              if (macrotrace)
              {
                Debug.WriteLine(Builtins.DisplayFormat(result), "macro::out");
              }
              if (result is Cons && Parser.sourcemap.ContainsKey(c))
              {
                Parser.sourcemap[(Cons)result] = Parser.sourcemap[c];
              }
              return GetAst(result, cb);
            }

            GeneratorHandler gh = m as GeneratorHandler;
            if (gh != null)
            {
              if (!Parser.sourcemap.TryGetValue(c, out spanhint))
              {
                spanhint = SourceSpan.None;
              }
              return gh(c.Cdr, cb);
            }

            BuiltinFunction bf = m as BuiltinFunction;
            if (bf != null)
            {
              MethodBinder mb = MethodBinder.MakeBinder(BINDER, SymbolTable.IdToString(f), bf.Targets, BinderType.Normal);
              Expression[] pars = GetAstList(c.Cdr as Cons, cb);
              Type[] types = GetExpressionTypes(pars);
              MethodCandidate mc = mb.MakeBindingTarget(CallType.None, types);
              if (mc == null)
              {
                throw new SyntaxErrorException("no match for " + f);
              }
              if (mc.Target.NeedsContext)
              {
                pars = ArrayUtils.Insert<Expression>(Ast.CodeContext(), pars);
              }
              return Ast.ComplexCallHelper(mc.Target.Method as MethodInfo, pars);
            }
          }


        }
        return Ast.Action.Call(typeof(object), GetAstList(c, cb));
      }
      object[] v = args as object[];
      if (v != null)
      {
        return GetConsVector(v, cb);
      }
      else
      {
        if (Builtins.IsSymbol(args))
        {
          return Read((SymbolId)args, cb, typeof(object));
        }
        if (args == Builtins.Unspecified)
        {
          return Ast.ReadField(null, Unspecified);
        }
        return Ast.Constant(args);
      }
    }

    static SourceSpan spanhint;

    // macro-expand
    [Generator("macro-expand")]
    public static Expression MacroExpand(object args, CodeBlock cb)
    {
      args = Builtins.Car(args);
      object result = SyntaxExpander.Expand(args);
      return GetCons(result, cb);
    }

    // macro-expand1
    [Generator("macro-expand1")]
    public static Expression MacroExpand1(object args, CodeBlock cb)
    {
      args = Builtins.Car(args);
      object result = SyntaxExpander.Expand1(args);
      return GetAst(result, cb);
    }


    // quote
    [Generator]
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
    [Generator("set!")]
    public static Expression Set(object args, CodeBlock cb)
    {
      SymbolId s = (SymbolId)Builtins.First(args);

      namehint = s;

      Expression value = GetAst(Builtins.Second(args), cb);

      namehint = SymbolId.Invalid;

      Variable v = FindVar(cb, s);

      if (v == null)
      {
        throw new MissingMemberException(string.Format("name '{0}' not defined", SymbolTable.IdToString(s)));
      }

      if (value.Type.IsValueType)
      {
        value = Ast.DynamicConvert(value, typeof(object));
      }

      return Ast.Comma( Ast.Assign(v, value), Ast.ReadField(null, Unspecified));
    }
    
    // define
    [Generator]
    public static Expression Define(object args, CodeBlock cb)
    {

      object f = Builtins.First(args);
      if (f is SymbolId)
      {
        SymbolId s = (SymbolId)f;

        namehint = s;

        Variable v = Create(s, cb, typeof(object));

        object second = Builtins.Second(args);

        Expression value = GetAst(second, cb);

        namehint = SymbolId.Invalid;

        if (value.Type.IsValueType)
        {
          value = Ast.DynamicConvert(value, typeof(object));
        }
        Expression r = Ast.Comma(Ast.Assign(v, value), Ast.ReadField(null, Unspecified));
        if (cb.IsGlobal)
        {
          object o = r.Evaluate(Compiler);
        }
        return r;
      }
      else
      {
        Cons r = (Cons)f;
        Cons l = Cons.FromArray(r.Car, 
          Builtins.Append(Cons.FromArray(SymbolTable.StringToId("lambda"), r.Cdr), Builtins.Cdr(args)));

        return Define(l, cb);
      }
    }



    // macro
    [Generator]
    public static Expression Macro(object args, CodeBlock c)
    {
      CodeBlock cb = Ast.CodeBlock(GetFullname(NameHint, c));
      cb.Parent = c;

      object arg = Builtins.First(args);
      Cons body = Builtins.Cdr(args) as Cons;

      bool isrest = AssignParameters(cb, arg);

      List<Statement> stmts = new List<Statement>();
      FillBody(cb, stmts, body, true);

      cb.BindClosures();

      CodeBlockExpression cbe = Ast.CodeBlockExpression(cb, false);

      Expression ex = Ast.SimpleCallHelper(isrest ? Macro_MakeVarArgX : Macro_Make, Ast.CodeContext(), cbe, 
        Ast.Constant(cb.Parameters.Count), Ast.Constant(cb.Name));

      return ex;
    }
    
    // lambda
    [Generator]
    public static Expression Lambda(object args, CodeBlock c)
    {
      CodeBlock cb = Ast.CodeBlock(spanhint, GetFullname(NameHint, c));
      cb.Parent = c;

      object arg = Builtins.First(args);
      Cons body = Builtins.Cdr(args) as Cons;

      bool isrest = AssignParameters(cb, arg);

      List<Statement> stmts = new List<Statement>();
      FillBody(cb, stmts, body, true);

      //if (!isrest)
      //{
      //  cb.BindClosures();
      //  return Ast.CodeBlockExpression(cb, false);
      //}

      Expression ex = MakeClosure(cb, isrest);

      return ex;
    }

    // if
    [Generator]
    public static Expression If(object args, CodeBlock cb)
    {
      int alen = Builtins.Length(args);
      if (alen < 2 || alen > 3)
      {
        throw new SyntaxErrorException("argument mismatch. expected: (if a b c?) got: " + new Cons("if", args));
      }
      object test = Builtins.First(args);
      object trueexp = Builtins.Second(args);
      object falseexp = alen == 3 ? Builtins.Third(args) : null;

      // fast check for (if #f #f) == Unspecified or (if #t ...)
      if (test is bool) // constant
      {
        bool tt = (bool)test;
        if (tt)
        {
          return GetAst(test, cb);
        }
        else
        {
          if (falseexp == null)
          {
            return Ast.ReadField(null, Unspecified);
          }
          else
          {
            return GetAst(falseexp, cb);
          }
        }
      }

      Expression e = null;
      if (falseexp != null)
      {
        e = GetAst(falseexp, cb);
      }
      else
      {
        e = Ast.ReadField(null, Unspecified);
      }

      if (e.Type != typeof(object))
      {
        e = Ast.DynamicConvert(e, typeof(object));
      }

      Expression t = GetAst(trueexp, cb);

      if (t.Type != typeof(object))
      {
        t = Ast.DynamicConvert(t, typeof(object));
      }

      Expression testexp = GetAst(test, cb);

      if (testexp.Type != typeof(bool))
      {
        testexp = Ast.SimpleCallHelper(Builtins_IsTrue, testexp);
      }

      return Ast.Condition(testexp, t, e);
    }

    static int nestinglevel = 0;

    // quasiquote
    [Generator]
    public static Expression Quasiquote(object args, CodeBlock cb)
    {
      nestinglevel++;
      try
      {
        if (nestinglevel == 1)
        {
          return GetCons(Builtins.First(args), cb);
        }
        else
        {
          // does this ever get hit?
          Debugger.Break(); 
          return Ast.SimpleCallHelper(Builtins_Cons2, Ast.Constant(quasiquote), GetCons(args, cb));
        }
      }
      finally
      {
        nestinglevel--;
      }
    }
  }

}
