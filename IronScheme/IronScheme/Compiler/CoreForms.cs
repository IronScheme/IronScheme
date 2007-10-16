using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting.Ast;
using IronScheme.Runtime;
using Microsoft.Scripting;

namespace IronScheme.Compiler
{
  static class Generator
  {
    delegate Expression GeneratorHandler(object args, CodeBlock cb);

    readonly static Dictionary<SymbolId, GeneratorHandler> generators = new Dictionary<SymbolId, GeneratorHandler>();

    static Generator()
    {
      Add("set!", Set);
      Add("quote", Quote);
      Add("lambda", Lambda);
      Add("if", If);
    }

    static void Add(string name, GeneratorHandler handler)
    {
      generators.Add(SymbolTable.StringToId(name), handler);
    }

    public static Expression GetCons(object args, CodeBlock cb)
    {
      Cons c = args as Cons;
      if (c != null)
      {
        List<Expression> items = new List<Expression>();
        while (c != null)
        {
          items.Add(GetCons(Builtins.Car(c), cb));
          c = c.Cdr as Cons;
        }
        return Ast.Call(null, typeof(Builtins).GetMethod("List", new Type[] { typeof(object[]) }), items.ToArray());
      }
      else
      {
        return Ast.Constant(args);
      }

    }

    public static Expression GetAst(object args, CodeBlock cb)
    {
      Cons c = args as Cons;
      if (c != null)
      {
        object first = Builtins.First(args);
        if (Builtins.IsSymbol(first))
        {
          GeneratorHandler gh;
          if (generators.TryGetValue((SymbolId)first, out gh))
          {
            return gh(c.Cdr, cb);
          }
        }
        return null;
      }
      else
      {
        if (Builtins.IsSymbol(args))
        {
          return Ast.Read((SymbolId)args);
        }
        return Ast.Constant(args);
      }
    }
    // quote
    public static Expression Quote(object args, CodeBlock cb)
    {
      return GetCons(Builtins.Car(args), cb);
    }

    // set!
    public static Expression Set(object args, CodeBlock cb)
    {
      SymbolId s = (SymbolId)Builtins.First(args);
      Expression value = GetAst(Builtins.Second(args), cb);

      return Ast.Assign(s, value);
    }



    // lambda
    public static Expression Lambda(object args, CodeBlock c)
    {
      CodeBlock cb = Ast.CodeBlock("lambda");
      cb.Parent = c;

      object arg = Builtins.First(args);
      object body = Builtins.Cdr(args);

      bool isrest = false;

      Cons cargs = arg as Cons;
      if (cargs != null)
      {
        while (cargs != null)
        {
          SymbolId an = (SymbolId)Builtins.First(cargs);
          cb.CreateParameter(an, typeof(object), null);

          Cons r = cargs.Cdr as Cons;

          if (r == null && cargs.Cdr != null)
          {
            SymbolId ta = (SymbolId)cargs.Cdr;
            cb.CreateParameter(ta, typeof(object), null);
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
        cb.CreateParameter(an, typeof(object), null);
      }

      List<Statement> stmts = new List<Statement>();
      //FillBody(cb, stmts, body);

     Expression ex = null;

      //if (isrest)
      //{
      //  ex =
      //    Mast.Call(null, typeof(Closure).GetMethod("MakeVarArgX"), Mast.CodeContext(),
      //    Mast.CodeBlockExpression(cb, false, false), Mast.Constant(cb.Parameters.Count), Mast.Constant(cb.Name));
      //}
      //else
      //{
      //  ex = MakeClosure(cb);
      //}

      //if (namehint != null)
      //{
      //  //cb.Parent = null;
      //  cb.BindClosures();
      //  if (isrest)
      //  {
      //    Compiler.Scope.SetName(SymbolTable.StringToId(namehint), Closure.MakeVarArgX(Compiler, cb.GetDelegateForInterpreter(Compiler, false), cb.Parameters.Count, cb.Name));
      //  }
      //  else
      //  {
      //    Compiler.Scope.SetName(SymbolTable.StringToId(namehint), Closure.Make(Compiler, cb.GetDelegateForInterpreter(Compiler, false), cb.Name));
      //  }
      //}

      return ex;
    }

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

      Expression testexp = Ast.Call(null, typeof(Builtins).GetMethod("IsTrue"), GetAst(test,cb));

      return Ast.Condition(testexp, GetAst(trueexp, cb), e, true);
    }
  }



  /*
  public class Lambda : SpecialExpression
  {
    internal string namehint;
    ExpressionList args;
    ExpressionList body;
    internal CodeBlock cb;

    internal ExpressionList Body
    {
      get { return body; }
    }

    internal ExpressionList Parameters
    {
      get { return args; }
    }

    public Lambda(ExpressionList args, ExpressionList body)
    {
      this.args = args;
      this.body = body;
    }

    public override MSA.Expression GetAst(CodeBlock c)
    {
      string cbname = GetFullname(c, namehint ?? "lambda");

      CodeBlock cb = Mast.CodeBlock(cbname);
      cb.Parent = c;

      this.cb = cb;

      bool isrest = false;


      foreach (Symbol s in args)
      {
        if (s.Name == "rested")
        {
          ;
        }
        if (s.Name == ".")
        {
          isrest = true;
        }
        else
        {
          if (isrest)
          {
            CreateParameter(s.Name, cb, typeof(IEnumerable));
          }
          else
          {
            CreateParameter(s.Name, cb, typeof(object));
          }
        }
      }

      List<Statement> stmts = new List<Statement>();
      FillBody(cb, stmts, body);

      MSA.Expression ex = null;

      if (isrest)
      {
        ex =
          Mast.Call(null, typeof(Closure).GetMethod("MakeVarArgX"), Mast.CodeContext(),
          Mast.CodeBlockExpression(cb, false, false), Mast.Constant(cb.Parameters.Count), Mast.Constant(cb.Name));
      }
      else
      {
        ex = MakeClosure(cb);
      }

      if (namehint != null)
      {
        //cb.Parent = null;
        cb.BindClosures();
        if (isrest)
        {
          Compiler.Scope.SetName(SymbolTable.StringToId(namehint), Closure.MakeVarArgX(Compiler, cb.GetDelegateForInterpreter(Compiler, false), cb.Parameters.Count, cb.Name));
        }
        else
        {
          Compiler.Scope.SetName(SymbolTable.StringToId(namehint), Closure.Make(Compiler, cb.GetDelegateForInterpreter(Compiler, false), cb.Name));
        }
      }

      return ex;
    }



    public override string ToString()
    {
      if (args.Count == 2 && ((Symbol)args[0]).Name == ".")
      {
        return string.Format("(lambda {0} {1})", args[1], Join(body));
      }
      return string.Format("(lambda ({0}) {1})", Join(args), Join(body));
    }
  }
   */

}
