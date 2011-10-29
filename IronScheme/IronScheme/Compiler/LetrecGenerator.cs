#region License
/* Copyright (c) 2007,2008,2009,2010,2011 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System.Collections.Generic;
using IronScheme.Runtime;
using Microsoft.Scripting;
using Microsoft.Scripting.Ast;

namespace IronScheme.Compiler
{

  [Generator("letrec")]
  public sealed class LetrecGenerator : SimpleGenerator
  {
    
    int level = 0;
    public override Expression Generate(object args, CodeBlock c)
    {
      var refs = ClrGenerator.SaveReferences();

      level++;
      NameHint = SymbolTable.StringToId("");
      CodeBlock cb = Ast.CodeBlock(SpanHint, GetLambdaName(c));
      cb.Parent = c;

      List<Variable> vars = new List<Variable>();
      List<Variable> temps = new List<Variable>();
      List<object> defs = new List<object>();

      Cons a = (args as Cons).car as Cons;

      while (a != null)
      {
        Cons d = a.car as Cons;
        Variable t = cb.CreateVariable((SymbolId)Builtins.GenSym(d.car), Variable.VariableKind.Temporary, typeof(object));
        temps.Add(t);
        Variable r = Create((SymbolId)d.car, cb, typeof(object));
        r.SetUnInitialized();
        vars.Add(r);
        defs.Add(((Cons)d.cdr).car);

        a = a.cdr as Cons;
      }

      List<Statement> stmts = new List<Statement>();

      for (int i = 0; i < vars.Count; i++)
      {
        NameHint = Builtins.UnGenSymInternal(vars[i].Name);
        Expression e = GetAst(defs[i], cb);

        if (e is MethodCallExpression)
        {
          MethodCallExpression mce = (MethodCallExpression)e;
          if (mce.Method == Closure_Make || mce.Method == Closure_MakeCase || mce.Method == Closure_MakeVarArgsX)
          {
            vars[i].SetInitialized();
          }
        }

        if (e.Type.IsValueType)
        {
          e = Ast.ConvertHelper(e, typeof(object));
        }
        stmts.Add(Ast.Write(temps[i], e));
      }

      for (int i = 0; i < vars.Count; i++)
      {
        stmts.Add(Ast.Write(vars[i], temps[i]));
      }

      NameHint = SymbolId.Invalid;
      
      Cons body = Builtins.Cdr(args) as Cons;

#if CPS
      //TryStatement ts = Ast.TryCatch(SourceSpan.None, SourceLocation.None, Ast.Block(stmts.ToArray()), 
      // Ast.Catch(typeof(Exception), cb.CreateTemporaryVariable(SymbolTable.StringToObject("$ex"), typeof(Exception)), 
      // Ast.Empty()));
      //stmts = new List<Statement>();
      //stmts.Add(ts);
#endif

      FillBody(cb, stmts, body, true);

#if OPTIMIZATIONS
      Expression ex = InlineCall(c, Ast.CodeBlockExpression(cb, false));
#else
      Expression ex = Ast.SimpleCallHelper(MakeClosure(cb, false), GetCallable(0));
#endif

      level--;

      ClrGenerator.ResetReferences(refs);

      return ex;
    }
  }
}
