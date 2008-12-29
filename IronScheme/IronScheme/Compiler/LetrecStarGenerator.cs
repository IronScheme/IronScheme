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
using Microsoft.Scripting.Ast;
using IronScheme.Runtime;
using Microsoft.Scripting;
using System.Reflection;

namespace IronScheme.Compiler
{

  [Generator("letrec*")]
  public sealed class LetrecStarGenerator : SimpleGenerator
  {
    int level = 0;
    public override Expression Generate(object args, CodeBlock c)
    {
      level++;
      NameHint = SymbolTable.StringToId("letrec*");
      CodeBlock cb = Ast.CodeBlock(SpanHint, GetLambdaName(c));
      cb.Parent = c;

      List<Variable> vars = new List<Variable>();
      List<object> defs = new List<object>();

      Cons a = (args as Cons).car as Cons;

      while (a != null)
      {
        Cons d = a.car as Cons;
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
        stmts.Add(Ast.Write(vars[i], e));
      }

      NameHint = SymbolId.Invalid;

      Cons body = Builtins.Cdr(args) as Cons;

      FillBody(cb, stmts, body, true);

#if OPTIMIZATIONS
      Expression ex = InlineCall(c, Ast.CodeBlockExpression(cb, false));
#else
      Expression ex = Ast.SimpleCallHelper(MakeClosure(cb, false), GetCallable(0));
#endif

      level--;
      return ex;
    }
  }
}
