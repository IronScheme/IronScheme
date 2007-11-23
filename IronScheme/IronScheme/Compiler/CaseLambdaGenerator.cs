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

namespace IronScheme.Compiler
{
  [Generator("case-lambda")]
  public class CaseLambdaGenerator : SimpleGenerator
  {
    public override Expression Generate(object args, CodeBlock c)
    {
      Cons lambdas = args as Cons;

      Dictionary<int, CodeBlockExpression> cbs = new Dictionary<int, CodeBlockExpression>();

      string lambdaname = GetLambdaName(c);

      while (lambdas != null)
      {
        object actual = lambdas.Car;
        CodeBlock cb = Ast.CodeBlock(SpanHint, lambdaname);
        cb.Parent = c;

        object arg = Builtins.First(actual);
        Cons body = Builtins.Cdr(actual) as Cons;

        bool isrest = AssignParameters(cb, arg);

        List<Statement> stmts = new List<Statement>();
        FillBody(cb, stmts, body, true);

        cbs.Add(isrest ? -1 : cb.Parameters.Count, Ast.CodeBlockExpression(cb, false));

        lambdas = lambdas.Cdr as Cons;
      }

      return MakeCaseClosure(lambdaname, cbs);
    }
  }
}
