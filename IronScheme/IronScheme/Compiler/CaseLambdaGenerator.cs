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

namespace IronScheme.Compiler
{
  [Generator("case-lambda")]
  public sealed class CaseLambdaGenerator : SimpleGenerator
  {
    LambdaGenerator lambdagen;
    public override Expression Generate(object args, CodeBlock c)
    {
      Cons lambdas = args as Cons;

      int arlen = (int)Builtins.Length(args);

      if (arlen == 1)
      {
        if (lambdagen == null)
        {
          lambdagen = Context.Scope.LookupName(SymbolTable.StringToId("lambda")) as LambdaGenerator;
        }
        return lambdagen.Generate(lambdas.car, c);
      }
      else
      {
        List<CodeBlockDescriptor> cbs = new List<CodeBlockDescriptor>();

        string lambdaname = GetLambdaName(c);

        while (lambdas != null)
        {
          object actual = lambdas.car;
          CodeBlock cb = Ast.CodeBlock(SpanHint, lambdaname);
          cb.Parent = c;

          object arg = Builtins.First(actual);
          Cons body = Builtins.Cdr(actual) as Cons;

          bool isrest = AssignParameters(cb, arg);

          List<Statement> stmts = new List<Statement>();
          FillBody(cb, stmts, body, true);

          CodeBlockDescriptor cbd = new CodeBlockDescriptor();
          cbd.arity = isrest ? -cb.ParameterCount : cb.ParameterCount;
          cbd.codeblock = Ast.CodeBlockExpression(cb, false);

          cbs.Add(cbd);

          lambdas = lambdas.cdr as Cons;
        }

        return MakeCaseClosure(lambdaname, cbs);
      }
    }
  }
}
