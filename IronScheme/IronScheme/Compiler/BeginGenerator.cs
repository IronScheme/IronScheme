#region License
/* Copyright (c) 2007-2016 Llewellyn Pritchard
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System.Collections.Generic;
using IronScheme.Runtime;
using Microsoft.Scripting.Ast;

namespace IronScheme.Compiler
{
  [Generator("begin")]
  sealed class BeginGenerator : SimpleGenerator
  {
    public override Expression Generate(object args, CodeBlock cb)
    {
      if (args == null)
      {
        return Ast.ReadField(null, Unspecified);
      }

      // discard effectfree
      List<Expression> newargs = new List<Expression>();
      Expression[] aa = GetAstList(args as Cons, cb);
      if (aa.Length == 1)
      {
        return Unwrap(aa[0]);
      }

      for (int i = 0; i < aa.Length - 1; i++)
      {
        Expression a = aa[i];
        Expression uwa = Unwrap(a);
        switch (uwa)
        {
          case ConstantExpression _:
          case MemberExpression me when me.Member == Unspecified:
            continue;
          case CommaExpression comma:
            newargs.AddRange(comma.Expressions);
            break;
          default:
            newargs.Add(a);
            break;
        }
      }

      if (newargs.Count == 0)
      {
        return aa[aa.Length - 1];
      }

      Expression uwb = aa[aa.Length - 1];
      if (uwb is CommaExpression commaExpr)
      {
        newargs.AddRange(commaExpr.Expressions);
      }
      else
      {
        newargs.Add(uwb);
      }

      return Ast.Comma(newargs);
    }
  }
}
