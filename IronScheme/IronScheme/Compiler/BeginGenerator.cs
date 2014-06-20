#region License
/* Copyright (c) 2007-2014 Llewellyn Pritchard 
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
      else
      {
        // discard effectfree
        List<Expression> newargs = new List<Expression>();
        Expression[] aa = GetAstList(args as Cons, cb);
        if (aa.Length == 1)
        {
          return aa[0];
        }
        for (int i = 0; i < aa.Length - 1; i++)
        {
          Expression a = aa[i];
          Expression uwa = Unwrap(a);
          if (uwa is ConstantExpression)
          {
            continue;
          }
          if (uwa is MemberExpression)
          {
            MemberExpression me = uwa as MemberExpression;
            if (me.Member == Unspecified)
            {
              continue;
            }
          }
          if (uwa is CommaExpression)
          {
            newargs.AddRange(((CommaExpression)uwa).Expressions);
          }
          else
          {
            newargs.Add(a);
          }
        }
        if (newargs.Count == 0)
        {
          return aa[aa.Length - 1];
        }
        else
        {
          Expression uwa = aa[aa.Length - 1];
          if (uwa is CommaExpression)
          {
            newargs.AddRange(((CommaExpression)uwa).Expressions);
          }
          else
          {
            newargs.Add(uwa);
          }

          return Ast.Comma(newargs);
        }
      }
    }
  }
}
