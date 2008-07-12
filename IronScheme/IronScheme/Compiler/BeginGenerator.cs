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
  [Generator("begin")]
  public sealed class BeginGenerator : SimpleGenerator
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
