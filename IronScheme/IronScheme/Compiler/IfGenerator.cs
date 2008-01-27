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
  [Generator("if")]
  public class IfGenerator : SimpleGenerator
  {
    public override Expression Generate(object args, CodeBlock cb)
    {
      int alen = (int)Builtins.Length(args);
      if (alen < 2 || alen > 3)
      {
        Builtins.SyntaxError("if", "argument mismatch. expected: (if a b c?) got: " + new Cons("if", args), args, false);
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
        e = Ast.ConvertHelper(e, typeof(object));
      }

      Expression t = GetAst(trueexp, cb);

      if (t.Type != typeof(object))
      {
        t = Ast.ConvertHelper(t, typeof(object));
      }

      Expression testexp = GetAst(test, cb);

      if (testexp.Type != typeof(bool))
      {
        testexp = Ast.SimpleCallHelper(Builtins_IsTrue, testexp);
      }

      return Ast.Condition(testexp, t, e);
    }
  }
}
