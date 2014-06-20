#region License
/* Copyright (c) 2007-2014 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using IronScheme.Runtime;
using Microsoft.Scripting.Ast;

namespace IronScheme.Compiler
{
  [Generator("if")]
  sealed class IfGenerator : SimpleGenerator
  {
    public override Expression Generate(object args, CodeBlock cb)
    {
      Cons c = args as Cons;
      int alen = c.Length;
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
        e = Unwrap(GetAst(falseexp, cb));
      }
      else
      {
        e = Ast.ReadField(null, Unspecified);
      }

      Expression t = Unwrap(GetAst(trueexp, cb));

      if (e.Type != typeof(object) && e.Type != t.Type)
      {
        e = Ast.ConvertHelper(e, typeof(object));
      }

      if (t.Type != typeof(object) && e.Type != t.Type)
      {
        t = Ast.ConvertHelper(t, typeof(object));
      }

      Expression testexp = Unwrap(GetAst(test, cb));

      if (testexp is ConstantExpression)
      {
        if (testexp.IsConstant(false))
        {
          return e;
        }
        else
        {
          return t;
        }
      }

      if (testexp.Type != typeof(bool))
      {
        testexp = Ast.SimpleCallHelper(Builtins_IsTrue, testexp);
        //testexp = Ast.NotEqual(Ast.Constant(false), testexp);
      }

      return Ast.Condition(testexp, t, e);
    }
  }
}
