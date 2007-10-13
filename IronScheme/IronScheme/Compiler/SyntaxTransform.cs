#region License
/* ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 
 *
 * This source code is subject to terms and conditions of the Microsoft Permissive License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Permissive License.
 *
 * You must not remove this notice, or any other, from this software.
 * ***************************************************************************/
#endregion

using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting;
using System.Diagnostics;
using IronScheme.Runtime;

namespace IronScheme.Compiler
{
  static class SyntaxTransform
  {
    public static Cons Transform(Cons input)
    {
      Cons c = input;
      do
      {
        c.Car = Transform(c.Car);
        if (!(c.Cdr is Cons))
        {
          c.Cdr = Transform(c.Cdr);
        }
        c = c.Cdr as Cons;
      }
      while (c != null);
      return input;
    }

    public static object Transform(object input)
    {
      //ExpressionList exprlist = input as ExpressionList;

      //if (exprlist != null)
      //{
      //  if (exprlist.Count == 2 && exprlist[0] is Symbol)
      //  {
      //    Symbol s = exprlist[0] as Symbol;

      //    switch (s.Name)
      //    {
      //      case "quote":
      //        return new Quote(Transform(exprlist[1]));
      //      case "quasiquote":
      //        return new Quasiquote(Transform(exprlist[1]));
      //      case "unquote":
      //        return new Unquote(Transform(exprlist[1]));
      //      case "unquote-splicing":
      //        return new UnquoteSplicing(Transform(exprlist[1]));

      //    }
      //  }
      //  if (exprlist.Count > 0 && exprlist[0] is Symbol)
      //  {
      //    object value;
      //    if (Expression.Compiler.Scope.TryLookupName(SymbolTable.StringToId(((Symbol)exprlist[0]).Name), out value))
      //    {
      //      if (value is Runtime.Macro)
      //      {
      //        Debug.WriteLine(exprlist, "macro:input");
              
      //        Runtime.Macro m = value as Runtime.Macro;
      //        object result = m.Call(Expression.Compiler, new List<Expression>(exprlist.Rest()).ToArray());

      //        Debug.WriteLine(result, "macro:output");

      //        if (result == null)
      //        {
      //          Debugger.Break();
      //        }
              
      //        // remember to take it thru transform again
      //        Expression r = IronLispLanguageContext.ReadExpressionString(result.ToString());
      //        return r;
      //      }
      //    }
      //  }
      //  return Transform(exprlist);
      //}
      //else
      //{
      //  return input;
      //}
      return input;
    }
  }
}
