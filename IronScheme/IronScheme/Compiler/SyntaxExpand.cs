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
using Microsoft.Scripting;
using System.Diagnostics;
using IronScheme.Runtime;

namespace IronScheme.Compiler
{
  static class SyntaxExpander
  {
    static Cons Expand(Cons input)
    {
      Cons c = input;
      while (c != null)
      {
        c.car = Expand(c.car);
        if (c.cdr != null && !(c.cdr is Cons))
        {
          c.cdr = Expand(c.cdr);
          break;
        }
        c = c.cdr as Cons;
      }
      return input;
    }

    public static object Expand(object input)
    {
      return Expand(input, false);
    }

    public static object Expand1(object input)
    {
      return Expand(input, true);
    }

    static object Expand(object input, bool expand1)
    {
      Cons c = input as Cons;
      if (c != null)
      {
        if ((bool)Builtins.IsSymbol(c.car))
        {
          SymbolId s = (SymbolId)c.car;

          if (s == Generator.quote || s == Generator.quasiquote)
          {
            return input;
          }


          if ((bool)Builtins.IsEqual(s, Generator.define) && (bool)Builtins.IsPair(Builtins.Second(c)))
          {
            Cons t = (Cons)c.cdr;
            Cons r = (Cons)t.car;
            Cons l = Cons.FromArray(r.car,
              Builtins.Append(Cons.FromArray(Generator.lambda, r.cdr), Builtins.Cdr(t)));
            c.cdr = l;
            return Expand((object)c, expand1);
          }

          object value;
          if (BaseHelper.cc.Scope.TryLookupName(s, out value))
          {
            if (value is Runtime.Macro)
            {
              Runtime.Macro m = value as Runtime.Macro;

              object result = m.Invoke(BaseHelper.cc, c.cdr);
              if (result is Cons && Parser.sourcemap.ContainsKey(c))
              {
                Parser.sourcemap[result] = Parser.sourcemap[c];
              }
              if (expand1)
              {
                return result;
              }
              result = Expand(result);

              return result;
            }
          }
        }
        return c;
      }
      return input;
    }
  }
}
