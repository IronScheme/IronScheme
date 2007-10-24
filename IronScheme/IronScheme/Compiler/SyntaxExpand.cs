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
  static class SyntaxExpander
  {


    public static Cons Expand(Cons input)
    {
      Cons c = input;
      while (c != null)
      {
        c.Car = Expand(c.Car);
        if (c.Cdr != null && !(c.Cdr is Cons))
        {
          c.Cdr = Expand(c.Cdr);
          break;
        }
        c = c.Cdr as Cons;
      }
      return input;
    }

    public static object Expand(object input)
    {
      Cons c = input as Cons;
      if (c != null)
      {
        if (Builtins.IsSymbol(c.Car))
        {
          SymbolId s = (SymbolId)c.Car;

          if (s == Generator.unquote || s == Generator.quasiquote)
          {
            return input;
          }

          object value;
          if (Generator.Compiler.Scope.TryLookupName(s, out value))
          {
            if (value is Runtime.Macro)
            {
              Runtime.Macro m = value as Runtime.Macro;
              object result = Expand(m.Invoke(Generator.Compiler, c.Cdr));

              if (result == null)
              {
                Debugger.Break();
              }

              return result;
            }
          }
        }
        return Expand(c);
      }
      return input;
    }
  }
}
