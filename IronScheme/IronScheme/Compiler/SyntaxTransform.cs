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
      return input;
      //Cons c = input;
      //while (c != null)
      //{
      //  c.Car = Transform(c.Car);
      //  if (c.Cdr != null && !(c.Cdr is Cons))
      //  {
      //    c.Cdr = Transform(c.Cdr);
      //    break;
      //  }
      //  c = c.Cdr as Cons;
      //}
      //return input;
    }

    public static object Transform(object input)
    {
      //Cons c = input as Cons;
      //if (c != null && Builtins.IsSymbol(c.Car))
      //{
      //  SymbolId s = (SymbolId)c.Car;

      //  object value;
      //  if (Generator.Compiler.Scope.TryLookupName(s, out value))
      //  {
      //    if (value is Runtime.Macro)
      //    {
      //      Debug.WriteLine(c, "macro:in ");

      //      Runtime.Macro m = value as Runtime.Macro;
      //      object result = m.Invoke(Generator.Compiler, c.Cdr);
      //      Debug.WriteLine(result, "macro:out");

      //      if (result == null)
      //      {
      //        Debugger.Break();
      //      }

      //      return Transform(result);
      //    }
      //  }
      //}
      return input;
    }
  }
}
