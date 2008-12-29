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
using IronScheme.Compiler;
using Microsoft.Scripting;
using Microsoft.Scripting.Utils;
using Microsoft.Scripting.Math;
using System.Collections.Specialized;

namespace IronScheme.Runtime.R6RS.Arithmetic
{
  public class Fixnums : Builtins
  {
    [Builtin("fx+")]
    public static object FxAdd(object a, object b)
    {
      int x1 = RequiresNotNull<int>(a);
      int x2 = RequiresNotNull<int>(b);

      try
      {
        return checked(x1 + x2);
      }
      catch (OverflowException)
      {
        return ImplementationRestriction("fx+", "arithmetic overflow", a , b);
      }
    }

    [Builtin("fx*")]
    public static object FxMultiply(object a, object b)
    {
      int x1 = RequiresNotNull<int>(a);
      int x2 = RequiresNotNull<int>(b);

      try
      {
        return checked(x1 * x2);
      }
      catch (OverflowException)
      {
        return ImplementationRestriction("fx*", "arithmetic overflow", a, b);
      }
    }

    [Builtin("fx-")]
    public static object FxMinus(object a)
    {
      int x1 = RequiresNotNull<int>(a);

      // this try is dumb, only 1 case i think
      try
      {
        return checked(-x1);
      }
      catch (OverflowException)
      {
        return ImplementationRestriction("fx-", "arithmetic overflow", a);
      }
    }

    [Builtin("fx-")]
    public static object FxMinus(object a, object b)
    {
      int x1 = RequiresNotNull<int>(a);
      int x2 = RequiresNotNull<int>(b);

      try
      {
        return checked(x1 - x2);
      }
      catch (OverflowException)
      {
        return ImplementationRestriction("fx-", "arithmetic overflow", a, b);
      }
    }


    //(fxreverse-bit-field fx1 fx2 fx3)
    [Builtin("fxreverse-bit-field")]
    public static object FxReverseBitField(object fx1, object fx2, object fx3)
    {
      int i1 = RequiresNotNull<int>(fx1);
      int i2 = RequiresNotNull<int>(fx2);
      int i3 = RequiresNotNull<int>(fx3);

      if (i2 >= i3)
      {
        return AssertionViolation("fxreverse-bit-field", "start must be less than end", fx2, fx3);
      }

      BitVector32 bitvec = new BitVector32(i1);

      int range = i3 - i2 - 1;

      for (int i = i2; i < (i3 - range/2); i++)
      {
        int m1 = BitVector32.CreateMask(i);
        int m2 = BitVector32.CreateMask(i3 - (i - i2));
        bool b1 = bitvec[m1];
        bool b2 = bitvec[m2];
        bitvec[m1] = b2;
        bitvec[m2] = b1;
      }

      return bitvec.Data;
    }

  }
}
