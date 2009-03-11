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
using IronScheme.Compiler;
using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Utils;
using Microsoft.Scripting;
using System.Globalization;

namespace IronScheme.Runtime.R6RS.Arithmetic
{


  public class Flonums : Builtins
  {
    [Builtin("real->flonum")]
    [Obsolete("Implemented in Scheme, do not use, remove if possible")]
    public static object RealToFlonum(object n)
    {
      // must be number? fixme
      return Convert.ToDouble(n, CultureInfo.InvariantCulture);
    }

    //(flnumerator fl) procedure
    [Builtin("flnumerator")]
    [Obsolete("Implemented in Scheme, do not use, remove if possible")]
    public static object FlNumerator(object a)
    {
      if (IsTrue(IsNan(a)) || IsTrue(IsInfinite(a)))
      {
        return a;
      }
      return Convert.ToDouble((((Fraction)RequiresNotNull<double>(a)).Numerator));
    }

    //(fldenominator fl)
    [Builtin("fldenominator")]
    [Obsolete("Implemented in Scheme, do not use, remove if possible")]
    public static object FlDenominator(object a)
    {
      if (IsTrue(IsNan(a)) || IsTrue(IsInfinite(a)))
      {
        return 1.0;
      }
      return Convert.ToDouble((((Fraction)RequiresNotNull<double>(a)).Denominator));
    }


    //(fixnum->flonum fx)
    [Builtin("fixnum->flonum")]
    [Obsolete("Implemented in Scheme, do not use, remove if possible")]
    public static object FixnumToFlonum(object a)
    {
      return (double)RequiresNotNull<int>(a);
    }

  }
}
