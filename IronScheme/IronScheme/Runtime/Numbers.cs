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
using Microsoft.Scripting.Math;
using System.Reflection;
using Microsoft.Scripting.Utils;
using System.ComponentModel;
using System.Text.RegularExpressions;
using System.Diagnostics;
using IronScheme.Compiler.Numbers;
using System.Globalization;
using Microsoft.Scripting;

namespace IronScheme.Runtime
{
  public partial class Builtins
  {
    [Builtin("decompose-flonum")]
    public static object DecomposeFlonum(object flonum)
    {
      double d = (double)flonum;

      BigInteger mantissa;
      BigInteger exponent;
      bool res = d.GetMantissaAndExponent(out mantissa, out exponent);

      if (res)
      {
        return Values(flonum, mantissa, exponent);
      }
      else
      {
        return FALSE;
      }
    }


    [Builtin("string->number", AllowConstantFold = true)]
    public static object StringToNumber(object obj)
    {
      string str = RequiresNotNull<string>(obj);

      if (str.Length == 0)
      {
        return FALSE;
      }

      Parser number_parser = new Parser();
      Scanner number_scanner = new Scanner();

      number_parser.scanner = number_scanner;
      number_scanner.SetSource(str,0);
      number_parser.result = null;
      number_scanner.yy_push_state(3);

      try
      {
        if (number_parser.Parse())
        {
          Debug.Assert(number_parser.result != null);
          return number_parser.result;
        }
        else
        {
          return FALSE;
        }
      }
      catch
      {
        return FALSE;
      }
    }

    [Builtin("string->number", AllowConstantFold = true)]
    public static object StringToNumber(object obj, object radix)
    {
      string str = RequiresNotNull<string>(obj);
      radix = radix ?? 10;
      int r = (int)radix;

      if (str.Length == 0)
      {
        return FALSE;
      }

      switch (r)
      {
        case 2:
          return StringToNumber("#b" + str);
        case 8:
          return StringToNumber("#o" + str);
        case 10:
          return StringToNumber(str);
        case 16:
          return StringToNumber("#x" + str);
        default:
          return FALSE;

      }
    }

    [Builtin("inexact=?", AllowConstantFold = true)]
    public static object InexactEqual(object a, object b)
    {
      return GetBool(ConvertToComplex(a) == ConvertToComplex(b));
    }

    [Builtin("exact-compare", AllowConstantFold = true)]
    public static object ExactCompare(object a, object b)
    {
      NumberClass f = GetNumberClass(a);
      NumberClass s = GetNumberClass(b);

      NumberClass effective = f & s;

      switch (effective)
      {
        case NumberClass.Integer:
          return RuntimeHelpers.Int32ToObject( ((int)a).CompareTo((int)b));
        case NumberClass.BigInteger:
          return RuntimeHelpers.Int32ToObject( ConvertToBigInteger(a).CompareTo(ConvertToBigInteger(b)));
        case NumberClass.Rational:
          return RuntimeHelpers.Int32ToObject( ConvertToRational(a).CompareTo(ConvertToRational(b)));

        default:
          return AssertionViolation("exact-compare", "not exact", a, b);
      }
    }

    [Builtin("inexact-compare", AllowConstantFold = true)]
    public static object InexactCompare(object a, object b)
    {
      NumberClass f = GetNumberClass(a);
      NumberClass s = GetNumberClass(b);

      NumberClass effective = f & s;

      switch (effective)
      {
        case NumberClass.Real:
          return RuntimeHelpers.Int32ToObject( ((double)a).CompareTo(b));
        default:
          return AssertionViolation("inexact-compare", "not a real", a, b);
      }
    }

    internal static bool IsExact(object obj)
    {
      return obj is int || 
             obj is BigInteger || 
             obj is Fraction || 
             obj is ComplexFraction;
    }



    #region math

    enum NumberClass
    {
      Complex = 1,
      Real = 2 | Complex,
      Rational = 4 | Real ,
      BigInteger = 8 | Rational,
      Integer = 16 | BigInteger,
      NotANumber = 0
    }

    static NumberClass GetNumberClass(object obj)
    {
      if (obj is int)
      {
        return NumberClass.Integer;
      }
      else if (obj is BigInteger)
      {
        return NumberClass.BigInteger;
      }
      else if (obj is Fraction)
      {
        return NumberClass.Rational;
      }
      else if (obj is double)
      {
        return NumberClass.Real;
      }
      else if (obj is Complex64 || obj is ComplexFraction)
      {
        return NumberClass.Complex;
      }
      else
      {
        return NumberClass.NotANumber;
      }
    }

    protected static int ConvertToInteger(object o)
    {
      if (o is int)
      {
        return (int)o;
      }
      throw new NotSupportedException("number type not supported");
    }

    protected internal static BigInteger ConvertToBigInteger(object o)
    {
      if (o is int)
      {
        return (int)o;
      }
      if (o is BigInteger)
      {
        return (BigInteger)o;
      }
      throw new NotSupportedException("number type not supported");
    }

    static Fraction ConvertToRational(object o)
    {
      if (o is int)
      {
        return (int)o;
      }
      if (o is Fraction)
      {
        return (Fraction)o;
      }
      if (o is BigInteger)
      {
        return new Fraction((BigInteger)o, 1);
      }
      if (o is double)
      {
        return (Fraction)(double)o;
      }
      throw new NotSupportedException("number type not supported");
    }

    protected static double ConvertToReal(object o)
    {
      return SafeConvert(o);
    }

    protected static Complex64 ConvertToComplex(object o)
    {
      if (o is Complex64)
      {
        return (Complex64)o;
      }
      else if (o is ComplexFraction)
      {
        return (ComplexFraction)o;
      }
      else
      {
        return Complex64.MakeReal(ConvertToReal(o));
      }
    }

    [Builtin("generic+", AllowConstantFold = true)]
    public static object Add(object first, object second)
    {
      NumberClass f = GetNumberClass(first);

      if (f == NumberClass.NotANumber)
      {
        throw new NotSupportedException("number type not supported");
      }
      
      NumberClass s = GetNumberClass(second);

      if (s == NumberClass.NotANumber)
      {
        throw new NotSupportedException("number type not supported");
      }

      NumberClass effective = f & s;

      switch (effective)
      {
        case NumberClass.Integer:
          if (avoidoverflow || overflowcount > 10)
          {
            goto case NumberClass.BigInteger;
          }
          try
          {
            return RuntimeHelpers.Int32ToObject(checked(ConvertToInteger(first) + ConvertToInteger(second)));
          }
          catch (OverflowException)
          {
            overflowcount++;
            avoidoverflow = true;
            return ConvertToBigInteger(first) + ConvertToBigInteger(second);
          }
        case NumberClass.BigInteger:
          return ToIntegerIfPossible(ConvertToBigInteger(first) + ConvertToBigInteger(second));
        case NumberClass.Rational:
          return IntegerIfPossible(ConvertToRational(first) + ConvertToRational(second));
        case NumberClass.Real:
          return ConvertToReal(first) + ConvertToReal(second);
        case NumberClass.Complex:
          return ConvertToComplex(first) + ConvertToComplex(second);
      }

      throw new NotSupportedException("number type not supported");
    }

    [Builtin("generic-", AllowConstantFold = true)]
    public static object Subtract(object first, object second)
    {
      NumberClass f = GetNumberClass(first);

      if (f == NumberClass.NotANumber)
      {
        throw new NotSupportedException("number type not supported");
      }

      NumberClass s = GetNumberClass(second);

      if (s == NumberClass.NotANumber)
      {
        throw new NotSupportedException("number type not supported");
      }

      NumberClass effective = f & s;

      switch (effective)
      {
        case NumberClass.Integer:
          if (avoidoverflow || overflowcount > 25)
          {
            goto case NumberClass.BigInteger;
          }
          try
          {
            return RuntimeHelpers.Int32ToObject(checked(ConvertToInteger(first) - ConvertToInteger(second)));
          }
          catch (OverflowException)
          {
            overflowcount++;
            avoidoverflow = true;
            return ConvertToBigInteger(first) - ConvertToBigInteger(second);
          }
        case NumberClass.BigInteger:
          return ToIntegerIfPossible(ConvertToBigInteger(first) - ConvertToBigInteger(second));
        case NumberClass.Rational:
          return IntegerIfPossible(ConvertToRational(first) - ConvertToRational(second));
        case NumberClass.Real:
          return ConvertToReal(first) - ConvertToReal(second);
        case NumberClass.Complex:
          return ConvertToComplex(first) - ConvertToComplex(second);
      }

      throw new NotSupportedException("number type not supported");
    }

    static bool avoidoverflow = false;
    static int overflowcount = 0;

    [Builtin("generic*", AllowConstantFold = true)]
    public static object Multiply(object first, object second)
    {
      NumberClass f = GetNumberClass(first);

      if (f == NumberClass.NotANumber)
      {
        throw new NotSupportedException("number type not supported");
      }

      NumberClass s = GetNumberClass(second);

      if (s == NumberClass.NotANumber)
      {
        throw new NotSupportedException("number type not supported");
      }

      NumberClass effective = f & s;

      switch (effective)
      {
        case NumberClass.Integer:
          if (avoidoverflow || overflowcount > 25)
          {
            goto case NumberClass.BigInteger;
          }
          try
          {
            return RuntimeHelpers.Int32ToObject(checked(ConvertToInteger(first) * ConvertToInteger(second)));
          }
          catch (OverflowException)
          {
            overflowcount++;
            avoidoverflow = true;
            return ConvertToBigInteger(first) * ConvertToBigInteger(second);
          }
        case NumberClass.BigInteger:
          return ToIntegerIfPossible(ConvertToBigInteger(first) * ConvertToBigInteger(second));
        case NumberClass.Rational:
          return IntegerIfPossible(ConvertToRational(first) * ConvertToRational(second));
        case NumberClass.Real:
          return ConvertToReal(first) * ConvertToReal(second);
        case NumberClass.Complex:
          return ConvertToComplex(first) * ConvertToComplex(second);
      }

      throw new NotSupportedException("number type not supported");
    }

    // improve this
    protected internal static object ToIntegerIfPossible(BigInteger i)
    {
      if (i <= int.MaxValue && i >= int.MinValue)
      {
        avoidoverflow = false;
        return RuntimeHelpers.Int32ToObject((int)i);
      }
      else
      {
        return i;
      }
    }

    [Builtin("generic/", AllowConstantFold = true)]
    public static object Divide(object first, object second)
    {
      NumberClass f = GetNumberClass(first);

      if (f == NumberClass.NotANumber)
      {
        throw new NotSupportedException("number type not supported");
      }

      NumberClass s = GetNumberClass(second);

      if (s == NumberClass.NotANumber)
      {
        throw new NotSupportedException("number type not supported");
      }

      NumberClass effective = f & s;

      switch (effective)
      {
        case NumberClass.Integer:
        case NumberClass.BigInteger:
          return IntegerIfPossible(new Fraction(ConvertToBigInteger(first), ConvertToBigInteger(second)));
        case NumberClass.Rational:
          return IntegerIfPossible(ConvertToRational(first) / ConvertToRational(second));
        case NumberClass.Real:
          return ConvertToReal(first) / ConvertToReal(second);
        case NumberClass.Complex:
          return ConvertToComplex(first) / ConvertToComplex(second);
      }

      throw new NotSupportedException("number type not supported");
    }

    #endregion



    static double SafeConvert(object obj)
    {
      try
      {
        if (obj is Complex64)
        {
          Complex64 c = (Complex64)obj;
          if (c.Imag == 0.0)
          {
            return c.Real;
          }
          else
          {
            return (double)AssertionViolation(GetCaller(), "no conversion to real possible", obj);
          }
        }
        return Convert.ToDouble(obj, CultureInfo.InvariantCulture);
      }
      catch (OverflowException)
      {
        var i = (BigInteger)obj;
        return i < 0 ? double.NegativeInfinity : double.PositiveInfinity;
        //return IsTrue(IsPositive(obj)) ? double.PositiveInfinity : double.NegativeInfinity;
      }
    }

    //based on lsqrt()
    [Builtin("bignum-sqrt", AllowConstantFold = true)]
    public static object SqrtBigInteger(object num)
    {
      BigInteger x = (BigInteger)num;
      BigInteger v0, q0, x1;

      if (x <= 1)
      {
        return x;
      }

      v0 = x;
      x = x / 2;
      while (true)
      {
        q0 = v0 / x;
        x1 = (x + q0) / 2;
        if (q0 >= x)
          break;
        x = x1;
      }
      if (x1 * x1 != v0)
      {
        return Math.Sqrt(v0);
      }
      return x1;
    }

    [Builtin("bignum-sqrt-exact", AllowConstantFold = true)]
    public static object ExactSqrtBigInteger(object num)
    {
      BigInteger x = (BigInteger)num;
      BigInteger v0, q0, x1;

      if (x <= 1)
      {
        return x;
      }

      v0 = x;
      x = x / 2;
      while (true)
      {
        q0 = v0 / x;
        x1 = (x + q0) / 2;
        if (q0 >= x)
          break;
        x = x1;
      }
      q0 = x1 * x1;

      if (q0 > v0)
      {
        x1 = x1 - 1;
        q0 = x1 * x1;
      }
      return Values(x1, v0 - q0);
    }


    // improve actually
    static object IntegerIfPossible(object res)
    {
      if (res is BigInteger)
      {
        return ToIntegerIfPossible((BigInteger)res);
      }
      else if (res is Fraction)
      {
        Fraction f = (Fraction)res;
        if (f.Denominator == 1)
        {
          return ToIntegerIfPossible(f.Numerator);
        }
      }
      else if (res is ComplexFraction)
      {
        ComplexFraction cf = (ComplexFraction)res;
        if (cf.Imag == 0)
        {
          return IntegerIfPossible(cf.Real);
        }
      }
      return res;
    }

    #region Other Obsolete

 
  
    [Builtin("inexact")]
    [Obsolete("Implemented in Scheme, do not use, remove if possible", false)]
    internal static object Inexact(object obj)
    {
      if (IsExact(obj))
      {
        return SafeConvert(obj);
      }
      return obj;
    }

    [Builtin("exact")]
    [Obsolete("Implemented in Scheme, do not use, remove if possible", false)]
    internal static object Exact(object obj)
    {
      if (obj is double)
      {
        double d = (double)obj;

        if (double.IsNaN(d) || double.IsInfinity(d))
        {
          return AssertionViolation("exact", "no exact equivalent", obj);
        }
        return Exact((Fraction)d);
      }
      if (obj is Complex64)
      {
        Complex64 c = (Complex64)obj;
        if (double.IsNaN(c.Real) || double.IsNaN(c.Imag) ||
            double.IsInfinity(c.Real) || double.IsInfinity(c.Imag))
        {
          return AssertionViolation("exact", "no exact equivalent", obj);
        }
        else
        {
          return new ComplexFraction((Fraction)c.Real, (Fraction)c.Imag);
        }
      }
      if (obj is long)
      {
        Debugger.Break();
        BigInteger r = (BigInteger)(long)obj;
        int ir;
        if (r.AsInt32(out ir))
        {
          return ir;
        }
        return r;
      }
      if (obj is Fraction)
      {
        Fraction f = (Fraction)obj;
        if (f.Denominator == 1)
        {
          if (f.Numerator > int.MaxValue || f.Numerator < int.MinValue)
          {
            return (BigInteger)f.Numerator;
          }
          return RuntimeHelpers.Int32ToObject((int)f.Numerator);
        }
        return f;
      }
      return obj;
    }

 
    #endregion
  }
}
