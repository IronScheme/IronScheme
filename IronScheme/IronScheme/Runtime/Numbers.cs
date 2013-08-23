#region License
/* Copyright (c) 2007-2013 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Diagnostics;
using System.Globalization;
using IronScheme.Compiler.Numbers;
using Microsoft.Scripting;
using Microsoft.Scripting.Math;
using BigInteger = Oyster.Math.IntX;


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
      number_scanner.SetSource(str, 0);
      number_parser.result = null;
      number_scanner.yy_push_state(3);

      try
      {
        CallTarget0 disp = delegate
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
        };
        CallTarget1 handler = delegate(object o)
        {
          throw new Continuation();
        };
        return Runtime.R6RS.Exceptions.WithExceptionHandler(
                          Closure.Create(handler),
                          Closure.Create(disp));
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
      int r = RequiresNotNull<int>(radix);

      if (str.Length == 0)
      {
        return FALSE;
      }

      string prefix = "";

      if (str.Length > 1)
      {
        prefix = str.Substring(0, 2);
      }

      switch (prefix)
      {
        case "#b":
        case "#o":
        case "#x":
          return StringToNumber(str);
        default:
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
    }

    [Builtin("inexact=?", AllowConstantFold = true)]
    public static object InexactEqual(object a, object b)
    {
      return GetBool(ConvertToComplex(a) == ConvertToComplex(b));
    }

    public static int ExactCompare(object a, object b)
    {
      NumberClass f = GetNumberClass(a);
      NumberClass s = GetNumberClass(b);

      NumberClass effective = f & s;

      switch (effective)
      {
        case NumberClass.Integer:
          return ((int)a).CompareTo((int)b);
        case NumberClass.BigInteger:
          return ConvertToBigInteger(a).CompareTo(ConvertToBigInteger(b));
        case NumberClass.Rational:
          return ConvertToRational(a).CompareTo(ConvertToRational(b));

        default:
          AssertionViolation("exact-compare", "not exact", a, b);
          // never reached
          return 0;
      }
    }

    public static int InexactCompare(object a, object b)
    {
      NumberClass f = GetNumberClass(a);
      NumberClass s = GetNumberClass(b);

      NumberClass effective = f & s;

      switch (effective)
      {
        case NumberClass.Real:
          return ((double)a).CompareTo(b);
        default:
          AssertionViolation("inexact-compare", "not a real", a, b);
          // never reached
          return 0;
      }
    }

    static bool IsExact(object obj)
    {
      return obj is int ||
             obj is BigInteger ||
             obj is Fraction ||
             obj is ComplexFraction;
    }

    #region math

    [Builtin("fx+internal", AllowConstantFold = true)]
    public static object FxPlusInternal(int a, int b)
    {
      long r = (long)a + b;
      if (r > int.MaxValue || r < int.MinValue)
      {
        return FALSE;
      }
      return RuntimeHelpers.Int32ToObject((int)r);
    }

    [Builtin("fx-internal", AllowConstantFold = true)]
    public static object FxMinusInternal(int a, int b)
    {
      long r = (long)a - b;
      if (r > int.MaxValue || r < int.MinValue)
      {
        return FALSE;
      }
      return RuntimeHelpers.Int32ToObject((int)r);
    }

    [Builtin("fx*internal", AllowConstantFold = true)]
    public static object FxMultiplyInternal(int a, int b)
    {
      long r = (long)a * b;
      if (r > int.MaxValue || r < int.MinValue)
      {
        return FALSE;
      }
      return RuntimeHelpers.Int32ToObject((int)r);
    }

    [Builtin("fxarithmetic-shift-left-internal", AllowConstantFold = true)]
    public static object FxShiftLeftInternal(int a, int b)
    {
      long r = (long)a << b;
      if (r > int.MaxValue || r < int.MinValue)
      {
        return FALSE;
      }
      return RuntimeHelpers.Int32ToObject((int)r);
    }

    enum NumberClass
    {
      Complex = 1,
      Real = 2 | Complex,
      Rational = 4 | Real,
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
      else if (obj is double)
      {
        return NumberClass.Real;
      }
      else if (obj is BigInteger)
      {
        return NumberClass.BigInteger;
      }
      else if (obj is Fraction)
      {
        return NumberClass.Rational;
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

    static int ConvertToInteger(object o)
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

    static double ConvertToReal(object o)
    {
      return SafeConvert(o);
    }

    static Complex64 ConvertToComplex(object o)
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

    static ComplexFraction ConvertToComplexFraction(object o)
    {
      if (o is ComplexFraction)
      {
        return (ComplexFraction)o;
      }
      else if (o is int)
      {
        return new ComplexFraction((int)o);
      }
      else if (o is BigInteger)
      {
        return new ComplexFraction((BigInteger)o);
      }
      else
      {
        return new ComplexFraction((Fraction)o);
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
          {
            long result = (long)ConvertToInteger(first) + ConvertToInteger(second);
            if (result > int.MaxValue || result < int.MinValue)
            {
              return (BigInteger)result;
            }
            else
            {
              return RuntimeHelpers.Int32ToObject((int)result);
            }
          }
        case NumberClass.BigInteger:
          return ToIntegerIfPossible(ConvertToBigInteger(first) + ConvertToBigInteger(second));
        case NumberClass.Rational:
          return IntegerIfPossible(ConvertToRational(first) + ConvertToRational(second));
        case NumberClass.Real:
          return ConvertToReal(first) + ConvertToReal(second);
        case NumberClass.Complex:
          if (IsExact(first) && IsExact(second))
          {
            return IntegerIfPossible(ConvertToComplexFraction(first) + ConvertToComplexFraction(second));
          }
          return DoubleIfPossible(ConvertToComplex(first) + ConvertToComplex(second));
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
          {
            long result = (long)ConvertToInteger(first) - ConvertToInteger(second);
            if (result > int.MaxValue || result < int.MinValue)
            {
              return (BigInteger)result;
            }
            else
            {
              return RuntimeHelpers.Int32ToObject((int)result);
            }
          }
        case NumberClass.BigInteger:
          return ToIntegerIfPossible(ConvertToBigInteger(first) - ConvertToBigInteger(second));
        case NumberClass.Rational:
          return IntegerIfPossible(ConvertToRational(first) - ConvertToRational(second));
        case NumberClass.Real:
          return ConvertToReal(first) - ConvertToReal(second);
        case NumberClass.Complex:
          if (IsExact(first) && IsExact(second))
          {
            return IntegerIfPossible(ConvertToComplexFraction(first) - ConvertToComplexFraction(second));
          }
          return DoubleIfPossible(ConvertToComplex(first) - ConvertToComplex(second));
      }

      throw new NotSupportedException("number type not supported");
    }

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
          {
            long result = (long)ConvertToInteger(first) * ConvertToInteger(second);
            if (result > int.MaxValue || result < int.MinValue)
            {
              return (BigInteger)result;
            }
            else
            {
              return RuntimeHelpers.Int32ToObject((int)result);
            }
          }
        case NumberClass.BigInteger:
          return ToIntegerIfPossible(ConvertToBigInteger(first) * ConvertToBigInteger(second));
        case NumberClass.Rational:
          return IntegerIfPossible(ConvertToRational(first) * ConvertToRational(second));
        case NumberClass.Real:
          return ConvertToReal(first) * ConvertToReal(second);
        case NumberClass.Complex:
          if (IsExact(first) && IsExact(second))
          {
            return IntegerIfPossible(ConvertToComplexFraction(first) * ConvertToComplexFraction(second));
          }
          return DoubleIfPossible(ConvertToComplex(first) * ConvertToComplex(second));
      }

      throw new NotSupportedException("number type not supported");
    }

    // improve this
    protected internal static object ToIntegerIfPossible(BigInteger i)
    {
      if (i <= int.MaxValue && i >= int.MinValue)
      {
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

      try
      {

        switch (effective)
        {
          case NumberClass.Integer:
          case NumberClass.BigInteger:
            return IntegerIfPossible(new Fraction(ConvertToBigInteger(first), ConvertToBigInteger(second)));
          case NumberClass.Rational:
            return IntegerIfPossible(ConvertToRational(first)/ConvertToRational(second));
          case NumberClass.Real:
            return ConvertToReal(first)/ConvertToReal(second);
          case NumberClass.Complex:
            if (IsExact(first) && IsExact(second))
            {
              return IntegerIfPossible(ConvertToComplexFraction(first)/ConvertToComplexFraction(second));
            }
            return DoubleIfPossible(ConvertToComplex(first)/ConvertToComplex(second));
        }
      }
      catch (DivideByZeroException)
      {
        return AssertionViolation("/", "divide by zero", first);
      }

      throw new NotSupportedException("number type not supported");
    }

    #endregion



    static double SafeConvert(object obj)
    {
      try
      {
        if (obj is int)
        {
          return (int)obj;
        }
        if (obj is double)
        {
          return (double)obj;
        }
        if (obj is BigInteger)
        {
          return ((BigInteger)obj).ToFloat64();
        }
        if (obj is Fraction)
        {
          return ((Fraction)obj).ToDouble(null);
        }
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
        return Math.Sqrt(v0.ToFloat64());
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
      return Values(ToIntegerIfPossible(x1), ToIntegerIfPossible(v0 - q0));
    }

    static object DoubleIfPossible(object res)
    {
      if (res is Complex64)
      {
        var c = (Complex64)res;
        if (c.Imag == 0)
        {
          return c.Real;
        }
      }
      return res;
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
  }
}
