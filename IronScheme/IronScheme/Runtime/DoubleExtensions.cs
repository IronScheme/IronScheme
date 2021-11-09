#region License
/* Copyright (c) 2007-2016 Llewellyn Pritchard
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using BigInteger = Oyster.Math.IntX;

namespace System.Runtime.CompilerServices
{
  public class ExtensionAttribute : Attribute
  {
  }
}

namespace IronScheme.Runtime
{
  static class DoubleExtensions
  {
    const long MANTISSA_MASK = (long)(ulong.MaxValue >> (64 - 52));
    const long EXPONENT_MASK = (long)(ulong.MaxValue >> (64 - 11));
    const long SIGN_MASK = 0x1;

    public static long GetMantissa(this double d)
    {
      var r = BitConverter.DoubleToInt64Bits(d);
      return GetMantissa(r);
    }

    static long GetMantissa(long r)
    {
      return r & MANTISSA_MASK;
    }

    public static int GetExponent(this double d)
    {
      var r = BitConverter.DoubleToInt64Bits(d);
      return GetExponent(r);
    }

    static int GetExponent(long r)
    {
      r >>= 52;
      return (int)(r & EXPONENT_MASK);
    }

    public static int GetSign(this double d)
    {
      var r = BitConverter.DoubleToInt64Bits(d);
      return GetSign(r);
    }

    static int GetSign(long r)
    {
      r >>= 63;
      return (int)(r & SIGN_MASK);
    }

    readonly static BigInteger MANTISSA = (ulong.MaxValue >> (64 - 52)) + 1;

    public static bool GetMantissaAndExponent(this double d, out BigInteger mantissa, out BigInteger exponent)
    {
      if (double.IsNaN(d))
      {
        mantissa = exponent = 0;
        return false;
      }

      if (double.IsNegativeInfinity(d))
      {
        mantissa = -1;
        exponent = 0;
        return false;
      }

      if (double.IsPositiveInfinity(d))
      {
        mantissa = 1;
        exponent = 0;
        return false;
      }

      var r = BitConverter.DoubleToInt64Bits(d);

      var man = GetMantissa(r);
      var exp = GetExponent(r);

      const int BIAS = 1075;
      if (exp == 0)
      {
        exponent = exp - BIAS;
        mantissa = (man << 1);
      }
      else
      {
        exponent = exp - BIAS;
        mantissa = man + MANTISSA;
      }

      return true;
    }

    public static bool GetComponents(this double d, out BigInteger numerator, out BigInteger denominator)
    {
      if (double.IsNaN(d))
      {
        numerator = denominator = 0;
        return false;
      }

      if (double.IsNegativeInfinity(d))
      {
        numerator = -1;
        denominator = 0;
        return false;
      }

      if (double.IsPositiveInfinity(d))
      {
        numerator = 1;
        denominator = 0;
        return false;
      }

      if (d == 0.0)
      {
        numerator = 0;
        denominator = 1;
        return true;
      }

      var r = BitConverter.DoubleToInt64Bits(d);

      var m = GetMantissa(r);
      var e = GetExponent(r);
      var s = GetSign(r) == 0 ? 1 : -1;

      const int BIAS = 1023;
      var re = e - BIAS;

      var exp = (((BigInteger)1) << Math.Abs(re));

      if (e == 0)
      {
        denominator = s * exp * (MANTISSA >> 1);
        numerator = m;
        return true;
      }

      if (re < 0)
      {
        denominator = MANTISSA * s * exp;
        numerator = (MANTISSA + m);
      }
      else
      {
        denominator = MANTISSA * s;
        numerator = exp * (MANTISSA + m);
      }
      
      return true;
    }

  }
}
