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

namespace IronScheme.Runtime
{
  public partial class Builtins
  {
    [Builtin("number->string")]
    public static object NumberToString(object obj)
    {
      return NumberToString(obj, 10);
    }

    static object PrintBinary(object num)
    {
      if (num is Fraction)
      {
        Fraction f = (Fraction)num;

        return string.Format("{0}/{1}", PrintBinary(f.Numerator), PrintBinary(f.Denominator));
      }
      else
      {
        BigInteger n = ConvertToBigInteger(num);
        bool positive = n >= 0;

        n = n.Abs();

        StringBuilder sb = new StringBuilder();

        do
        {
          sb.Append(((n & 1) == 1) ? "1" : "0");
          n >>= 1;
        }
        while (n != 0);

        char[] output = new char[sb.Length];

        for (int i = 0; i < sb.Length; i++)
        {
          output[output.Length - i - 1] = sb[i];
        }

        string ret = new string(output);

        if (positive)
        {
          return ret;
        }
        else
        {
          return "-" + ret;
        }
      }
    }

    static object PrintOctal(object num)
    {
      if (num is Fraction)
      {
        Fraction f = (Fraction)num;

        return string.Format("{0}/{1}", PrintOctal(f.Numerator), PrintOctal(f.Denominator));
      }
      else
      {
        BigInteger n = ConvertToBigInteger(num);
        bool positive = n >= 0;

        n = n.Abs();

        StringBuilder sb = new StringBuilder();

        do
        {
          sb.Append((char)((n & 7) + '0'));
          n /= 8;
        }
        while (n != 0);

        char[] output = new char[sb.Length];

        for (int i = 0; i < sb.Length; i++)
        {
          output[output.Length - i - 1] = sb[i];
        }

        string ret = new string(output);

        if (positive)
        {
          return ret;
        }
        else
        {
          return "-" + ret;
        }
      }
    }

    [Builtin("number->string")]
    public static object NumberToString(object obj, object radix, object precision)
    {
      return NumberToString(obj, radix);
    }
    
    [Builtin("number->string")]
    public static object NumberToString(object obj, object radix)
    {
      if (!IsTrue(IsNumber(obj)))
      {
        return AssertionViolation("number->string", "not a number", obj);
      }
      radix = radix ?? 10;
      int r = (int)radix;

      switch (r)
      {
        case 2:
          return PrintBinary(obj);
        case 8:
          return PrintOctal(obj);
        case 10:
          if (obj is double)
          {
            double d = (double)obj;
            if (Math.IEEERemainder(d, 1) == 0)
            {
              string rr = string.Format("{0:r}", obj).ToLower();
              if (rr.Contains("e"))
              {
                return rr;
              }
              else
              {
                return rr + ".0";
              }
            }
            else
            {
              if (double.IsNegativeInfinity(d))
              {
                return "-inf.0";
              }
              else if (double.IsPositiveInfinity(d))
              {
                return "+inf.0";
              }
              else if (double.IsNaN(d))
              {
                return "+nan.0";
              }
              else
              return string.Format("{0:r}", obj).ToLower();
            }
          }
          if (obj is Complex64)
          {
            Complex64 c = (Complex64)obj;
            double a = (double)Angle(c);
            object mag = Magnitude(c);

            double m = mag is int ? (int)mag : (double)mag;

            if (IsTrue(IsIntegerValued(a * 1000)) || IsTrue(IsIntegerValued(m * 1000)) 
              && !IsTrue(IsIntegerValued(c.Real * 1000)) && !IsTrue(IsIntegerValued(c.Imag * 1000)))
            {
              return string.Format("{0}@{1}", NumberToString(m), NumberToString(a));
            }
            else
            {
              return string.Format("{0}{2}{1}i", NumberToString(c.Real), NumberToString(Abs(c.Imag)), c.Imag > 0 ? "+" : "-");
            }
          }
          return obj.ToString();
        case 16:
          if (IsTrue(IsNegative(obj)))
          {
            return string.Format("-{0:X}", Abs(obj));
          }
          else
          {
            return string.Format("{0:X}", obj);
          }
      }

      return FALSE;
    }




    [Builtin("string->number")]
    public static object StringToNumber(object obj)
    {
      string str = RequiresNotNull<string>(obj);

      if (str.Length == 0)
      {
        return AssertionViolation("string->number", "cannot convert empty string to a number", obj);
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

    [Builtin("string->number")]
    public static object StringToNumber(object obj, object radix)
    {
      string str = RequiresNotNull<string>(obj);
      radix = radix ?? 10;
      int r = (int)radix;

      if (str.Length == 0)
      {
        return AssertionViolation("string->number", "cannot convert empty string to a number", obj);
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
    
    [Builtin("number?")]
    public static object IsNumber(object obj)
    {
      return IsComplex(obj);
    }

    [Builtin("complex?")]
    public static object IsComplex(object obj)
    {
      return GetBool(IsTrue(IsReal(obj)) || obj is Complex64);
    }

    [Builtin("real?")]
    public static object IsReal(object obj)
    {
      return GetBool(IsTrue(IsRational(obj)) || obj is double);
    }

    [Builtin("rational?")]
    public static object IsRational(object obj)
    {
      return GetBool(IsTrue(IsInteger(obj)) || obj is Fraction);
    }

    [Builtin("integer?")]
    public static object IsInteger(object obj)
    {
      return GetBool(obj is int || obj is BigInteger);
    }
    
    [Builtin("integer-valued?")]
    public static object IsIntegerValued(object obj)
    {
      if (obj is int || obj is BigInteger)
      {
        return TRUE;
      }
      if (obj is Complex64)
      {
        return GetBool(((Complex64)obj).Imag == 0);
      }
      if (obj is Fraction)
      {
        return GetBool(((Fraction)obj).Denominator == 1);
      }
      if (IsTrue(IsNan(obj)) || IsTrue(IsInfinite(obj)))
      {
        return FALSE;
      }
      return IsZero(Mod(obj, 1));
    }

    [Builtin("rational-valued?")]
    public static object IsRationalValued(object obj)
    {
      if (obj is Fraction)
      {
        return TRUE;
      }

      bool iv = IsTrue(IsIntegerValued(obj));
      if (iv)
      {
        return TRUE;
      }

      if (obj is Complex64)
      {
        return FALSE;
      }

      if (IsTrue(IsNumber(obj)))
      {
        double d = SafeConvert(obj);
        return GetBool(d == (double)(Fraction)d);
      }
      return FALSE;
    }

    [Builtin("real-valued?")]
    public static object IsRealValued(object obj)
    {
      if (obj is Complex64)
      {
        Complex64 c = (Complex64)obj;
        if (c.Imag != 0)
        {
          return FALSE;
        }
      }
      return IsNumber(obj);
    }

    [Builtin("finite?")]
    public static object IsFinite(object obj)
    {
      return Not(IsInfinite(obj));
    }

    [Builtin("infinite?")]
    public static object IsInfinite(object obj)
    {
      if (obj is double)
      {
        return GetBool(double.IsInfinity((double)obj));
      }
      if (obj is float)
      {
        return GetBool(float.IsInfinity((float)obj));
      }

      return FALSE;
    }

    [Builtin("nan?")]
    public static object IsNan(object obj)
    {
      if (obj is double)
      {
        return GetBool(double.IsNaN((double)obj));
      }
      if (obj is float)
      {
        return GetBool(float.IsNaN((float)obj));
      }

      return FALSE;
    }

    [Builtin("inexact")]
    [Builtin("exact->inexact")]
    public static object Inexact(object obj)
    {
      if (!IsTrue(IsNumber(obj)))
      {
        return AssertionViolation("inexact", "not a number", obj);
      }

      if (IsTrue(IsExact(obj)))
      {
        return SafeConvert(obj);
      }
      return obj;
    }

    [Builtin("exact")]
    [Builtin("inexact->exact")]
    public static object Exact(object obj)
    {
      if (!IsTrue(IsNumber(obj)))
      {
        return AssertionViolation("exact", "not a number", obj);
      }
      if (IsTrue(IsInexact(obj)))
      {
        if (obj is double)
        {
          double d = (double)obj;

          if (double.IsNaN(d) || double.IsInfinity(d))
          {
            return AssertionViolation("exact", "no exact equivalent", obj);
          }
        }
        try
        {
          Fraction f = (Fraction)SafeConvert(obj);
          if (f.Denominator == 1)
          {
            if (f.Numerator > int.MaxValue || f.Numerator < int.MinValue)
            {
              return (BigInteger)f.Numerator;
            }
            return (int)f.Numerator;
          }
          return f;
        }
        catch (DivideByZeroException)
        {
          // fall back to bigint
        }
        catch (OverflowException)
        {
          // fall back to bigint
        }
        BigInteger r = (BigInteger)BigIntConverter.ConvertFrom(Round(obj));
        int ir;
        if (r.AsInt32(out ir))
        {
          return ir;
        }
        return r;
      }
      if (obj is long)
      {
        BigInteger r = (BigInteger)BigIntConverter.ConvertFrom(obj);
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
          return (int)f.Numerator;
        }
        return f;
      }
      return obj;
    }

    [Builtin("exact?")]
    public static object IsExact(object obj)
    {
      if (IsTrue(IsNumber(obj)))
      {
        return IsRational(obj);
      }
      return AssertionViolation("exact?", "not a number", obj);
    }

    [Builtin("inexact?")]
    public static object IsInexact(object obj)
    {
      if (IsTrue(IsNumber(obj)))
      {
        return Not(IsRational(obj));
      }
      return AssertionViolation("inexact?", "not a number", obj);
    }

    #region relations

    [Builtin("=")]
    public static object IsSame(object first, object second)
    {
      NumberClass f = GetNumberClass(first);

      if (f == NumberClass.NotANumber)
      {
        return AssertionViolation("=", "not a number", first);
      }

      NumberClass s = GetNumberClass(second);

      if (s == NumberClass.NotANumber)
      {
        return AssertionViolation("=", "not a number", second);
      }

      NumberClass effective = f & s;

      bool result = false;

      switch (effective)
      {
        case NumberClass.Integer:
          result = ConvertToInteger(first) == ConvertToInteger(second);
          break;
        case NumberClass.BigInteger:
          result = ConvertToBigInteger(first) == ConvertToBigInteger(second);
          break;
        case NumberClass.Rational:
          result = ConvertToRational(first) == ConvertToRational(second);
          break;
        case NumberClass.Real:
          double f1 = ConvertToReal(first);
          double f2 = ConvertToReal(second);
          result = f1 == f2 
            || (double.IsNegativeInfinity(f1) && double.IsNegativeInfinity(f2))
            || (double.IsPositiveInfinity(f1) && double.IsPositiveInfinity(f2));
          break;
        case NumberClass.Complex:
          result = ConvertToComplex(first) == ConvertToComplex(second);
          break;
        default:
          return AssertionViolation("=", "not a number", first, second);
      }

      return GetBool(result);
    }

    [Builtin("=")]
    public static object IsSame(object first, params object[] rest)
    {
      NumberClass e = GetNumberClass(first);
      if (e == NumberClass.NotANumber)
      {
        AssertionViolation("=", "not a number", first);
      }

      object o = first;

      foreach (object item in rest)
      {
        if (!IsTrue(IsSame(o, item)))
        {
          return FALSE;
        }
        o = item;
      }

      return TRUE;
    }

    [Builtin("<")]
    public static object IsLessThan(object first, object second)
    {
      NumberClass f = GetNumberClass(first);

      if (f == NumberClass.NotANumber)
      {
        return AssertionViolation("<", "not a number", first);
      }

      NumberClass s = GetNumberClass(second);

      if (s == NumberClass.NotANumber)
      {
        return AssertionViolation("<", "not a number", second);
      }

      NumberClass effective = f & s;

      bool result = false;

      switch (effective)
      {
        case NumberClass.Integer:
          result = ConvertToInteger(first) < ConvertToInteger(second);
          break;
        case NumberClass.BigInteger:
          result = ConvertToBigInteger(first) < ConvertToBigInteger(second);
          break;
        case NumberClass.Rational:
          result = ConvertToRational(first) < ConvertToRational(second);
          break;
        case NumberClass.Real:
          result = ConvertToReal(first) < ConvertToReal(second);
          break;
        case NumberClass.Complex:
          return AssertionViolation("<", "not real", first, second);
        default:
          return Error("<", "BUG");
      }

      return GetBool(result);
    }

    [Builtin("<")]
    public static object IsLessThan(object first, params object[] rest)
    {
      NumberClass e = GetNumberClass(first);
      if (e == NumberClass.Complex)
      {
        AssertionViolation("<", "not a real", first);
      }

      object o = first;

      foreach (object item in rest)
      {
        if (!IsTrue(IsLessThan(o, item)))
        {
          return FALSE;
        }
        o = item;
      }

      return TRUE;
    }

    [Builtin("<=")]
    public static object IsLessThanOrEqual(object first, object second)
    {
      NumberClass f = GetNumberClass(first);

      if (f == NumberClass.NotANumber)
      {
        return AssertionViolation("<=", "not a number", first);
      }

      NumberClass s = GetNumberClass(second);

      if (s == NumberClass.NotANumber)
      {
        return AssertionViolation("<=", "not a number", second);
      }

      NumberClass effective = f & s;

      bool result = false;

      switch (effective)
      {
        case NumberClass.Integer:
          result = ConvertToInteger(first) <= ConvertToInteger(second);
          break;
        case NumberClass.BigInteger:
          result = ConvertToBigInteger(first) <= ConvertToBigInteger(second);
          break;
        case NumberClass.Rational:
          result = ConvertToRational(first) <= ConvertToRational(second);
          break;
        case NumberClass.Real:
          result = ConvertToReal(first) <= ConvertToReal(second);
          break;
        case NumberClass.Complex:
          return AssertionViolation("<=", "not real", first, second);
        default:
          return Error("<=", "BUG");
      }

      return GetBool(result);
    }

    [Builtin("<=")]
    public static object IsLessThanOrEqual(object first, params object[] rest)
    {
      NumberClass e = GetNumberClass(first);
      if (e == NumberClass.Complex)
      {
        AssertionViolation("<=", "not a real", first);
      }

      object o = first;

      foreach (object item in rest)
      {
        if (!IsTrue(IsLessThanOrEqual(o, item)))
        {
          return FALSE;
        }
        o = item;
      }

      return TRUE;
    }

    [Builtin(">")]
    public static object IsGreaterThan(object first, object second)
    {
      NumberClass f = GetNumberClass(first);

      if (f == NumberClass.NotANumber)
      {
        return AssertionViolation(">", "not a number", first);
      }

      NumberClass s = GetNumberClass(second);

      if (s == NumberClass.NotANumber)
      {
        return AssertionViolation(">", "not a number", second);
      }

      NumberClass effective = f & s;

      bool result = false;

      switch (effective)
      {
        case NumberClass.Integer:
          result = ConvertToInteger(first) > ConvertToInteger(second);
          break;
        case NumberClass.BigInteger:
          result = ConvertToBigInteger(first) > ConvertToBigInteger(second);
          break;
        case NumberClass.Rational:
          result = ConvertToRational(first) > ConvertToRational(second);
          break;
        case NumberClass.Real:
          result = ConvertToReal(first) > ConvertToReal(second);
          break;
        case NumberClass.Complex:
          return AssertionViolation(">", "not real", first, second);
        default:
          return Error(">", "BUG");
      }

      return GetBool(result);
    }

    [Builtin(">")]
    public static object IsGreaterThan(object first, params object[] rest)
    {
      NumberClass e = GetNumberClass(first);
      if (e == NumberClass.Complex)
      {
        AssertionViolation(">", "not a real", first);
      }

      object o = first;

      foreach (object item in rest)
      {
        if (!IsTrue(IsGreaterThan(o, item)))
        {
          return FALSE;
        }
        o = item;
      }

      return TRUE;
    }

    [Builtin(">=")]
    public static object IsGreaterThanOrEqual(object first, object second)
    {
      NumberClass f = GetNumberClass(first);

      if (f == NumberClass.NotANumber)
      {
        return AssertionViolation(">=", "not a number", first);
      }

      NumberClass s = GetNumberClass(second);

      if (s == NumberClass.NotANumber)
      {
        return AssertionViolation(">=", "not a number", second);
      }

      NumberClass effective = f & s;

      bool result = false;

      switch (effective)
      {
        case NumberClass.Integer:
          result = ConvertToInteger(first) >= ConvertToInteger(second);
          break;
        case NumberClass.BigInteger:
          result = ConvertToBigInteger(first) >= ConvertToBigInteger(second);
          break;
        case NumberClass.Rational:
          result = ConvertToRational(first) >= ConvertToRational(second);
          break;
        case NumberClass.Real:
          result = ConvertToReal(first) >= ConvertToReal(second);
          break;
        case NumberClass.Complex:
          return AssertionViolation(">=", "not real", first, second);
        default:
          return Error(">=", "BUG");
      }

      return GetBool(result);
    }

    [Builtin(">=")]
    public static object IsGreaterThanOrEqual(object first, params object[] rest)
    {
      NumberClass e = GetNumberClass(first);
      if (e == NumberClass.Complex)
      {
        AssertionViolation(">=", "not a real", first);
      }

      object o = first;

      foreach (object item in rest)
      {
        if (!IsTrue(IsGreaterThanOrEqual(o, item)))
        {
          return FALSE;
        }
        o = item;
      }

      return TRUE;
    }


    #endregion

    [Builtin("zero?")]
    public static object IsZero(object obj)
    {
      return IsSame(obj, 0);
    }

    [Builtin("positive?")]
    public static object IsPositive(object obj)
    {
      if (IsTrue(IsRealValued(obj)))
      {
        return IsGreaterThan(obj, 0);
      }
      return AssertionViolation("positive?", "not a real", obj);

    }

    [Builtin("negative?")]
    public static object IsNegative(object obj)
    {
      if (IsTrue(IsRealValued(obj)))
      {
        return IsLessThan(obj, 0);
      }
      return AssertionViolation("negative?", "not a real", obj);
    }

    [Builtin("odd?")]
    public static object IsOdd(object obj)
    {
      if (IsTrue(IsIntegerValued(obj)))
      {
        return Not(IsEven(obj));
      }
      return AssertionViolation("odd?", "not an integer", obj);
    }

    [Builtin("even?")]
    public static object IsEven(object obj)
    {
      if (IsTrue(IsIntegerValued(obj)))
      {
        return IsZero(Mod(obj, 2));
      }
      return AssertionViolation("odd?", "not an integer", obj);
    }

    [Builtin("min")]
    public static object Min(object first, params object[] rest)
    {
      NumberClass e = GetNumberClass(first);
      if (e == NumberClass.Complex)
      {
        AssertionViolation("min", "not a real", first);
      }
      object min = first;
      foreach (object var in rest)
      {
        e &= GetNumberClass(var);
        if (IsTrue(IsLessThan(var, min)))
        {
          min = var;
        }
      }
      return GetNumber(e, min);
    }

    [Builtin("max")]
    public static object Max(object first, params object[] rest)
    {
      NumberClass e = GetNumberClass(first);

      if (e == NumberClass.Complex)
      {
        AssertionViolation("max", "not a real", first);
      }

      object max = first;

      foreach (object var in rest)
      {
        e &= GetNumberClass(var);
        if (IsTrue(IsGreaterThan(var, max)))
        {
          max = var;
        }
      }

      return GetNumber(e, max);
    }

    static object GetNumber(NumberClass nc, object number)
    {
      switch (nc)
      {
        case NumberClass.Integer:
        case NumberClass.BigInteger:
          BigInteger r = ConvertToBigInteger(number);
          if (r > int.MaxValue || r < int.MinValue)
          {
            return r;
          }
          else
          {
            return (int)r;
          }
        case NumberClass.Rational:
          return ConvertToRational(number);
        case NumberClass.Real:
          return ConvertToReal(number);
        case NumberClass.Complex:
          return ConvertToComplex(number);

      }

      throw new Exception("BUG");
    }

    #region math

    [Builtin("+")]
    public static object Add()
    {
      return 0;
    }

    [Builtin("+")]
    public static object Add(object first)
    {
      if (IsTrue(IsNumber(first)))
      {
        return first;
      }
      else
      {
        return AssertionViolation("+", "not a number", first);
      }
    }

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
      else if (obj is Complex64)
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
      return (int)AssertionViolation(GetCaller(), "not an integer", o);
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
      return (BigInteger) AssertionViolation(GetCaller(), "not a big integer", o);
    }

    static Fraction ConvertToRational(object o)
    {
      if (o is Fraction)
      {
        return (Fraction)o;
      }
      return (Fraction)FractionConverter.ConvertFrom(o);
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
      else
      {
        return Complex64.MakeReal(ConvertToReal(o));
      }
    }

    [Builtin("+")]
    public static object Add(object first, object second)
    {
      NumberClass f = GetNumberClass(first);

      if (f == NumberClass.NotANumber)
      {
        return AssertionViolation("+", "not a number", first);
      }
      
      NumberClass s = GetNumberClass(second);

      if (s == NumberClass.NotANumber)
      {
        return AssertionViolation("+", "not a number", second);
      }

      NumberClass effective = f & s;

      switch (effective)
      {
        case NumberClass.Integer:
          try
          {
            return checked(ConvertToInteger(first) + ConvertToInteger(second));
          }
          catch (OverflowException)
          {
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

      return Error("+", "BUG");
    }

    [Builtin("+")]
    public static object Add(object car, params object[] args)
    {
      for (int i = 0; i < args.Length; i++)
			{
        car = Add(car, args[i]); 
			}

      return car;
    }

    [Builtin("-")]
    public static object Subtract(object first)
    {
      if (first is int)
      {
        return -(int)first;
      }
      if (first is double)
      {
        return -(double)first;
      }
      return Subtract(0, first);
    }

    [Builtin("-")]
    public static object Subtract(object first, object second)
    {
      NumberClass f = GetNumberClass(first);

      if (f == NumberClass.NotANumber)
      {
        return AssertionViolation("-", "not a number", first);
      }

      NumberClass s = GetNumberClass(second);

      if (s == NumberClass.NotANumber)
      {
        return AssertionViolation("-", "not a number", second);
      }

      NumberClass effective = f & s;

      switch (effective)
      {
        case NumberClass.Integer:
          try
          {
            return checked(ConvertToInteger(first) - ConvertToInteger(second));
          }
          catch (OverflowException)
          {
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

      return Error("-", "BUG");
    }

    [Builtin("-")]
    public static object Subtract(object car, params object[] args)
    {
      for (int i = 0; i < args.Length; i++)
      {
        car = Subtract(car, args[i]);
      }

      return car;
    }

    [Builtin("*")]
    public static object Multiply()
    {
      return 1;
    }

    [Builtin("*")]
    public static object Multiply(object first)
    {
      if (IsTrue(IsNumber(first)))
      {
        return first;
      }
      else
      {
        return AssertionViolation("*", "not a number", first);
      }
    }

    [Builtin("*")]
    public static object Multiply(object first, object second)
    {
      NumberClass f = GetNumberClass(first);

      if (f == NumberClass.NotANumber)
      {
        return AssertionViolation("*", "not a number", first);
      }

      NumberClass s = GetNumberClass(second);

      if (s == NumberClass.NotANumber)
      {
        return AssertionViolation("*", "not a number", second);
      }

      NumberClass effective = f & s;

      switch (effective)
      {
        case NumberClass.Integer:
          try
          {
            return checked(ConvertToInteger(first) * ConvertToInteger(second));
          }
          catch (OverflowException)
          {
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

      return Error("*", "BUG");
    }

    protected static object ToIntegerIfPossible(BigInteger i)
    {
      if (i <= int.MaxValue && i >= int.MinValue)
      {
        return (int)i;
      }
      else
      {
        return i;
      }
    }

    [Builtin("*")]
    public static object Multiply(object car, params object[] args)
    {
      for (int i = 0; i < args.Length; i++)
      {
        car = Multiply(car, args[i]);
      }

      return car;
    }

    static object ConvertNumber(object result, Type type)
    {
      try
      {
        return Convert.ChangeType(result, type);
      }
      catch (OverflowException)
      {
        if (type == typeof(int) || type == typeof(long))
        {
          return BigIntConverter.ConvertFrom(result);
        }

        throw;
      }
    }

    static TypeConverter BigIntConverter = TypeDescriptor.GetConverter(typeof(BigInteger));

    [Builtin("/")]
    public static object Divide(object first)
    {
      return Divide(1, first);
    }

    [Builtin("/")]
    public static object Divide(object first, object second)
    {
      NumberClass f = GetNumberClass(first);

      if (f == NumberClass.NotANumber)
      {
        return AssertionViolation("/", "not a number", first);
      }

      NumberClass s = GetNumberClass(second);

      if (s == NumberClass.NotANumber)
      {
        return AssertionViolation("/", "not a number", second);
      }

      NumberClass effective = f & s;

      if (IsTrue(IsZero(first)) && IsTrue(IsZero(second)))
      {
        if (effective == NumberClass.BigInteger || effective == NumberClass.Integer)
        {
          return AssertionViolation("/", "divide by zero", first, second);
        }
        return double.NaN;
      }

      try
      {
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
      }
      catch (DivideByZeroException)
      {
        return AssertionViolation("/", "divide by zero", first, second);
      }

      return Error("/", "BUG");
    }

    [Builtin("/")]
    public static object Divide(object car, params object[] args)
    {
      for (int i = 0; i < args.Length; i++)
      {
        car = Divide(car, args[i]);
      }

      return car;
    }

    #endregion

    [Builtin("abs")]
    public static object Abs(object obj)
    {
      if (obj is double)
      {
        return Math.Abs((double)obj);
      }
      else if (obj is int)
      {
        int i = (int)obj;
        if (i == int.MinValue)
        {
          return ((BigInteger)i).Abs();
        }
        else
        {
          return Math.Abs(i);
        }
      }
      else if (obj is BigInteger)
      {
        return ((BigInteger)obj).Abs();
      }
      else if (obj is Complex64)
      {
        return AssertionViolation("abs", "not a real", obj);
      }
      else if (obj is Fraction)
      {
        Fraction f = (Fraction)obj;
        if (f < 0)
        {
          return new Fraction(-f.Numerator, f.Denominator);
        }
        return obj;
      }
      else
      {
        double d = SafeConvert(obj);
        return Math.Abs(d);
      }
    }

    
    static object RemainderInternal(object first, object second)
    {
      NumberClass f = GetNumberClass(first);

      if (f == NumberClass.NotANumber)
      {
        return AssertionViolation("RemainderInternal", "not a number", first);
      }

      NumberClass s = GetNumberClass(second);

      if (s == NumberClass.NotANumber)
      {
        return AssertionViolation("RemainderInternal", "not a number", second);
      }

      NumberClass effective = f & s;

      switch (effective)
      {
        case NumberClass.Integer:
          try
          {
            return checked(ConvertToInteger(first) % ConvertToInteger(second));
          }
          catch (OverflowException)
          {
            return ConvertToBigInteger(first) % ConvertToBigInteger(second);
          }
        case NumberClass.BigInteger:
          return ConvertToBigInteger(first) % ConvertToBigInteger(second);
        case NumberClass.Rational:
          return IntegerIfPossible(ConvertToRational(first) % ConvertToRational(second));
        case NumberClass.Real:
          return ConvertToReal(first) % ConvertToReal(second);
        case NumberClass.Complex:
          return ConvertToComplex(first) % ConvertToComplex(second);
      }

      return Error("RemainderInternal", "BUG");
    }

    static object Sign(object o)
    {
      if (IsTrue(IsNegative(o)))
      {
        return -1;
      }
      else
      {
        return 1;
      }
    }

    static bool BothPostiveOrNegative(object a, object b)
    {
      if ((IsTrue(IsPositive(a)) && IsTrue(IsPositive(b))) ||
          (IsTrue(IsNegative(a)) && IsTrue(IsNegative(b))))
      {
        return true;
      }
      else
      {
        return false;
      }
    }

    [Builtin("div")]
    public static object Div(object a, object b)
    {
      return ((MultipleValues)DivMod(a, b))[0];
    }

    [Builtin("mod")]
    public static object Mod(object a, object b)
    {
      return ((MultipleValues)DivMod(a, b))[1];
    }

    [Builtin("div-and-mod")]
    public static object DivMod(object x1, object x2)
    {
      if (IsTrue(IsZero(x2)))
      {
        return AssertionViolation("div-and-mod", "divide by zero", x1, x2);
      }
      if (IsTrue(IsInfinite(x1)))
      {
        return AssertionViolation("div-and-mod", "cannot be infinite", x1);
      }
      if (IsTrue(IsNan(x1)))
      {
        return AssertionViolation("div-and-mod", "cannot be nan", x1);
      }

      bool exactargs = IsTrue(IsExact(x1)) && IsTrue(IsExact(x2));
      object scale = 1;

      if (exactargs)
      {
        scale = Multiply(Denominator(x1), Denominator(x2));
        x1 = Multiply(x1, scale);
        x2 = Multiply(x2, scale);
      }

      object a = x1;
      object b = x2;

      object div = Floor(Divide(a, b));
      object mod = RemainderInternal(a , b);

      if (IsTrue(IsNegative(mod)))
      {
        mod = Add(mod, Multiply(b , Sign(b)));

        if (BothPostiveOrNegative(a,b))
        {
          div = Add(div, 1);
        }
        else if (IsTrue(IsZero(div)))
        {
          div = Add(div, -1);
        }
      }
      else if (IsTrue(IsZero(mod)) || IsTrue(IsZero(div)))
      {
      }
      else if (IsTrue(IsGreaterThan(mod, b)))
      {
        if (!BothPostiveOrNegative(a,b))
        {
          div = Add(div, 1);
        }
      }

      if (exactargs)
      {
        return Values(Exact(div), Exact(Divide(mod, scale)));
      }

      if (IsTrue(IsNan(div)) || IsTrue(IsInfinite(div)))
      {
        return Values(div, mod);
      }
      else
      {
        return Values(Exact(div), mod);
      }
    }

    [Builtin("div0")]
    public static object Div0(object a, object b)
    {
      return ((MultipleValues)Div0Mod0(a, b))[0];
    }

    [Builtin("mod0")]
    public static object Mod0(object a, object b)
    {
      return ((MultipleValues)Div0Mod0(a, b))[1];
    }

    [Builtin("div0-and-mod0")]
    public static object Div0Mod0(object x1, object x2)
    {
      bool exactargs = IsTrue(IsExact(x1)) && IsTrue(IsExact(x2));
      object scale = 1;

      if (exactargs)
      {
        scale = Multiply(Denominator(x1), Denominator(x2));
        x1 = Multiply(x1, scale);
        x2 = Multiply(x2, scale);
      }

      object a = x1;
      object b = x2;

      MultipleValues dv = (MultipleValues)DivMod(a, b);
      object div = dv[0];
      object mod = dv[1];
      object h = Abs(Divide( b , 2));

      if (IsTrue(IsGreaterThan(mod, h)) && IsTrue(IsGreaterThanOrEqual(mod, Subtract(h))))
      {
        mod = Subtract(mod, Multiply(b, Sign(b)));
        div = Divide(Subtract(a, mod), b);
      }
      else if (IsTrue(IsSame(mod, h)))
      {
        mod = Subtract(mod, Multiply(b, Sign(b)));
        div = Add(div, Sign(b));
      }

      if (exactargs)
      {
        return Values(Exact(div), Exact(Divide(mod, scale)));
      }

      if (IsTrue(IsNan(div)) || IsTrue(IsInfinite(div)))
      {
        return Values(div, mod);
      }
      else
      {
        return Values(Exact(div), mod);
      }
    }
    
    [Builtin("gcd")]
    public static object GreatestCommonDivider(params object[] args)
    {
      switch (args.Length)
      {
        case 0:
          return 0;
        case 1:
          if (!IsTrue(IsIntegerValued(args[0])))
          {
            return AssertionViolation("gcd", "not an integer", args[0]);
          }
          return Abs(args[0]);
        case 2:
          object first = args[0], second = args[1];

          if (!IsTrue(IsIntegerValued(first)))
          {
            return AssertionViolation("gcd", "not an integer", first);
          }
          if (!IsTrue(IsIntegerValued(second)))
          {
            return AssertionViolation("gcd", "not an integer", second);
          }

          if ((bool)IsZero(second))
          {
            return Abs(first);
          }
          else
          {
            return Abs(GreatestCommonDivider(second, Mod(first, second)));
          }
        default:
          object gcd = GreatestCommonDivider(args[0],args[1]);
          for (int i = 2; i < args.Length; i++)
			    {
			      gcd = GreatestCommonDivider(gcd, args[i]);
			    }
          return gcd;
      }
    }

    [Builtin("lcm")]
    public static object LowestCommonMultiple(params object[] args)
    {
      switch (args.Length)
      {
        case 0:
          return 1;
        case 1:
          if (!IsTrue(IsIntegerValued(args[0])))
          {
            return AssertionViolation("lcm", "not an integer", args[0]);
          }
          return Abs(args[0]);
        case 2:
          object first = args[0], second = args[1];
          if (!IsTrue(IsIntegerValued(first)))
          {
            return AssertionViolation("lcm", "not an integer", first);
          }
          if (!IsTrue(IsIntegerValued(second)))
          {
            return AssertionViolation("lcm", "not an integer", second);
          }
          if (IsTrue(IsZero(first)) || IsTrue(IsZero(second)))
          {
            return 0;
          }
          return Abs(Multiply(Divide(first, GreatestCommonDivider(first, second)), second));
        default:
          object lcm = LowestCommonMultiple(args[0], args[1]);
          for (int i = 2; i < args.Length; i++)
          {
            lcm = LowestCommonMultiple(lcm, args[i]);
          }
          return lcm;
      }
    }

    static TypeConverter FractionConverter = TypeDescriptor.GetConverter(typeof(Fraction));

    [Builtin("numerator")]
    public static object Numerator(object obj)
    {
      if (obj is Complex64)
      {
        return AssertionViolation("numerator", "not a real", obj);
      }
      if (IsTrue(IsInteger(obj)))
      {
        return obj;
      }
      if (obj is Fraction)
      {
        return Exact(((Fraction)obj).Numerator);
      }
      bool exact = IsTrue(IsExact(obj));
      BigInteger r = ((Fraction)FractionConverter.ConvertFrom(obj)).Numerator;
      if (exact)
      {
        return Exact(r);
      }
      else
      {
        return SafeConvert(r);
      }
    }

    [Builtin("denominator")]
    public static object Denominator(object obj)
    {
      if (obj is Complex64)
      {
        return AssertionViolation("denominator", "not a real", obj);
      }
      if (IsTrue(IsInteger(obj)))
      {
        return 1;
      }
      if (obj is Fraction)
      {
        return Exact(((Fraction)obj).Denominator);
      }
      bool exact = IsTrue(IsExact(obj));
      BigInteger r = ((Fraction)FractionConverter.ConvertFrom(obj)).Denominator;
      if (exact)
      {
        return Exact(r);
      }
      else
      {
        return SafeConvert(r);
      }
    }

    [Builtin("floor")]
    public static object Floor(object obj)
    {
      if (IsTrue(IsInteger(obj)))
      {
        return obj;
      }
      if (IsTrue(IsRational(obj)))
      {
        Fraction f = ConvertToRational(obj);
        BigInteger c = f.Numerator / f.Denominator;
        if (c < 0)
        {
          c -= 1;
        }
        return ToIntegerIfPossible(c);
      }
      object res = MathHelper(Math.Floor, obj);
      if (IsTrue(IsExact(obj)))
      {
        return Exact(res);
      }
      else
      {
        return res;
      }
    }

    [Builtin("ceiling")]
    public static object Ceiling(object obj)
    {
      if (IsTrue(IsInteger(obj)))
      {
        return obj;
      }
      if (IsTrue(IsRational(obj)))
      {
        Fraction f = ConvertToRational(obj);
        BigInteger c = f.Numerator / f.Denominator;
        if (c > 0)
        {
          c += 1;
        }
        return ToIntegerIfPossible(c);
      }
      object res = MathHelper(Math.Ceiling, obj);
      if (IsTrue(IsExact(obj)))
      {
        return Exact(res);
      }
      else
      {
        return res;
      }
    }

    [Builtin("truncate")]
    public static object Truncate(object obj)
    {
      if (IsTrue(IsInteger(obj)))
      {
        return obj;
      }
      if (IsTrue(IsRational(obj)))
      {
        Fraction f = ConvertToRational(obj);
        BigInteger c = f.Numerator / f.Denominator;
        return ToIntegerIfPossible(c);
      }
      object res = MathHelper(Math.Truncate, obj);
      if (IsTrue(IsExact(obj)))
      {
        return Exact(res);
      }
      else
      {
        return res;
      }
    }

    [Builtin("round")]
    public static object Round(object obj)
    {
      if (IsTrue(IsInteger(obj)))
      {
        return obj;
      }
      if (IsTrue(IsRational(obj)))
      {
        Fraction f = ConvertToRational(obj);
        BigInteger c = f.Numerator / f.Denominator;
        BigInteger d = f.Numerator % f.Denominator;
        if (d < 0)
        {
          if (-d > f.Denominator / 2)
          {
            return ToIntegerIfPossible(c - 1);
          }
          else if (-d < f.Denominator / 2)
          {
            return ToIntegerIfPossible(c);
          }
        }
        else if (d > 0)
        {
          if (d > f.Denominator / 2)
          {
            return ToIntegerIfPossible(c + 1);
          }
          else if (d < f.Denominator / 2)
          {
            return ToIntegerIfPossible(c);
          }
        }
        else
        {
          if (c % 2 == 0)
          {
            return ToIntegerIfPossible(c);
          }
          else
          {
            return ToIntegerIfPossible(c + 1);
          }
        }
      }
      object res = MathHelper(Math.Round, obj);
      if (IsTrue(IsExact(obj)))
      {
        return Exact(res);
      }
      else
      {
        return res;
      }
    }

    #region MathHelper

    delegate R Function<T, R>(T t);
    delegate R Function<T1, T2, R>(T1 t1, T2 t2);

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
        return Convert.ToDouble(obj);
      }
      catch (OverflowException)
      {
        return IsTrue(IsPositive(obj)) ? double.PositiveInfinity : double.NegativeInfinity;
      }
    }

    static object MathHelper(Function<double,double> func, object obj)
    {
      if (obj is double)
      {
        return func((double)obj);
      }
      else if (obj is int)
      {
        return func((int)obj);
      }
      else
      {
        double d = SafeConvert(obj);
        return func(d);
      }
    }

    static object MathHelper(Function<double, double, double> func, object num1, object num2)
    {
      if (num1 is double)
      {
        if (num2 is double)
        {
          return func((double)num1, (double)num2);
        }
        else if (num2 is int)
        {
          return func((double)num1, (int)num2);
        }
      }
      if (num1 is int)
      {
        if (num2 is int)
        {
          return func((int)num1, (int)num2);
        }
        else if (num2 is double)
        {
          return func((int)num1, (double)num2);
        }
      }
      double d1 = SafeConvert(num1);
      double d2 = SafeConvert(num2);
      return func(d1,d2);
    }

    #endregion

    [Builtin("exp")]
    public static object Exp(object obj)
    {
      if (obj is Complex64)
      {
        return Complex64.Exp((Complex64)obj);
      }
      return MathHelper(Math.Exp, obj);
    }

    [Builtin("log")]
    public static object Log(object obj)
    {
      if (obj is Complex64 || IsTrue(IsNegative(obj)))
      {
        return Complex64.Log(ConvertToComplex(obj));
      }
      if (IsTrue(IsZero(obj)))
      {
        if (IsTrue(IsExact(obj)))
        {
          return AssertionViolation("log", "not possible", obj);
        }
        else
        {
          return double.NegativeInfinity;
        }
      }
      if (IsTrue(IsInfinite(obj)))
      {
        if (IsTrue(IsNegative(obj)))
        {
          return MakeRectangular(Abs(obj), 0);
        }
        else
        {
          return obj;
        }
      }
      return MathHelper(Math.Log, obj);
    }

    [Builtin("log")]
    public static object Log(object obj, object bas)
    {
      return Divide(Log(obj), Log(bas));
    }
    
    [Builtin("sin")]
    public static object Sin(object obj)
    {
      if (obj is Complex64)
      {
        return Complex64.Sin((Complex64)obj);
      }

      return MathHelper(Math.Sin, obj);
    }

    [Builtin("asin")]
    public static object Asin(object obj)
    {
      return Complex64.Asin(ConvertToComplex(obj));
    }

    [Builtin("sinh")]
    public static object Sinh(object obj)
    {
      if (obj is Complex64)
      {
        return Complex64.Sinh((Complex64)obj);
      }

      return MathHelper(Math.Sinh, obj);
    }

    [Builtin("cos")]
    public static object Cos(object obj)
    {
      if (obj is Complex64)
      {
        return Complex64.Cos((Complex64)obj);
      }

      return MathHelper(Math.Cos, obj);
    }

    [Builtin("acos")]
    public static object Acos(object obj)
    {
      return Complex64.Acos(ConvertToComplex(obj));
    }

    [Builtin("cosh")]
    public static object Cosh(object obj)
    {
      if (obj is Complex64)
      {
        return Complex64.Cosh((Complex64)obj);
      }

      return MathHelper(Math.Cosh, obj);
    }

    [Builtin("tan")]
    public static object Tan(object obj)
    {
      if (obj is Complex64)
      {
        return Complex64.Tan((Complex64)obj);
      }

      return MathHelper(Math.Tan, obj);
    }

    [Builtin("atan")]
    public static object Atan(object obj)
    {
      if (obj is Complex64)
      {
        return Complex64.Atan((Complex64)obj);
      }

      return MathHelper(Math.Atan, obj);
    }

    [Builtin("atan")]
    public static object Atan(object obj, object obj2)
    {
      return MathHelper(Math.Atan2, obj, obj2);
    }

    [Builtin("tanh")]
    public static object Tanh(object obj)
    {
      if (obj is Complex64)
      {
        return Complex64.Tanh((Complex64)obj);
      }

      return MathHelper(Math.Tanh, obj);
    }

    //based on lsqrt()
    static object SqrtBigInteger(BigInteger x)
    {
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
        return MathHelper(Math.Sqrt, v0);
      }
      return x1;
    }

    [Builtin("sqrt")]
    public static object Sqrt(object obj)
    {
      if (obj is Complex64)
      {
        return Complex64.Sqrt((Complex64)obj);
      }

      if (IsTrue(IsNegative(obj)))
      {
        return MakeRectangular(0, Sqrt(Subtract(obj)));
      }
      if (obj is BigInteger)
      {
         return SqrtBigInteger((BigInteger)obj);
      }

      if (IsTrue(IsInfinite(obj)))
      {
        return obj;
      }

      object res = MathHelper(Math.Sqrt, obj);
      if (IsTrue(IsExact(obj)))
      {
        return IntegerIfPossible(res);
      }
      else
      {
        return res;
      }
    }

    [Builtin("exact-integer-sqrt")]
    public static object ExactIntegerSqrt(object obj)
    {
      object r = Sqrt(obj);
      object rf = Exact(Floor(r));
      object rest = Subtract(obj, Multiply(rf, rf));

      return Values(rf, rest);
    }

    [Builtin("expt")]
    public static object Expt(object obj1, object obj2)
    {
      if (obj1 is Complex64 || IsTrue(IsNegative(obj1)))
      {
        return Complex64.Pow(ConvertToComplex(obj1), ConvertToComplex(obj2));
      }

      bool exact = IsTrue(IsExact(obj1)) && IsTrue(IsExact(obj2));

      if (IsTrue(IsZero(obj1)) && !IsTrue(IsZero(obj2)))
      {
        return exact ? 0 : (object)0.0;
      }

      if (IsTrue(IsZero(obj2)))
      {
        return exact ? 1 : (object)1.0;
      }

      if (IsTrue(IsSame(obj1, 1)))
      {
        return exact ? 1 : (object)1.0;
      }

      if (IsTrue(IsSame(obj2, 1)))
      {
        return exact ? Exact(obj1) : Inexact(obj1);
      }


      bool isnegative = IsTrue(IsNegative(obj2));

      if (isnegative)
      {
        obj2 = Abs(obj2);
      }

      if (IsTrue(IsInteger(obj1)) && IsTrue(IsInteger(obj2)))
      {
        BigInteger a = (BigInteger)BigIntConverter.ConvertFrom(obj1);
        BigInteger r = a.Power(Convert.ToInt32(obj2));
        if (isnegative)
        {
          if (r == 0)
          {
            return ImplementationRestriction("expt", "no supported", obj1, obj2);
          }
          return Divide(1, r);
        }
        if (r < int.MaxValue && r > int.MinValue)
        {
          return r.ToInt32();
        }
        return r;
      }

      if (IsTrue(IsRational(obj1)) && IsTrue(IsIntegerValued(obj2)))
      {
        Fraction f = ConvertToRational(obj1);
        if (obj2 is Fraction)
        {
          obj2 = Divide(obj2, 1);
        }
        if (isnegative)
        {
          return Divide(Expt(f.Denominator, obj2), Expt(f.Numerator, obj2));
        }
        else
        {
          return Divide(Expt(f.Numerator, obj2), Expt(f.Denominator, obj2));
        }
      }

      if (IsTrue(IsReal(obj1)) && IsTrue(IsReal(obj2)))
      {
        object res = MathHelper(Math.Pow, obj1, obj2);
        if (isnegative)
        {
          return Divide(1, res);
        }
        else
        {
          //NumberClass e = GetNumberClass(obj1) & GetNumberClass(obj2);
          //return GetNumber(e, res);
          return res;
        }
      }



      return ImplementationRestriction("expt", "no supported", obj1, obj2);
    }

    static object IntegerIfPossible(object res)
    {
      if (IsTrue(IsIntegerValued(res)))
      {
        return Exact(res);
      }
      return res;
    }

    [Builtin("make-rectangular")]
    public static object MakeRectangular(object obj1, object obj2)
    {
      return Complex64.Make(SafeConvert(obj1), SafeConvert(obj2));
    }

    [Builtin("make-polar")]
    public static object MakePolar(object obj1, object obj2)
    {
      if (!IsTrue(IsReal(obj1)))
      {
        return AssertionViolation("make-polar", "not a real", obj1);
      }
      if (!IsTrue(IsReal(obj2)))
      {
        return AssertionViolation("make-polar", "not a real", obj2);
      }

      return Multiply(obj1, MakeRectangular(Cos(obj2), Sin(obj2)));
    }

    [Builtin("real-part")]
    public static object RealPart(object obj)
    {
      if (obj is Complex64)
      {
        Complex64 c = RequiresNotNull<Complex64>(obj);
        return c.Real;
      }
      else if (IsTrue(IsReal(obj)))
      {
        return obj;
      }
      return AssertionViolation("real-part", "not a number", obj);
    }

    [Builtin("imag-part")]
    public static object ImagPart(object obj)
    {
      if (obj is Complex64)
      {
        Complex64 c = RequiresNotNull<Complex64>(obj);
        return c.Imag;
      }
      else if (IsTrue(IsReal(obj)))
      {
        return 0;
      }
      return AssertionViolation("imag-part", "not a number", obj);
    }

    [Builtin("magnitude")]
    public static object Magnitude(object obj)
    {
      if (obj is Complex64)
      {
        Complex64 c = ConvertToComplex(obj);
        double m = Math.Sqrt(c.Imag * c.Imag + c.Real * c.Real);
        return m;
      }
      else if (IsTrue(IsNumber(obj)))
      {
        return Abs(obj);
      }
      else
      {
        return AssertionViolation("magnitude", "not a number", obj);
      }
    }

    [Builtin("angle")]
    public static object Angle(object obj)
    {
      Complex64 c = ConvertToComplex(obj);
      return Atan(c.Imag, c.Real);
    }
  }
}
