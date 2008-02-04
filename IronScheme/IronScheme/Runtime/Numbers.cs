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
      return false;
    }

    static object PrintOctal(object num)
    {
      return false;
    }


    [Builtin("number->string")]
    public static object NumberToString(object obj, object radix)
    {
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
              return string.Format("{0:f1}", obj);
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
              if (d > 10e17 || d < -10e17)
              {
                return string.Format("{0:g}", new Decimal(d));
              }
              return string.Format("{0:r}", obj);
            }
          }
          return obj.ToString();
        case 16:
          return string.Format("{0:x}", obj);
      }

      return false;
    }

    [Builtin("string->number")]
    public static object StringToNumber(object obj)
    {
      string str = RequiresNotNull<string>(obj);

      if (str.Length == 0)
      {
        return false;
      }

      switch (str[0])
      {
        case '#':
          switch (str[1])
          {
            case 'b':
              return StringToNumber(str.Substring(2), 2);
            case 'o':
              return StringToNumber(str.Substring(2), 8);
            case 'd':
              return StringToNumber(str.Substring(2), 10);
            case 'x':
              return StringToNumber(str.Substring(2), 16);
            default:
              return false;
          }
        default:
          return StringToNumber(obj, 10);
      }
    }

    static object ParseBinary(string str)
    {
      if (str.Length <= 32)
      {
        int b = 1;
        int n = 0;
        for (int i = 0; i < str.Length; i++, b *= 2)
        {
          char c = str[str.Length - 1 - i];
          n += b * (c - '0');
        }
        return n;
      }
      else
      {
        long b = 1;
        long n = 0;
        for (int i = 0; i < str.Length; i++, b *= 2)
        {
          char c = str[str.Length - 1 - i];
          n += b * (c - '0');
        }
        return n;
      }
    }

    static object ParseOctal(string str)
    {
      if (str.Length < 11) // not precise, bleh
      {
        int b = 1;
        int n = 0;
        for (int i = 0; i < str.Length; i++, b *= 8)
        {
          char c = str[str.Length - 1 - i];
          n += b * (c - '0');
        }
        return n;
      }
      else
      {
        long b = 1;
        long n = 0;
        for (int i = 0; i < str.Length; i++, b *= 8)
        {
          char c = str[str.Length - 1 - i];
          n += b * (c - '0');
        }
        return n;
      }
    }


    static bool BigIntegerTryParse(string number, out BigInteger result)
    {
      result = null;

      if (number == null)
        return false;


      int i = 0, len = number.Length, sign = 1;

      char c;
      bool digits_seen = false;
      BigInteger val = new BigInteger(0);
      if (number[i] == '+')
      {
        i++;
      }
      else if (number[i] == '-')
      {
        sign = -1;
        i++;
      }
      for (; i < len; i++)
      {
        c = number[i];
        if (c == '\0')
        {
          i = len;
          continue;
        }
        if (c >= '0' && c <= '9')
        {
          val = val * 10 + (c - '0');
          digits_seen = true;
        }
        else
        {
          if (Char.IsWhiteSpace(c))
          {
            for (i++; i < len; i++)
            {
              if (!Char.IsWhiteSpace(number[i]))
                return false;
            }
            break;
          }
          else
            return false;
        }
      }
      if (!digits_seen)
        return false;

      result = val * sign;

      return true;
    }

    static object ParseDecimal(string str)
    {
      int n;
      if (int.TryParse(str, out n))
      {
        return n;
      }
      long l;
      if (long.TryParse(str, out l))
      {
        return l;
      }
      BigInteger bi;
      if (BigIntegerTryParse(str, out bi))
      {
        return bi;
      }
      double d;
      if (double.TryParse(str, out d))
      {
        return d;
      }
      char sign = str[0];
      string value = str.Substring(1);
      if (value == "nan.0")
      {
        return double.NaN;
      }
      else if (value == "inf.0")
      {
        return sign == '-' ? double.NegativeInfinity : double.PositiveInfinity;
      }
      // TODO parse complex
      string[] tokens = Regex.Split(str, "[+-]");

      return false;
    }




    [Builtin("string->number")]
    public static object StringToNumber(object obj, object radix)
    {
      string str = RequiresNotNull<string>(obj);
      radix = radix ?? 10;
      int r = (int)radix;

      if (str.Length == 0)
      {
        return false;
      }

      int fi = str.IndexOf('/');

      if (fi > 0)
      {
        object n1 = StringToNumber( str.Substring(0, fi), radix);
        object n2 = StringToNumber( str.Substring(fi + 1), radix);

        return new Fraction(Convert.ToInt64(n1), Convert.ToInt64(n2));
      }


      switch (r)
      {
        case 2:
          return ParseBinary(str);
        case 8:
          return ParseOctal(str);
        case 10:
          return ParseDecimal(str);
        case 16:
          if (str.Length > 16)
          {
            return false;
          }
          else
          if (str.Length > 8)
          {
            return long.Parse(str, System.Globalization.NumberStyles.HexNumber);
          }
          else
          {
            return int.Parse(str, System.Globalization.NumberStyles.HexNumber);
          }
        default:
          return false;
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
      return (bool)IsReal(obj) || obj is Complex64;
    }

    [Builtin("real?")]
    public static object IsReal(object obj)
    {
      return (bool)IsRational(obj) || obj is float || obj is double;
    }

    [Builtin("rational?")]
    public static object IsRational(object obj)
    {
      return (bool)IsInteger(obj) || obj is Fraction;
    }

    [Builtin("integer?")]
    public static object IsInteger(object obj)
    {
      return (obj is int || obj is long || obj is BigInteger || obj is uint || obj is ulong || obj is byte || obj is sbyte || obj is short || obj is ushort);
    }


    //real-valued?, rational-valued?,integer-valued?

    [Builtin("integer-valued?")]
    public static object IsIntegerValued(object obj)
    {
      return IsZero(Mod(obj, 1));
    }

    [Builtin("rational-valued?")]
    public static object IsRationalValued(object obj)
    {
      if (obj is Fraction)
      {
        return true;
      }

      bool iv = (bool)IsIntegerValued(obj);
      if (iv)
      {
        return true;
      }

      if ((bool)IsNumber(obj))
      {
        double d = SafeConvert(obj);
        return d == (double)(Fraction)d;
      }
      return false;
    }

    [Builtin("real-valued?")]
    public static object IsRealValued(object obj)
    {
      if (obj is Complex64)
      {
        Complex64 c = (Complex64)obj;
        if (c.Imag != 0)
        {
          return false;
        }
      }
      return IsNumber(obj);
    }

    [Builtin("finite?")]
    public static object IsFinite(object obj)
    {
      return !(bool)IsInfinite(obj);
    }

    [Builtin("infinite?")]
    public static object IsInfinite(object obj)
    {
      if (obj is double)
      {
        return double.IsInfinity((double)obj);
      }
      if (obj is float)
      {
        return float.IsInfinity((float)obj);
      }

      return false;
    }

    [Builtin("nan?")]
    public static object IsNan(object obj)
    {
      if (obj is double)
      {
        return double.IsNaN((double)obj);
      }
      if (obj is float)
      {
        return float.IsNaN((float)obj);
      }

      return false;
    }


    [Builtin("inexact")]
    public static object Inexact(object obj)
    {
      if ((bool)IsExact(obj))
      {
        return SafeConvert(obj);
      }
      return obj;
    }

    [Builtin("exact")]
    public static object Exact(object obj)
    {
      if ((bool)IsInexact(obj))
      {
        BigInteger r = (BigInteger) BigIntConverter.ConvertFrom(obj);
        int ir;
        if (r.AsInt32(out ir))
        {
          return ir;
        }
        return r;
      }
      return obj;
    }



    [Builtin("exact?")]
    public static object IsExact(object obj)
    {
      return IsRational(obj);
    }

    [Builtin("inexact?")]
    public static object IsInexact(object obj)
    {
      return !(bool)IsRational(obj);
    }


    #region relations

    [Builtin("=")]
    public static object IsSame(object first, object second)
    {
      return Equals(first, second);
    }

    [Builtin("=")]
    public static object IsSame(object first, params object[] rest)
    {
      IComparable last = first as IComparable;

      foreach (IComparable item in rest)
      {
        if (last.CompareTo(item) != 0)
          return false;
        last = item;
      }

      return true;
    }

    [Builtin("<")]
    public static object IsLessThan(object first, object second)
    {
      if (first is int)
      {
        if (second is int)
        {
          return (int)first < (int)second;
        }
        else if (second is double)
        {
          return (int)first < (double)second;
        }
      }
      if (first is double)
      {
        if (second is int)
        {
          return (double)first < (int)second;
        }
        else if (second is double)
        {
          return (double)first < (double)second;
        }
      }
      object value;
      if (OperatorHelper("op_LessThan", first, second, out value))
      {
        return value;
      }
      return IsLessThan(first, new object[] { second });
    }

    [Builtin("<")]
    public static object IsLessThan(object first, params object[] rest)
    {
      IComparable last = first as IComparable;

      foreach (IComparable item in rest)
      {
        if (last.CompareTo(item) >= 0)
          return false;
        last = item;
      }

      return true;
    }

    [Builtin("<=")]
    public static object IsLessThanOrEqual(object first, object second)
    {
      if (first is int)
      {
        if (second is int)
        {
          return (int)first <= (int)second;
        }
        else if (second is double)
        {
          return (int)first <= (double)second;
        }
      }
      if (first is double)
      {
        if (second is int)
        {
          return (double)first <= (int)second;
        }
        else if (second is double)
        {
          return (double)first <= (double)second;
        }
      }
      object value;
      if (OperatorHelper("op_LessThanOrEqual", first, second, out value))
      {
        return value;
      }
      return IsLessThanOrEqual(first, new object[] { second });
    }

    [Builtin("<=")]
    public static object IsLessThanOrEqual(object first, params object[] rest)
    {
      IComparable last = first as IComparable;

      foreach (IComparable item in rest)
      {
        if (last.CompareTo(item) > 0)
          return false;
        last = item;
      }

      return true;
    }

    [Builtin(">")]
    public static object IsGreaterThan(object first, object second)
    {
      if (first is int)
      {
        if (second is int)
        {
          return (int)first > (int)second;
        }
        else if (second is double)
        {
          return (int)first > (double)second;
        }
      }
      if (first is double)
      {
        if (second is int)
        {
          return (double)first > (int)second;
        }
        else if (second is double)
        {
          return (double)first > (double)second;
        }
      }
      object value;
      if (OperatorHelper("op_GreaterThan", first, second, out value))
      {
        return value;
      }
      return IsGreaterThan(first, new object[] { second });
    }

    [Builtin(">")]
    public static object IsGreaterThan(object first, params object[] rest)
    {
      IComparable last = first as IComparable;

      foreach (IComparable item in rest)
      {
        if (last.CompareTo(item) <= 0)
          return false;
        last = item;
      }

      return true;
    }

    [Builtin(">=")]
    public static object IsGreaterThanOrEqual(object first, object second)
    {
      if (first is int)
      {
        if (second is int)
        {
          return (int)first >= (int)second;
        }
        else if (second is double)
        {
          return (int)first >= (double)second;
        }
      }
      if (first is double)
      {
        if (second is int)
        {
          return (double)first >= (int)second;
        }
        else if (second is double)
        {
          return (double)first >= (double)second;
        }
      }
      object value;
      if (OperatorHelper("op_GreaterThanOrEqual", first, second, out value))
      {
        return value;
      }
      return IsGreaterThanOrEqual(first, new object[] { second });
    }

    [Builtin(">=")]
    public static object IsGreaterThanOrEqual(object first, params object[] rest)
    {
      IComparable last = first as IComparable;

      foreach (IComparable item in rest)
      {
        if (last.CompareTo(item) < 0)
          return false;
        last = item;
      }

      return true;
    }


    #endregion



    [Builtin("zero?")]
    public static object IsZero(object obj)
    {
      if ((bool)IsInteger(obj))
      {
        return IsEqualValue(obj, 0);
      }
      else
      {
        return IsEqualValue(obj, 0.0);
      }
    }

    [Builtin("positive?")]
    public static object IsPositive(object obj)
    {
      return IsGreaterThan(obj, 0);
    }

    [Builtin("negative?")]
    public static object IsNegative(object obj)
    {
      return IsLessThan(obj, 0);
    }

    [Builtin("odd?")]
    public static object IsOdd(object obj)
    {
      return !(bool)IsEven(obj);
    }

    [Builtin("even?")]
    public static object IsEven(object obj)
    {
      return IsZero(Mod(obj, 2));
    }

    [Builtin("min")]
    public static object Min(object first, params object[] rest)
    {
      object min = first;
      foreach (object var in rest)
      {
        if ((bool)IsLessThan(var, min))
        {
          min = var;
        }
      }
      return min;
    }

    [Builtin("max")]
    public static object Max(object first, params object[] rest)
    {
      object max = first;
      foreach (object var in rest)
      {
        if ((bool)IsGreaterThan(var, max))
        {
          max = var;
        }
      }
      return max;
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
      return first;
    }

    [Builtin("+")]
    public static object Add(object first, object second)
    {
      if (first is int)
      {
        if (second is int)
        {
          long l = (long)(int)first + (int)second;
          if (l > int.MaxValue || l < int.MinValue)
          {
            return (BigInteger)l;
          }
          else
          {
            return (int)l;
          }
        }
        else if (second is double)
        {
          return (int)first + (double)second;
        }
      }
      if (first is double)
      {
        if (second is int)
        {
          return (double)first + (int)second;
        }
        else if (second is double)
        {
          return (double)first + (double)second;
        }
      }
      if (first is BigInteger && second is int)
      {
        return BigInteger.Add((BigInteger)first, (int)second);
      }
      if (first is int && second is BigInteger)
      {
        return BigInteger.Add((int)first, (BigInteger)second);
      }
      if (first is BigInteger && second is BigInteger)
      {
        return BigInteger.Add((BigInteger)first, (BigInteger)second);
      }
      object value;
      if (OperatorHelper("op_Addition", first, second, out value))
      {
        return value;
      }
      return AssertionViolation("+", "types are not compatible", first, second);
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
      return Subtract(first, new object[0]);

    }

    [Builtin("-")]
    public static object Subtract(object first, object second)
    {
      //optimized version
      if (first is int)
      {
        if (second is int)
        {
          long l = (long)(int)first - (int)second;
          if (l > int.MaxValue || l < int.MinValue)
          {
            return (BigInteger)l;
          }
          else
          {
            return (int)l;
          }
        }
        else if (second is double)
        {
          return (int)first - (double)second;
        }
      }
      if (first is double)
      {
        if (second is int)
        {
          return (double)first - (int)second;
        }
        else if (second is double)
        {
          return (double)first - (double)second;
        }
      }
      if (first is BigInteger && second is int)
      {
        return BigInteger.Subtract((BigInteger)first, (int)second);
      }
      if (first is int && second is BigInteger)
      {
        return BigInteger.Subtract((int)first, (BigInteger)second);
      }
      if (first is BigInteger && second is BigInteger)
      {
        return BigInteger.Subtract((BigInteger)first, (BigInteger)second);
      }
      object value;
      if (OperatorHelper("op_Subtraction", first, second, out value))
      {
        return value;
      }

      return AssertionViolation("-", "types are not compatible", first, second);
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
      return first;
    }

    [Builtin("*")]
    public static object Multiply(object first, object second)
    {
      if (first is int)
      {
        if (second is int)
        {
          long l = Math.BigMul((int)first,(int)second);
          if (l > int.MaxValue || l < int.MinValue)
          {
            return (BigInteger)l;
          }
          else
          {
            return (int)l;
          }
        }
        else if (second is double)
        {
          return (int)first * (double)second;
        }
      }
      if (first is double)
      {
        if (second is int)
        {
          return (double)first * (int)second;
        }
        else if (second is double)
        {
          return (double)first * (double)second;
        }
      }
      if (first is BigInteger && second is int)
      {
        return BigInteger.Multiply((BigInteger)first, (int)second);
      }
      if (first is int && second is BigInteger)
      {
        return BigInteger.Multiply((int)first, (BigInteger)second);
      }
      if (first is BigInteger && second is BigInteger)
      {
        return BigInteger.Multiply((BigInteger)first, (BigInteger)second);
      }
      object value;
      if (OperatorHelper("op_Multiply", first, second, out value))
      {
        return value;
      }
      return AssertionViolation("*", "types are not compatible", first, second);
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
      if (first is int)
      {
        return 1.0 / (int)first;
      }
      if (first is double)
      {
        return 1.0 / (double)first;
      }
      if (first is BigInteger)
      {
        return 1.0 / SafeConvert(first);
      }
      return Divide(first, new object[0]);

    }

    [Builtin("/")]
    public static object Divide(object first, object second)
    {
      if (first is int)
      {
        if (second is int)
        {
          return (int)first / (double)(int)second;
        }
        else if (second is double)
        {
          return (int)first / (double)second;
        }
      }
      if (first is double)
      {
        if (second is int)
        {
          return (double)first / (int)second;
        }
        else if (second is double)
        {
          return (double)first / (double)second;
        }
      }
      if (first is BigInteger && second is int)
      {
        return BigInteger.Divide((BigInteger)first, (int)second);
      }
      if (first is int && second is BigInteger)
      {
        return BigInteger.Divide((int)first, (BigInteger)second);
      }
      if (first is BigInteger && second is BigInteger)
      {
        return BigInteger.Divide((BigInteger)first, (BigInteger)second);
      }
      object value;
      if (OperatorHelper("op_Division", first, second, out value))
      {
        return value;
      }
      return AssertionViolation("/", "types are not compatible", first, second);
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

    static bool OperatorHelper(string opname, object first, object second, out object value)
    {
      if (first != null)
      {
        foreach (MethodInfo mi in first.GetType().GetMember(opname, BindingFlags.Static | BindingFlags.FlattenHierarchy | BindingFlags.Public))
        {
          ParameterInfo[] pis = mi.GetParameters();
          if (pis.Length == 2)
          {
            Type p1t = pis[0].ParameterType;
            Type p2t = pis[1].ParameterType;
            if (p1t.IsInstanceOfType(first)
              && p2t.IsInstanceOfType(second))
            {
              value = mi.Invoke(null, new object[] { first, second });
              return true;
            }
            else if (second != null)
            {
              TypeConverter tc1 = TypeDescriptor.GetConverter(p1t);
              TypeConverter tc2 = TypeDescriptor.GetConverter(p2t);

              if (tc1.CanConvertFrom(first.GetType()) && tc2.CanConvertFrom(second.GetType()))
              {
                object afirst = tc1.ConvertFrom(first);
                object asecond = tc2.ConvertFrom(second);

                value = mi.Invoke(null, new object[] { afirst, asecond });
                return true;
              }
            }
          }
        }
      }
      if (second != null)
      {
        foreach (MethodInfo mi in second.GetType().GetMember(opname, BindingFlags.Static | BindingFlags.FlattenHierarchy | BindingFlags.Public))
        {
          ParameterInfo[] pis = mi.GetParameters();
          if (pis.Length == 2)
          {
            Type p1t = pis[0].ParameterType;
            Type p2t = pis[1].ParameterType;
            if (p1t.IsInstanceOfType(first)
              && p2t.IsInstanceOfType(second))
            {
              value = mi.Invoke(null, new object[] { first, second });
              return true;
            }
            else if (first != null)
            {
              TypeConverter tc1 = TypeDescriptor.GetConverter(p1t);
              TypeConverter tc2 = TypeDescriptor.GetConverter(p2t);

              if (tc1.CanConvertFrom(first.GetType()) && tc2.CanConvertFrom(second.GetType()))
              {
                object afirst = tc1.ConvertFrom(first);
                object asecond = tc2.ConvertFrom(second);

                value = mi.Invoke(null, new object[] { afirst, asecond });
                return true;
              }
            }
          }
        }
      }
      value = null;
      return false;
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
        return Math.Abs((int)obj);
      }
      else if (obj is BigInteger)
      {
        return ((BigInteger)obj).Abs();
      }
      else if (obj is Complex64)
      {
        return ((Complex64)obj).Abs();
      }
      else
      {
        double d = SafeConvert(obj);
        return Math.Abs(d);
      }
    }

    [Builtin("div")]
    public static object Div(object a, object b)
    {
      return ((object[])DivMod(a, b))[0];
    }

    [Builtin("mod")]
    public static object Mod(object a, object b)
    {
      return ((object[])DivMod(a, b))[1];
    }

    [Builtin("div-and-mod")]
    public static object DivMod(object x1, object x2)
    {
      double a = Convert.ToDouble(x1);
      double b = Convert.ToDouble(x2);

      double div = Math.Floor(a / b);
      double mod = a % b;

      if (mod < 0)
      {
        mod += (b * Math.Sign(b));
        if ((a > 0 && b > 0) || (a < 0 && b < 0))
        {
          div++;
        }
      }
      else if (mod > b)
      {
        if (!((a > 0 && b > 0) || (a < 0 && b < 0)))
        {
          div++;
        }
      }
      return Values(Convert.ToInt32(div), mod);
    }

    [Builtin("div0")]
    public static object Div0(object a, object b)
    {
      return ((object[])Div0Mod0(a, b))[0];
    }

    [Builtin("mod0")]
    public static object Mod0(object a, object b)
    {
      return ((object[])Div0Mod0(a, b))[1];
    }

    [Builtin("div0-and-mod0")]
    public static object Div0Mod0(object x1, object x2)
    {
      double a = Convert.ToDouble(x1);
      double b = Convert.ToDouble(x2);

      object[] dv = (object[]) DivMod(a, b);
      double div = Convert.ToDouble(dv[0]);
      double mod = Convert.ToDouble(dv[1]);
      double h = b / 2;

      if (mod > h && mod > -h)
      {
        mod -= (b * Math.Sign(b));
        if ((a > 0 && b > 0) || (a < 0 && b < 0) && mod != -h)
        {
          div--;
        }
        else
        {
          div++;
        }
      }
      else if (mod == h)
      {
        mod -= (b * Math.Sign(b));
        div++;
      }

      return Values(Convert.ToInt32(div), mod);
    }
    

    /*
(= gcd (fn (a b)
  (if (eql b 0)
      a
      (gcd b (% a b)))))
     */

    [Builtin("gcd")]
    public static object GreatestCommonDivider(params object[] args)
    {
      switch (args.Length)
      {
        case 0:
          return 0;
        case 1:
          return args[0];
        case 2:
          object first = args[0], second = args[1];

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

//    If you know gcd, then the lcm of two numbers is simply: 
//    a / gcd(a, b) * b 
    [Builtin("lcm")]
    public static object LowestCommonMultiple(params object[] args)
    {
      switch (args.Length)
      {
        case 0:
          return 1;
        case 1:
          return args[0];
        case 2:
          object first = args[0], second = args[1];

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
      if (obj is Fraction)
      {
        return ((Fraction)obj).Numerator;
      }
      return ((Fraction)FractionConverter.ConvertFrom(obj)).Numerator;
    }

    [Builtin("denominator")]
    public static object Denominator(object obj)
    {
      if (obj is Fraction)
      {
        return ((Fraction)obj).Denominator;
      }
      return ((Fraction)FractionConverter.ConvertFrom(obj)).Denominator;
    }

    [Builtin("floor")]
    public static object Floor(object obj)
    {
      return MathHelper(Math.Floor, obj);
    }

    [Builtin("ceiling")]
    public static object Ceiling(object obj)
    {
      return MathHelper(Math.Ceiling, obj);
    }

    [Builtin("truncate")]
    public static object Truncate(object obj)
    {
      return MathHelper(Math.Truncate, obj);
    }

    [Builtin("round")]
    public static object Round(object obj)
    {
      return MathHelper(Math.Round, obj);
    }

    [Builtin("rationalize")]
    public static object Rationalize(object obj1, object obj2)
    {
      return false;
    }


    #region MathHelper

    static double SafeConvert(object obj)
    {
      try
      {
        return Convert.ToDouble(obj);
      }
      catch (OverflowException)
      {
        return (bool)IsPositive(obj) ? double.PositiveInfinity : double.NegativeInfinity;
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
      return MathHelper(Math.Exp, obj);
    }

    [Builtin("log")]
    public static object Log(object obj)
    {
      return MathHelper(Math.Log, obj);
    }
    

    [Builtin("sin")]
    public static object Sin(object obj)
    {
      return MathHelper(Math.Sin, obj);
    }

    [Builtin("asin")]
    public static object Asin(object obj)
    {
      return MathHelper(Math.Asin, obj);
    }

#if EXT_LIB
    [Builtin("sinh")]
    public static object Sinh(object obj)
    {
      return MathHelper(Math.Sinh, obj);
    }
#endif

    [Builtin("cos")]
    public static object Cos(object obj)
    {
      return MathHelper(Math.Cos, obj);
    }

    [Builtin("acos")]
    public static object Acos(object obj)
    {
      return MathHelper(Math.Acos, obj);
    }

#if EXT_LIB
    [Builtin("cosh")]
    public static object Cosh(object obj)
    {
      return MathHelper(Math.Cosh, obj);
    }
#endif

    [Builtin("tan")]
    public static object Tan(object obj)
    {
      return MathHelper(Math.Tan, obj);
    }

    [Builtin("atan")]
    public static object Atan(object obj)
    {
      return MathHelper(Math.Atan, obj);
    }

    [Builtin("atan")]
    public static object Atan(object obj, object obj2)
    {
      return MathHelper(Math.Atan2, obj, obj2);
    }

#if EXT_LIB
    [Builtin("tanh")]
    public static object Tanh(object obj)
    {
      return MathHelper(Math.Tanh, obj);
    }
#endif

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
      if (obj is BigInteger)
      {
         return SqrtBigInteger((BigInteger)obj);
      }
      return MathHelper(Math.Sqrt, obj);
    }

    [Builtin("expt")]
    public static object Expt(object obj1, object obj2)
    {
      if ((bool)IsInteger(obj1) && (bool)IsInteger(obj2))
      {
        BigInteger a = (BigInteger)BigIntConverter.ConvertFrom(obj1);
        BigInteger r = a.Power(Convert.ToInt32(obj2));
        if (r < int.MaxValue && r > int.MinValue)
        {
          return r.ToInt32();
        }
        return r;
      }

      return MathHelper(Math.Pow, obj1, obj2);
    }

    [Builtin("make-rectangular")]
    public static object MakeRectangular(object obj1, object obj2)
    {
      return false;
    }

    [Builtin("make-polar")]
    public static object MakePolar(object obj1, object obj2)
    {
      return false;
    }

    [Builtin("real-part")]
    public static object RealPart(object obj)
    {
      return false;
    }

    [Builtin("imag-part")]
    public static object ImagPart(object obj)
    {
      return false;
    }

    [Builtin("magnitude")]
    public static object Magnitude(object obj)
    {
      return false;
    }

    [Builtin("angle")]
    public static object Angle(object obj)
    {
      return false;
    }
    

    [Builtin("exact->inexact")]
    public static object ExactToInexact(object obj)
    {
      if ((bool)IsExact(obj))
      {
        return SafeConvert(obj);
      }
      return obj;
    }

    [Builtin("inexact->exact")]
    public static object InexactToExact(object obj)
    {
      if ((bool)IsInexact(obj))
      {
        return Convert.ToInt64(obj);
      }
      return obj;
    }

  }
}
