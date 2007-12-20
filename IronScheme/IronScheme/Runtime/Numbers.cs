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
      decimal dec;
      if (decimal.TryParse(str, out dec))
      {
        return dec;
      }
      // TODO parse complex
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
      return (bool)IsRational(obj) || obj is float || obj is double || obj is decimal;
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


#if R6RS
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
        decimal d = Convert.ToDecimal(obj);
        return d == (decimal)(Fraction)d;
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
        if (obj is Fraction)
        {
          return (decimal)((Fraction)obj);
        }
        return Convert.ToDecimal(obj);
      }
      return obj;
    }

    [Builtin("exact")]
    public static object Exact(object obj)
    {
      if ((bool)IsInexact(obj))
      {
        return Convert.ToInt64(obj);
      }
      return obj;
    }

#endif

    [Builtin("exact?")]
    public static object IsExact(object obj)
    {
      return (bool)IsInteger(obj) || (bool)IsRational(obj);
    }

    [Builtin("inexact?")]
    public static object IsInexact(object obj)
    {
      return obj is Complex64 || obj is float || obj is double || obj is decimal;
    }




    #region relations

    [Builtin("=")]
    public static object IsSame(object first, object second)
    {
      return IsEqualValue(first, second);
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
        return (bool)value;
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
        return (bool)value;
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
        return (bool)value;
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
        return (bool)value;
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
      return !(bool)IsEqualValue(Mod(obj, 2), 0);
    }

    [Builtin("even?")]
    public static object IsEven(object obj)
    {
      return IsEqualValue(Mod(obj, 2), 0);
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
          try
          {
            return checked((int)first + (int)second);
          }
          catch (OverflowException)
          {
            return (BigInteger)(int)first + (int)second;
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
      if (first is BigInteger && second is BigInteger)
      {
        return BigInteger.Add((BigInteger)first, (BigInteger)second);
      }
      object value;
      if (OperatorHelper("op_Addition", first, second, out value))
      {
        return value;
      }
      return Add(first, new object[] { second });
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
          try
          {
            return checked((int)first * (int)second);
          }
          catch (OverflowException)
          {
            return (BigInteger)(int)first * (int)second;
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
      if (first is BigInteger && second is BigInteger)
      {
        return BigInteger.Multiply((BigInteger)first, (BigInteger)second);
      }
      object value;
      if (OperatorHelper("op_Multiply", first, second, out value))
      {
        return value;
      }
      return Multiply(first, new object[] { second });
    }


    [Builtin("*")]
    public static object Multiply(object car, params object[] args)
    {
      if (car is Missing)
      {
        return 1;
      }
      Type type = car == null ? typeof(double) : car.GetType();
      double result = Convert.ToDouble(car);
      foreach (object item in args)
      {
        if (item is Double)
          type = item.GetType();

        result *= Convert.ToDouble(item);
      }
      return ConvertNumber(result, type);
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
      return Divide(first, new object[0]);

    }

    [Builtin("/")]
    public static object Divide(object first, object second)
    {
      if (first is int)
      {
        if (second is int)
        {
          return (int)first / (int)second;
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
      if (first is BigInteger && second is BigInteger)
      {
        return BigInteger.Divide((BigInteger)first, (BigInteger)second);
      }
      object value;
      if (OperatorHelper("op_Division", first, second, out value))
      {
        return value;
      }
      return Divide(first, new object[] { second });
    }


    [Builtin("/")]
    public static object Divide(object car, params object[] args)
    {
      if (car is Missing)
      {
        return null;
      }
      Type type = car == null ? typeof(double) : car.GetType();
      double result = Convert.ToDouble(car);
      if (args.Length == 0)
      {
        return result = 1.0 / result;
      }
      else
      {
        foreach (object item in args)
        {
          if (item is Double)
            type = item.GetType();

          result /= Convert.ToDouble(item);
        }
        return ConvertNumber(result, type);
      }
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
          try
          {
            return checked((int)first - (int)second);
          }
          catch (OverflowException)
          {
            return (BigInteger)(int)first - (int)second;
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
      if (first is BigInteger && second is BigInteger)
      {
        return BigInteger.Subtract((BigInteger)first, (BigInteger)second);
      }
      object value;
      if (OperatorHelper("op_Subtraction", first, second, out value))
      {
        return value;
      }

      //slow version
      return Subtract(first, new object[] { second });
    }

    [Builtin("-")]
    public static object Subtract(object car, params object[] args)
    {
      if (car is Missing)
      {
        return null;
      }
      Type type = car == null ? typeof(double) : car.GetType();
      double result = Convert.ToDouble(car);
      if (args.Length == 0)
      {
        result = -result;
      }
      else
      {
        foreach (object item in args)
        {
          if (item is Double)
            type = item.GetType();

          result -= Convert.ToDouble(item);
        }
      }
      return ConvertNumber(result, type);
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
      else if (obj is decimal)
      {
        return Math.Abs((decimal)obj);
      }
      else if (obj is Complex64)
      {
        return ((Complex64)obj).Abs();
      }
      else
      {
        double d = Convert.ToDouble(obj);
        return Math.Abs(d);
      }
    }

    /*
procedure:  (quotient n1 n2) 
procedure:  (remainder n1 n2) 
procedure:  (modulo n1 n2) 
These procedures implement number-theoretic (integer) division. n2 should be non-zero. All three procedures return integers. If n1/n2 is an integer: 

    (quotient n1 n2)           ===> n1/n2
    (remainder n1 n2)          ===> 0
    (modulo n1 n2)             ===> 0


If n1/n2 is not an integer: 

    (quotient n1 n2)           ===> nq
    (remainder n1 n2)          ===> nr
    (modulo n1 n2)             ===> nm


where nq is n1/n2 rounded towards zero, 0 < |nr| < |n2|, 0 < |nm| < |n2|, nr and nm differ from n1 by a multiple of n2, nr has the same sign as n1, and nm has the same sign as n2.

From this we can conclude that for integers n1 and n2 with n2 not equal to 0, 

     (= n1 (+ (* n2 (quotient n1 n2))
           (remainder n1 n2)))
                                         ===>  #t

provided all numbers involved in that computation are exact.


(modulo 13 4)                   ===>  1
(remainder 13 4)                ===>  1

(modulo -13 4)                  ===>  3
(remainder -13 4)               ===>  -1

(modulo 13 -4)                  ===>  -3
(remainder 13 -4)               ===>  1

(modulo -13 -4)                 ===>  -1
(remainder -13 -4)              ===>  -1

(remainder -13 -4.0)            ===>  -1.0  ; inexact

     */
#if !R6RS
    [Builtin("quotient")]
    public static object Quotient(object first, object second)
    {
      return Divide(first, second);
    }

    [Builtin("remainder")]
    public static object Remainder(object first, object second)
    {
      if (first is int)
      {
        if (second is int)
        {
          return (int)first % (int)second;
        }
        else if (second is double)
        {
          return (int)first % (double)second;
        }
      }
      if (first is double)
      {
        if (second is int)
        {
          return (double)first % (int)second;
        }
        else if (second is double)
        {
          return (double)first % (double)second;
        }
      }
      return Remainder(first, new object[] { second });
    }

    static object Remainder(object car, params object[] args)
    {
      if (car is Missing)
      {
        return null;
      }
      Type type = car == null ? typeof(decimal) : car.GetType();
      decimal result = Convert.ToDecimal(car);
      foreach (object item in args)
      {
        if (item is decimal)
          type = item.GetType();

        result %= Convert.ToDecimal(item);
      }
      return result;
    }
#endif

#if !R6RS
    [Builtin("modulo")]
    public static object Modulo(object first, object second)
    {
      if (first is int)
      {
        if (second is int)
        {
          return (int)first % (int)second;
        }
        else if (second is double)
        {
          return (int)first % (double)second;
        }
      }
      if (first is double)
      {
        if (second is int)
        {
          return (double)first % (int)second;
        }
        else if (second is double)
        {
          return (double)first % (double)second;
        }
      }
      object value;
      if (OperatorHelper("op_Modulus", first, second, out value))
      {
        return value;
      }
      return false;
    }
#endif

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

          if ((bool)IsEqualValue(second, 0))
          {
            return Abs(first);
          }
          else
          {
            return Abs(GreatestCommonDivider(second, Mod(first, second)));
          }
        default:
          // TODO
          return false;
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
          // TODO
          return false;
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
        double d = Convert.ToDouble(obj);
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
      double d1 = Convert.ToDouble(num1);
      double d2 = Convert.ToDouble(num2);
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

    [Builtin("sqrt")]
    public static object Sqrt(object obj)
    {
      return MathHelper(Math.Sqrt, obj);
    }

    [Builtin("expt")]
    public static object Expt(object obj1, object obj2)
    {
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
        if (obj is Fraction)
        {
          return (decimal)((Fraction)obj);
        }
        return Convert.ToDecimal(obj);
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
