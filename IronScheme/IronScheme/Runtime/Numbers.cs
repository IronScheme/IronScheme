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

namespace IronScheme.Runtime
{
  public partial class Builtins
  {
    [Builtin("number->string")]
    public static string NumberToString(object obj)
    {
      return NumberToString(obj, 10);
    }

    [Builtin("number->string")]
    public static string NumberToString(object obj, object radix)
    {
      string str = RequiresNotNull<string>(obj);
      radix = radix ?? 10;
      int r = (int)radix;

      throw new NotImplementedException();
    }

    [Builtin("string->number")]
    public static object StringToNumber(object obj)
    {
      string str = RequiresNotNull<string>(obj);

      if (str.Length == 0)
      {
        throw new ArgumentException("string cannot be empty");
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
              throw new ArgumentException("unknown radix");
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

    [Builtin("string->number")]
    public static object StringToNumber(object obj, object radix)
    {
      string str = RequiresNotNull<string>(obj);
      radix = radix ?? 10;
      int r = (int)radix;

      if (str.Length == 0)
      {
        throw new ArgumentException("string cannot be empty");
      }

      switch (r)
      {
        case 2:
          return ParseBinary(str);
        case 8:
          return ParseOctal(str);
        case 10:
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
          break;
        case 16:
          if (str.Length > 16)
          {
            throw new NotSupportedException("big numbers not supported yet");
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
          throw new ArgumentException("unknown radix");
      }

      throw new NotImplementedException();
    }


    [Builtin("number?")]
    public static bool IsNumber(object obj)
    {
      return 
        (  IsComplex(obj) 
        || IsRational(obj)
        || IsReal(obj) 
        || IsInteger(obj));
    }

    [Builtin("complex?")]
    public static bool IsComplex(object obj)
    {
      return obj is Complex64;
    }

    [Builtin("real?")]
    public static bool IsReal(object obj)
    {
      return obj is float || obj is double;
    }

    [Builtin("rational?")]
    public static bool IsRational(object obj)
    {
      return false;
    }

    [Builtin("integer?")]
    public static bool IsInteger(object obj)
    {
      return (obj is int || obj is long);
    }

    [Builtin("exact?")]
    public static bool IsExact(object obj)
    {
      return IsInteger(obj) || IsRational(obj);
    }

    [Builtin("inexact?")]
    public static bool IsInexact(object obj)
    {
      return IsReal(obj) || IsComplex(obj); 
    }


    #region relations

    [Builtin("=")]
    public static bool IsSame(object first, object second)
    {
      return IsEqualValue(first, second);
    }

    [Builtin("=")]
    public static bool IsSame(object first, params object[] rest)
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
    public static bool IsLessThan(object first, object second)
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
    public static bool IsLessThan(object first, params object[] rest)
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
    public static bool IsLessThanOrEqual(object first, object second)
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
    public static bool IsLessThanOrEqual(object first, params object[] rest)
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
    public static bool IsGreaterThan(object first, object second)
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
    public static bool IsGreaterThan(object first, params object[] rest)
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
    public static bool IsGreaterThanOrEqual(object first, object second)
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
    public static bool IsGreaterThanOrEqual(object first, params object[] rest)
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
    public static bool IsZero(object obj)
    {
      return IsEqualValue(obj, 0);
    }

    [Builtin("positive?")]
    public static bool IsPositive(object obj)
    {
      return IsGreaterThan(obj, 0);
    }

    [Builtin("negative?")]
    public static bool IsNegative(object obj)
    {
      return IsLessThan(obj, 0);
    }

    [Builtin("odd?")]
    public static bool IsOdd(object obj)
    {
      return !IsEqualValue(Modulo(obj, 2), 0);
    }

    [Builtin("even?")]
    public static bool IsEven(object obj)
    {
      return IsEqualValue(Modulo(obj, 2), 0);
    }

    [Builtin("min")]
    public static object Min(object first, params object[] rest)
    {
      object min = first;
      foreach (object var in rest)
      {
        if (IsLessThan(var, min))
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
        if (IsGreaterThan(var, max))
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
          return (int)first + (int)second;
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
      if (car is Missing)
      {
        return 0;
      }
      Type type = car == null ? typeof(double) : car.GetType();
      double result = Convert.ToDouble(car);
      foreach (object item in args)
      {
        if (item is Double)
          type = item.GetType();

        result += Convert.ToDouble(item);
      }
      return Convert.ChangeType(result, type);
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
          return (int)first * (int)second;
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
      return Convert.ChangeType(result, type);
    }

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
        return Convert.ChangeType(result, type);
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
            if (pis[0].ParameterType.IsInstanceOfType(first)
              && pis[1].ParameterType.IsInstanceOfType(second))
            {
              value = mi.Invoke(null, new object[] { first, second });
              return true;
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
            if (pis[0].ParameterType.IsInstanceOfType(first)
              && pis[1].ParameterType.IsInstanceOfType(second))
            {
              value = mi.Invoke(null, new object[] { first, second });
              return true;
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
          return (int)first - (int)second;
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
      return Convert.ChangeType(result, type);
    }

#if EXT_LIB
    //[Builtin("&")]
    //public static object logand(object first, object second)
    //{
    //  return logand(first, new object[] { second });
    //}

    //[Builtin("&")]
    //public static object logand(object first, params object[] rest)
    //{
    //  Type type = first.GetType();
    //  object result = first;
    //  foreach (object item in rest)
    //  {
    //    // The integral types dont define operator overload methods
    //    // for performace reasons, so we have to implement this
    //    // operator on each integral type

    //    if (type == typeof(bool))
    //      result = (bool)result & (bool)(item);
    //    else if (type == typeof(sbyte))
    //      result = (sbyte)result & (sbyte)(item);
    //    else if (type == typeof(byte))
    //      result = (byte)result & (byte)(item);
    //    else if (type == typeof(char))
    //      result = (char)result & (char)(item);
    //    else if (type == typeof(short))
    //      result = (short)result & (short)(item);
    //    else if (type == typeof(ushort))
    //      result = (ushort)result & (ushort)(item);
    //    else if (type == typeof(int))
    //      result = (int)result & (int)(item);
    //    else if (type == typeof(uint))
    //      result = (uint)result & (uint)(item);
    //    else if (type == typeof(long))
    //      result = (long)result & (long)(item);
    //    else if (type == typeof(ulong))
    //      result = (ulong)result & (ulong)(item);
    //  }

    //  return Convert.ChangeType(result, type);
    //}

    //[Builtin("|")]
    //public static object logor(object first, object second)
    //{
    //  return logor(first, new object[] { second });
    //}

    //[Builtin("|")]
    //public static object logor(object first, params object[] rest)
    //{
    //  Type type = first.GetType();
    //  object result = first;
    //  foreach (object item in rest)
    //  {

    //    // The integral types dont define operator overload methods
    //    // for performace reasons, so we have to implement this
    //    // operator on each integral type

    //    if (type == typeof(bool))
    //      result = (bool)result | (bool)(item);
    //    else if (type == typeof(sbyte))
    //      result = (sbyte)result | (sbyte)(item);
    //    else if (type == typeof(byte))
    //      result = (byte)result | (byte)(item);
    //    else if (type == typeof(char))
    //      result = (char)result | (char)(item);
    //    else if (type == typeof(short))
    //      result = (short)result | (short)(item);
    //    else if (type == typeof(ushort))
    //      result = (ushort)result | (ushort)(item);
    //    else if (type == typeof(int))
    //      result = (int)result | (int)(item);
    //    else if (type == typeof(uint))
    //      result = (uint)result | (uint)(item);
    //    else if (type == typeof(long))
    //      result = (long)result | (long)(item);
    //    else if (type == typeof(ulong))
    //      result = (ulong)result | (ulong)(item);
    //  }

    //  return Convert.ChangeType(result, type);
    //}

    //[Builtin("^")]
    //public static object logxor(object first, object second)
    //{
    //  return logxor(first, new object[] { second });
    //}

    ///// <summary>
    ///// (^ expression*)
    ///// Performs a bitwise logical exclusive or operation on its arguments
    ///// </summary>
    ///// <param name="args"></param>
    ///// <param name="environment"></param>
    ///// <returns></returns>
    //[Builtin("^")]
    //public static object logxor(object first, params object[] rest)
    //{
    //  Type type = first.GetType();
    //  object result = first;
    //  foreach (object item in rest)
    //  {

    //    // The integral types dont define operator overload methods
    //    // for performace reasons, so we have to implement this
    //    // operator on each integral type
    //    if (type == typeof(bool))
    //      result = (bool)result ^ (bool)(item);
    //    else if (type == typeof(sbyte))
    //      result = (sbyte)result ^ (sbyte)(item);
    //    else if (type == typeof(byte))
    //      result = (byte)result ^ (byte)(item);
    //    else if (type == typeof(char))
    //      result = (char)result ^ (char)(item);
    //    else if (type == typeof(short))
    //      result = (short)result ^ (short)(item);
    //    else if (type == typeof(ushort))
    //      result = (ushort)result ^ (ushort)(item);
    //    else if (type == typeof(int))
    //      result = (int)result ^ (int)(item);
    //    else if (type == typeof(uint))
    //      result = (uint)result ^ (uint)(item);
    //    else if (type == typeof(long))
    //      result = (long)result ^ (long)(item);
    //    else if (type == typeof(ulong))
    //      result = (ulong)result ^ (ulong)(item);

    //  }

    //  return Convert.ChangeType(result, type);
    //}

#endif
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


    [Builtin("modulo")]
    public static object Modulo(object first, object second)
    {
      return (int)first % (int)second;
    }

  
    static object Remainder(object car, params object[] args)
    {
      if (car is Missing)
      {
        return null;
      }
      Type type = car == null ? typeof(double) : car.GetType();
      double result = Convert.ToDouble(car);
      foreach (object item in args)
      {
        if (item is Double)
          type = item.GetType();

        result %= Convert.ToDouble(item);
      }
      return Convert.ChangeType(result, type);
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
        case 2:
          object first = args[0], second = args[1];

          if (IsEqualValue(second, 0))
          {
            return Abs(first);
          }
          else
          {
            return Abs(GreatestCommonDivider(second, Remainder(first, second)));
          }
        default:
          throw new NotImplementedException();
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
        case 2:
          object first = args[0], second = args[1];

          return Abs(Multiply(Divide(first, GreatestCommonDivider(first, second)), second));
        default:
          throw new NotImplementedException();
      }
    }

    [Builtin("numerator")]
    public static object Numerator(object obj)
    {
      throw new NotImplementedException();
    }

    [Builtin("denominator")]
    public static object Denominator(object obj)
    {
      throw new NotImplementedException();
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
      throw new NotImplementedException();
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
      throw new NotImplementedException();
    }

    [Builtin("make-polar")]
    public static object MakePolar(object obj1, object obj2)
    {
      throw new NotImplementedException();
    }

    [Builtin("real-part")]
    public static object RealPart(object obj)
    {
      throw new NotImplementedException();
    }

    [Builtin("imag-part")]
    public static object ImagPart(object obj)
    {
      throw new NotImplementedException();
    }

    [Builtin("magnitude")]
    public static object Magnitude(object obj)
    {
      throw new NotImplementedException();
    }

    [Builtin("angle")]
    public static object Angle(object obj)
    {
      throw new NotImplementedException();
    }
    

    [Builtin("exact->inexact")]
    public static object ExactToInexact(object obj)
    {
      throw new NotImplementedException();
    }
    [Builtin("inexact->exact")]
    public static object InexactToExact(object obj)
    {
      throw new NotImplementedException();
    }

  }
}
