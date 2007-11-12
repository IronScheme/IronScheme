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
    /*

     ; 6.3.4
- char?
- char=?
- char<?
- char>? 
- char<=? 
- char>=?

- char-ci=? 
- char-ci<? 
- char-ci>? 
- char-ci<=?
- char-ci>=? 

- char-alphabetic? 
- char-numeric?
- char-whitespace? 
- char-upper-case? 
- char-lower-case?

- char->integer 
- integer->char 

- char-upcase
- char-downcase

     */
    /// <summary>
    /// Determines whether the specified obj is char.
    /// </summary>
    /// <param name="obj">The obj.</param>
    /// <returns>
    /// 	<c>true</c> if the specified obj is char; otherwise, <c>false</c>.
    /// </returns>
    [Builtin("char?")]
    public static object IsChar(object obj)
    {
      return obj is char;
    }

    /// <summary>
    /// Determines whether [is same char] [the specified obj1].
    /// </summary>
    /// <param name="obj1">The obj1.</param>
    /// <param name="obj2">The obj2.</param>
    /// <returns>
    /// 	<c>true</c> if [is same char] [the specified obj1]; otherwise, <c>false</c>.
    /// </returns>
    [Builtin("char=?")]
    public static object IsSameChar(object obj1, object obj2)
    {
      char c1 = RequiresNotNull<char>(obj1);
      char c2 = RequiresNotNull<char>(obj2);

      return c1 == c2;
    }

    /// <summary>
    /// Determines whether [is less than char] [the specified obj1].
    /// </summary>
    /// <param name="obj1">The obj1.</param>
    /// <param name="obj2">The obj2.</param>
    /// <returns>
    /// 	<c>true</c> if [is less than char] [the specified obj1]; otherwise, <c>false</c>.
    /// </returns>
    [Builtin("char<?")]
    public static object IsLessThanChar(object obj1, object obj2)
    {
      char c1 = RequiresNotNull<char>(obj1);
      char c2 = RequiresNotNull<char>(obj2);

      return c1 < c2;
    }

    /// <summary>
    /// Determines whether [is greater than char] [the specified obj1].
    /// </summary>
    /// <param name="obj1">The obj1.</param>
    /// <param name="obj2">The obj2.</param>
    /// <returns>
    /// 	<c>true</c> if [is greater than char] [the specified obj1]; otherwise, <c>false</c>.
    /// </returns>
    [Builtin("char>?")]
    public static object IsGreaterThanChar(object obj1, object obj2)
    {
      char c1 = RequiresNotNull<char>(obj1);
      char c2 = RequiresNotNull<char>(obj2);

      return c1 > c2;
    }

    /// <summary>
    /// Determines whether [is less than or equal char] [the specified obj1].
    /// </summary>
    /// <param name="obj1">The obj1.</param>
    /// <param name="obj2">The obj2.</param>
    /// <returns>
    /// 	<c>true</c> if [is less than or equal char] [the specified obj1]; otherwise, <c>false</c>.
    /// </returns>
    [Builtin("char<=?")]
    public static object IsLessThanOrEqualChar(object obj1, object obj2)
    {
      char c1 = RequiresNotNull<char>(obj1);
      char c2 = RequiresNotNull<char>(obj2);

      return c1 <= c2;
    }

    [Builtin("char<=?")]
    public static object IsLessThanOrEqualChar(object obj1, object obj2, object obj3)
    {
      char c1 = RequiresNotNull<char>(obj1);
      char c2 = RequiresNotNull<char>(obj2);
      char c3 = RequiresNotNull<char>(obj3);

      return c1 <= c2 && c2 <= c3;
    }

    /// <summary>
    /// Determines whether [is greater than or equal char] [the specified obj1].
    /// </summary>
    /// <param name="obj1">The obj1.</param>
    /// <param name="obj2">The obj2.</param>
    /// <returns>
    /// 	<c>true</c> if [is greater than or equal char] [the specified obj1]; otherwise, <c>false</c>.
    /// </returns>
    [Builtin("char>=?")]
    public static object IsGreaterThanOrEqualChar(object obj1, object obj2)
    {
      char c1 = RequiresNotNull<char>(obj1);
      char c2 = RequiresNotNull<char>(obj2);

      return c1 >= c2;
    }

    /// <summary>
    /// Determines whether [is same char case insensitive] [the specified obj1].
    /// </summary>
    /// <param name="obj1">The obj1.</param>
    /// <param name="obj2">The obj2.</param>
    /// <returns>
    /// 	<c>true</c> if [is same char case insensitive] [the specified obj1]; otherwise, <c>false</c>.
    /// </returns>
    [Builtin("char-ci=?")]
    public static object IsSameCharCaseInsensitive(object obj1, object obj2)
    {
      char c1 = RequiresNotNull<char>(obj1);
      char c2 = RequiresNotNull<char>(obj2);

      return char.ToLower(c1) == char.ToLower(c2);
    }

    /// <summary>
    /// Determines whether [is less than char case insensitive] [the specified obj1].
    /// </summary>
    /// <param name="obj1">The obj1.</param>
    /// <param name="obj2">The obj2.</param>
    /// <returns>
    /// 	<c>true</c> if [is less than char case insensitive] [the specified obj1]; otherwise, <c>false</c>.
    /// </returns>
    [Builtin("char-ci<?")]
    public static object IsLessThanCharCaseInsensitive(object obj1, object obj2)
    {
      char c1 = RequiresNotNull<char>(obj1);
      char c2 = RequiresNotNull<char>(obj2);

      return char.ToLower(c1) < char.ToLower(c2);
    }

    /// <summary>
    /// Determines whether [is greater than char case insensitive] [the specified obj1].
    /// </summary>
    /// <param name="obj1">The obj1.</param>
    /// <param name="obj2">The obj2.</param>
    /// <returns>
    /// 	<c>true</c> if [is greater than char case insensitive] [the specified obj1]; otherwise, <c>false</c>.
    /// </returns>
    [Builtin("char-ci>?")]
    public static object IsGreaterThanCharCaseInsensitive(object obj1, object obj2)
    {
      char c1 = RequiresNotNull<char>(obj1);
      char c2 = RequiresNotNull<char>(obj2); 
      
      return char.ToLower(c1) > char.ToLower(c2);
    }

    /// <summary>
    /// Determines whether [is less than or equal char case insensitive] [the specified obj1].
    /// </summary>
    /// <param name="obj1">The obj1.</param>
    /// <param name="obj2">The obj2.</param>
    /// <returns>
    /// 	<c>true</c> if [is less than or equal char case insensitive] [the specified obj1]; otherwise, <c>false</c>.
    /// </returns>
    [Builtin("char-ci<=?")]
    public static object IsLessThanOrEqualCharCaseInsensitive(object obj1, object obj2)
    {
      char c1 = RequiresNotNull<char>(obj1);
      char c2 = RequiresNotNull<char>(obj2);

      return char.ToLower(c1) <= char.ToLower(c2);
    }

    /// <summary>
    /// Determines whether [is greater than or equal char case insensitive] [the specified obj1].
    /// </summary>
    /// <param name="obj1">The obj1.</param>
    /// <param name="obj2">The obj2.</param>
    /// <returns>
    /// 	<c>true</c> if [is greater than or equal char case insensitive] [the specified obj1]; otherwise, <c>false</c>.
    /// </returns>
    [Builtin("char-ci>=?")]
    public static object IsGreaterThanOrEqualCharCaseInsensitive(object obj1, object obj2)
    {
      char c1 = RequiresNotNull<char>(obj1);
      char c2 = RequiresNotNull<char>(obj2);

      return char.ToLower(c1) >= char.ToLower(c2);
    }

    /// <summary>
    /// Determines whether [is alphabetic char] [the specified obj].
    /// </summary>
    /// <param name="obj">The obj.</param>
    /// <returns>
    /// 	<c>true</c> if [is alphabetic char] [the specified obj]; otherwise, <c>false</c>.
    /// </returns>
    [Builtin("char-alphabetic?")]
    public static object IsAlphabeticChar(object obj)
    {
      char c = RequiresNotNull<char>(obj);
      return char.IsLetter(c);
    }

    /// <summary>
    /// Determines whether [is numeric char] [the specified obj].
    /// </summary>
    /// <param name="obj">The obj.</param>
    /// <returns>
    /// 	<c>true</c> if [is numeric char] [the specified obj]; otherwise, <c>false</c>.
    /// </returns>
    [Builtin("char-numeric?")]
    public static object IsNumericChar(object obj)
    {
      char c = RequiresNotNull<char>(obj);
      return char.IsDigit(c);
    }

    /// <summary>
    /// Determines whether [is whitespace char] [the specified obj].
    /// </summary>
    /// <param name="obj">The obj.</param>
    /// <returns>
    /// 	<c>true</c> if [is whitespace char] [the specified obj]; otherwise, <c>false</c>.
    /// </returns>
    [Builtin("char-whitespace?")]
    public static object IsWhitespaceChar(object obj)
    {
      char c = RequiresNotNull<char>(obj);
      return char.IsWhiteSpace(c);
    }

    /// <summary>
    /// Determines whether [is upper case char] [the specified obj].
    /// </summary>
    /// <param name="obj">The obj.</param>
    /// <returns>
    /// 	<c>true</c> if [is upper case char] [the specified obj]; otherwise, <c>false</c>.
    /// </returns>
    [Builtin("char-upper-case?")]
    public static object IsUpperCaseChar(object obj)
    {
      char c = RequiresNotNull<char>(obj);
      return char.IsUpper(c);
    }

    /// <summary>
    /// Determines whether [is lower case char] [the specified obj].
    /// </summary>
    /// <param name="obj">The obj.</param>
    /// <returns>
    /// 	<c>true</c> if [is lower case char] [the specified obj]; otherwise, <c>false</c>.
    /// </returns>
    [Builtin("char-lower-case?")]
    public static object IsLowerCaseChar(object obj)
    {
      char c = RequiresNotNull<char>(obj);
      return char.IsLower(c);
    }

    /// <summary>
    /// Chars to integer.
    /// </summary>
    /// <param name="obj">The obj.</param>
    /// <returns></returns>
    [Builtin("char->integer")]
    public static object CharToInteger(object obj)
    {
      char c = RequiresNotNull<char>(obj);
      return (int)c;
    }

    /// <summary>
    /// Integers to char.
    /// </summary>
    /// <param name="obj">The obj.</param>
    /// <returns></returns>
    [Builtin("integer->char")]
    public static object IntegerToChar(object obj)
    {
      int i = RequiresNotNull<int>(obj);
      return (char)i;
    }

    /// <summary>
    /// Toes the upper case char.
    /// </summary>
    /// <param name="obj">The obj.</param>
    /// <returns></returns>
    [Builtin("char-upcase")]
    public static object ToUpperCaseChar(object obj)
    {
      char c = RequiresNotNull<char>(obj);
      return char.ToUpper(c);
    }

    /// <summary>
    /// Toes the lower case char.
    /// </summary>
    /// <param name="obj">The obj.</param>
    /// <returns></returns>
    [Builtin("char-downcase")]
    public static object ToLowerCaseChar(object obj)
    {
      char c = RequiresNotNull<char>(obj);
      return char.ToLower(c);
    }

  }
}
