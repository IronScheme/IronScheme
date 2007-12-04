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

#if R6RS
using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting.Generation;
using Microsoft.Scripting;
using System.Reflection;
using Microsoft.Scripting.Utils;
using System.Reflection.Emit;
using System.Collections;

namespace IronScheme.Runtime.R6RS
{
  public class Unicode : Builtins
  {
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

    [Builtin("char-titlecase")]
    public static object ToTitleCaseChar(object obj)
    {
      //TODO
      char c = RequiresNotNull<char>(obj);
      return false;
    }

    [Builtin("char-foldcase")]
    public static object ToFoldCaseChar(object obj)
    {
      //TODO
      char c = RequiresNotNull<char>(obj);
      return false;
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

    [Builtin("char-title-case?")]
    public static object IsTitleCaseChar(object obj)
    {
      char c = RequiresNotNull<char>(obj);
      //TODO
      return false;
    }

    [Builtin("char-general-catergory")]
    public static object CharGeneralCategory(object obj)
    {
      char c = RequiresNotNull<char>(obj);
      //TODO
      return false;
    }

    [Builtin("string-ci=?")]
    public static object IsSameStringCaseInsensitive(object obj1, object obj2)
    {
      string s1 = ToLower(GetString(obj1));
      string s2 = ToLower(GetString(obj2));

      return s1 == s2;
    }

    [Builtin("string-ci<?")]
    public static object IsLessThanStringCaseInsensitive(object obj1, object obj2)
    {
      string s1 = ToLower(GetString(obj1));
      string s2 = ToLower(GetString(obj2));

      return IsLessThan(s1, s2);
    }

    [Builtin("string-ci>?")]
    public static object IsGreaterThanStringCaseInsensitive(object obj1, object obj2)
    {
      string s1 = ToLower(GetString(obj1));
      string s2 = ToLower(GetString(obj2));

      return IsGreaterThan(s1, s2);
    }

    [Builtin("string-ci<=?")]
    public static object IsLessThanOrEqualStringCaseInsensitive(object obj1, object obj2)
    {
      string s1 = ToLower(GetString(obj1));
      string s2 = ToLower(GetString(obj2));

      return IsLessThanOrEqual(s1, s2);
    }

    [Builtin("string-ci>=?")]
    public static object IsGreaterThanOrEqualStringCaseInsensitive(object obj1, object obj2)
    {
      string s1 = ToLower(GetString(obj1));
      string s2 = ToLower(GetString(obj2));

      return IsGreaterThanOrEqual(s1, s2);
    }



    [Builtin("string-upcase")]
    public static object ToUpperCaseString(object obj)
    {
      string s = GetString(obj);
      return s.ToUpper();
    }

    /// <summary>
    /// Toes the lower case char.
    /// </summary>
    /// <param name="obj">The obj.</param>
    /// <returns></returns>
    [Builtin("string-downcase")]
    public static object ToLowerCaseString(object obj)
    {
      string s = GetString(obj);
      return s.ToLower();
    }

    [Builtin("string-titlecase")]
    public static object ToTitleCaseString(object obj)
    {
      //TODO
      string s = GetString(obj);
      return false;
    }

    [Builtin("string-foldcase")]
    public static object ToFoldCaseString(object obj)
    {
      //TODO
      string s = GetString(obj);
      return false;
    }

    [Builtin("string-normalize-nfd")]
    public static object StringNormalizeNFD(object obj)
    {
      //TODO
      string s = GetString(obj);
      return false;
    }

    [Builtin("string-normalize-nfkd")]
    public static object StringNormalizeNFKD(object obj)
    {
      //TODO
      string s = GetString(obj);
      return false;
    }
    
    [Builtin("string-normalize-nfc")]
    public static object StringNormalizeNFC(object obj)
    {
      //TODO
      string s = GetString(obj);
      return false;
    }


    [Builtin("string-normalize-nfkc")]
    public static object StringNormalizeNFKC(object obj)
    {
      //TODO
      string s = GetString(obj);
      return false;
    }
  }
}
#endif
