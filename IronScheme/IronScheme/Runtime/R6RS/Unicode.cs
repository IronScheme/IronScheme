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
using Microsoft.Scripting.Generation;
using Microsoft.Scripting;
using System.Reflection;
using Microsoft.Scripting.Utils;
using System.Reflection.Emit;
using System.Collections;
using System.Globalization;

namespace IronScheme.Runtime.R6RS
{
  public class Unicode : Builtins
  {
    static TextInfo textinfo = CultureInfo.InvariantCulture.TextInfo;
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
      char c = RequiresNotNull<char>(obj);
      return textinfo.ToTitleCase(c.ToString())[0];
    }

    [Builtin("char-foldcase")]
    public static object ToFoldCaseChar(object obj)
    {
      //TODO
      char c = RequiresNotNull<char>(obj);
      return FALSE;
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

      return GetBool( char.ToLower(c1) == char.ToLower(c2));
    }

    [Builtin("char-ci=?")]
    public static object IsSameCharCaseInsensitive(object obj1, object obj2, object obj3, params object[] rest)
    {
      char c1 = char.ToLower(RequiresNotNull<char>(obj1));
      char c2 = char.ToLower(RequiresNotNull<char>(obj2));
      char c3 = char.ToLower(RequiresNotNull<char>(obj3));

      bool head = c1 == c2 && c2 == c3;

      if (head)
      {
        foreach (char c in rest)
        {
          char ci = char.ToLower(c);
          if (c3 == ci)
          {
            c3 = ci;
          }
          else
          {
            return FALSE;
          }
        }
      }

      return GetBool( head);
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

      return GetBool( char.ToLower(c1) < char.ToLower(c2));
    }


    [Builtin("char-ci<?")]
    public static object IsLessThanCharCaseInsensitive(object obj1, object obj2, object obj3, params object[] rest)
    {
      char c1 = char.ToLower(RequiresNotNull<char>(obj1));
      char c2 = char.ToLower(RequiresNotNull<char>(obj2));
      char c3 = char.ToLower(RequiresNotNull<char>(obj3));

      bool head = c1 < c2 && c2 < c3;

      if (head)
      {
        foreach (char c in rest)
        {
          char ci = char.ToLower(c);
          if (c3 < ci)
          {
            c3 = ci;
          }
          else
          {
            return FALSE;
          }
        }
      }

      return GetBool( head);
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

      return GetBool( char.ToLower(c1) > char.ToLower(c2));
    }

    [Builtin("char-ci>?")]
    public static object IsGreaterThanCharCaseInsensitive(object obj1, object obj2, object obj3, params object[] rest)
    {
      char c1 = char.ToLower(RequiresNotNull<char>(obj1));
      char c2 = char.ToLower(RequiresNotNull<char>(obj2));
      char c3 = char.ToLower(RequiresNotNull<char>(obj3));

      bool head = c1 > c2 && c2 > c3;

      if (head)
      {
        foreach (char c in rest)
        {
          char ci = char.ToLower(c);
          if (c3 > ci)
          {
            c3 = ci;
          }
          else
          {
            return FALSE;
          }
        }
      }

      return GetBool( head);
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

      return GetBool( char.ToLower(c1) <= char.ToLower(c2));
    }

    [Builtin("char-ci<=?")]
    public static object IsLessThanOrEqualCharCaseInsensitive(object obj1, object obj2, object obj3, params object[] rest)
    {
      char c1 = char.ToLower(RequiresNotNull<char>(obj1));
      char c2 = char.ToLower(RequiresNotNull<char>(obj2));
      char c3 = char.ToLower(RequiresNotNull<char>(obj3));

      bool head = c1 <= c2 && c2 <= c3;

      if (head)
      {
        foreach (char c in rest)
        {
          char ci = char.ToLower(c);
          if (c3 <= ci)
          {
            c3 = ci;
          }
          else
          {
            return FALSE;
          }
        }
      }

      return GetBool( head);
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

      return GetBool( char.ToLower(c1) >= char.ToLower(c2));
    }

    [Builtin("char-ci>=?")]
    public static object IsGreaterThanOrEqualCharCaseInsensitive(object obj1, object obj2, object obj3, params object[] rest)
    {
      char c1 = char.ToLower(RequiresNotNull<char>(obj1));
      char c2 = char.ToLower(RequiresNotNull<char>(obj2));
      char c3 = char.ToLower(RequiresNotNull<char>(obj3));

      bool head = c1 >= c2 && c2 >= c3;

      if (head)
      {
        foreach (char c in rest)
        {
          char ci = char.ToLower(c);
          if (c3 >= ci)
          {
            c3 = ci;
          }
          else
          {
            return FALSE;
          }
        }
      }

      return GetBool( head);
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
      return GetBool( char.IsLetter(c));
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
      return GetBool( char.IsDigit(c));
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
      return GetBool( char.IsWhiteSpace(c));
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
      return GetBool( char.IsUpper(c));
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
      return GetBool( char.IsLower(c));
    }

    [Builtin("char-title-case?")]
    public static object IsTitleCaseChar(object obj)
    {
      char c = RequiresNotNull<char>(obj);
      return IsEqualValue(obj, ToUpperCaseChar(obj));
    }

    [Builtin("char-general-category")]
    public static object CharGeneralCategory(object obj)
    {
      char c = RequiresNotNull<char>(obj);
      switch (char.GetUnicodeCategory(c))
      {
        case UnicodeCategory.ClosePunctuation:          return SymbolTable.StringToId("Pe");
        case UnicodeCategory.ConnectorPunctuation:      return SymbolTable.StringToId("Pc");
        case UnicodeCategory.Control:                   return SymbolTable.StringToId("Cc");
        case UnicodeCategory.CurrencySymbol:            return SymbolTable.StringToId("Sc");
        case UnicodeCategory.DashPunctuation:           return SymbolTable.StringToId("Pd");
        case UnicodeCategory.DecimalDigitNumber:        return SymbolTable.StringToId("Nd");
        case UnicodeCategory.EnclosingMark:             return SymbolTable.StringToId("Me");
        case UnicodeCategory.FinalQuotePunctuation:     return SymbolTable.StringToId("Pf");
        case UnicodeCategory.Format:                    return SymbolTable.StringToId("Cf");
        case UnicodeCategory.InitialQuotePunctuation:   return SymbolTable.StringToId("Pi");
        case UnicodeCategory.LetterNumber:              return SymbolTable.StringToId("Nl");
        case UnicodeCategory.LineSeparator:             return SymbolTable.StringToId("Zl");
        case UnicodeCategory.LowercaseLetter:           return SymbolTable.StringToId("Ll");
        case UnicodeCategory.MathSymbol:                return SymbolTable.StringToId("Sm");
        case UnicodeCategory.ModifierLetter:            return SymbolTable.StringToId("Lm");
        case UnicodeCategory.ModifierSymbol:            return SymbolTable.StringToId("Sk");
        case UnicodeCategory.NonSpacingMark:            return SymbolTable.StringToId("Mn");
        case UnicodeCategory.OpenPunctuation:           return SymbolTable.StringToId("Ps");
        case UnicodeCategory.OtherLetter:               return SymbolTable.StringToId("Lo");
        case UnicodeCategory.OtherNotAssigned:          return SymbolTable.StringToId("Cn");
        case UnicodeCategory.OtherNumber:               return SymbolTable.StringToId("No");
        case UnicodeCategory.OtherPunctuation:          return SymbolTable.StringToId("Po");
        case UnicodeCategory.OtherSymbol:               return SymbolTable.StringToId("So");
        case UnicodeCategory.ParagraphSeparator:        return SymbolTable.StringToId("Zp");
        case UnicodeCategory.PrivateUse:                return SymbolTable.StringToId("Co");
        case UnicodeCategory.SpaceSeparator:            return SymbolTable.StringToId("Zs");
        case UnicodeCategory.SpacingCombiningMark:      return SymbolTable.StringToId("Mc");
        case UnicodeCategory.Surrogate:                 return SymbolTable.StringToId("Cs");
        case UnicodeCategory.TitlecaseLetter:           return SymbolTable.StringToId("Lt");
        case UnicodeCategory.UppercaseLetter:           return SymbolTable.StringToId("Lu");
      }
      return FALSE;
    }

    [Builtin("string-ci=?")]
    public static object IsSameStringCaseInsensitive(object obj1, object obj2)
    {
      string s1 = ToLower(obj1);
      string s2 = ToLower(obj2);

      return GetBool( s1 == s2);
    }

    [Builtin("string-ci=?")]
    public static object IsSameStringCaseInsensitive(object obj1, object obj2, object obj3, params object[] rest)
    {
      string s1 = ToLower(obj1);
      string s2 = ToLower(obj2);
      string s3 = ToLower(obj3);

      bool head = s1 == s2 && s2 == s3;

      if (head)
      {
        foreach (object s in rest)
        {
          string ss = ToLower(s);
          if (ss == s3)
          {
            s3 = ss;
          }
          else
          {
            return FALSE;
          }
        }
      }
      return GetBool( head);
    }

    [Builtin("string-ci<?")]
    public static object IsLessThanStringCaseInsensitive(object obj1, object obj2)
    {
      string s1 = ToLower(obj1);
      string s2 = ToLower(obj2);

      return IsLessThanString(s1, s2);
    }


    [Builtin("string-ci<?")]
    public static object IsLessThanStringCaseInsensitive(object obj1, object obj2, object obj3, params object[] rest)
    {
      string s1 = ToLower(obj1);
      string s2 = ToLower(obj2);
      string s3 = ToLower(obj3);

      bool head = IsTrue(IsLessThanString(s1, s2)) && IsTrue(IsLessThanString(s2, s3));

      if (head)
      {
        foreach (object s in rest)
        {
          string ss = ToLower(s);
          if (IsTrue(IsLessThanString(ss, s3)))
          {
            s3 = ss;
          }
          else
          {
            return FALSE;
          }
        }
      }
      return GetBool( head);
    }

    [Builtin("string-ci>?")]
    public static object IsGreaterThanStringCaseInsensitive(object obj1, object obj2)
    {
      string s1 = ToLower(obj1);
      string s2 = ToLower(obj2);

      return IsGreaterThanString(s1, s2);
    }

    [Builtin("string-ci>?")]
    public static object IsGreaterThanStringCaseInsensitive(object obj1, object obj2, object obj3, params object[] rest)
    {
      string s1 = ToLower(obj1);
      string s2 = ToLower(obj2);
      string s3 = ToLower(obj3);

      bool head = IsTrue(IsGreaterThanString(s1, s2)) && IsTrue(IsGreaterThanString(s2, s3));

      if (head)
      {
        foreach (object s in rest)
        {
          string ss = ToLower(s);
          if (IsTrue(IsGreaterThanString(ss, s3)))
          {
            s3 = ss;
          }
          else
          {
            return FALSE;
          }
        }
      }
      return GetBool( head);
    }

    [Builtin("string-ci<=?")]
    public static object IsLessThanOrEqualStringCaseInsensitive(object obj1, object obj2)
    {
      string s1 = ToLower(obj1);
      string s2 = ToLower(obj2);

      return IsLessThanOrEqualString(s1, s2);
    }

    [Builtin("string-ci<=?")]
    public static object IsLessThanOrEqualStringCaseInsensitive(object obj1, object obj2, object obj3, params object[] rest)
    {
      string s1 = ToLower(obj1);
      string s2 = ToLower(obj2);
      string s3 = ToLower(obj3);

      bool head = IsTrue(IsLessThanOrEqualString(s1, s2)) && IsTrue(IsLessThanOrEqualString(s2, s3));

      if (head)
      {
        foreach (object s in rest)
        {
          string ss = ToLower(s);
          if (IsTrue(IsLessThanOrEqualString(ss, s3)))
          {
            s3 = ss;
          }
          else
          {
            return FALSE;
          }
        }
      }
      return GetBool( head);
    }

    [Builtin("string-ci>=?")]
    public static object IsGreaterThanOrEqualStringCaseInsensitive(object obj1, object obj2)
    {
      string s1 = ToLower(obj1);
      string s2 = ToLower(obj2);

      return IsGreaterThanOrEqualString(s1, s2);
    }

    [Builtin("string-ci>=?")]
    public static object IsGreaterThanOrEqualStringCaseInsensitive(object obj1, object obj2, object obj3, params object[] rest)
    {
      string s1 = ToLower(obj1);
      string s2 = ToLower(obj2);
      string s3 = ToLower(obj3);

      bool head = IsTrue(IsGreaterThanOrEqualString(s1, s2)) && IsTrue(IsGreaterThanOrEqualString(s2, s3));

      if (head)
      {
        foreach (object s in rest)
        {
          string ss = ToLower(s);
          if (IsTrue(IsGreaterThanOrEqualString(ss, s3)))
          {
            s3 = ss;
          }
          else
          {
            return FALSE;
          }
        }
      }
      return GetBool( head);
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
      string s = GetString(obj);
      return textinfo.ToTitleCase(s);
    }

    [Builtin("string-foldcase")]
    public static object ToFoldCaseString(object obj)
    {
      //TODO
      string s = GetString(obj);
      return FALSE;
    }

    [Builtin("string-normalize-nfd")]
    public static object StringNormalizeNFD(object obj)
    {
      string s = GetString(obj);
      return s.Normalize(NormalizationForm.FormD);
    }

    [Builtin("string-normalize-nfkd")]
    public static object StringNormalizeNFKD(object obj)
    {
      string s = GetString(obj);
      return s.Normalize(NormalizationForm.FormKD);
    }
    
    [Builtin("string-normalize-nfc")]
    public static object StringNormalizeNFC(object obj)
    {
      string s = GetString(obj);
      return s.Normalize(NormalizationForm.FormC);
    }


    [Builtin("string-normalize-nfkc")]
    public static object StringNormalizeNFKC(object obj)
    {
      string s = GetString(obj);
      return s.Normalize(NormalizationForm.FormKC);
    }
  }
}

