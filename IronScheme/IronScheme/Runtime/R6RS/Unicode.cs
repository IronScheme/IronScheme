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
      char c = RequiresNotNull<char>(obj);
      return char.ToLowerInvariant(char.ToUpperInvariant(c));
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
      return IsEqualValue(obj, ToTitleCaseChar(obj));
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

    static readonly CompareInfo compare = CultureInfo.InvariantCulture.CompareInfo;

    [Builtin("string-ci-compare")]
    public static object StringCaseInsensitiveCompare(object obj1, object obj2)
    {
      string s1 = RequiresNotNull<string>(obj1);
      string s2 = RequiresNotNull<string>(obj2);

      return compare.Compare(s1, s2, CompareOptions.IgnoreCase);
    }

    [Builtin("string-upcase")]
    public static object ToUpperCaseString(object obj)
    {
      string s = RequiresNotNull<string>(obj);
      return s.ToUpper().Replace("ß", "SS");
    }

    /// <summary>
    /// Toes the lower case char.
    /// </summary>
    /// <param name="obj">The obj.</param>
    /// <returns></returns>
    [Builtin("string-downcase")]
    public static object ToLowerCaseString(object obj)
    {
      string s = RequiresNotNull<string>(obj);
      StringBuilder sb = new StringBuilder(s.ToLower());
      for (int i = 0; i < sb.Length; i++)
      {
        if (sb[i] == '\x03C3')
        {
          if (i == sb.Length - 1)
          {
            if (i != 0 && !char.IsSeparator(sb[i - 1]))
            {
              sb[i] = '\x03C2';
            }
          }
          else if (char.IsSeparator(sb[i + 1]))
          {
            sb[i] = '\x03C2';
          }
        }
      }
      return sb.ToString();
    }

    [Builtin("string-titlecase")]
    public static object ToTitleCaseString(object obj)
    {
      string s = ToLowerCaseString(obj) as string;
      return textinfo.ToTitleCase(s);
    }

    [Builtin("string-foldcase")]
    public static object ToFoldCaseString(object obj)
    {
      string s = ToUpperCaseString(obj) as string;
      return s.ToLowerInvariant();
    }

    [Builtin("string-normalize")]
    public static object StringNormalizeNFD(object obj, object form)
    {
      string s = RequiresNotNull<string>(obj);
      NormalizationForm nf = Helpers.SymbolToEnum<NormalizationForm>(form);
      return s.Normalize(nf);
    }
  }
}

