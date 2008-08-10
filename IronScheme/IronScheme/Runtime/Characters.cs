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
      return GetBool(obj is char);
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
      
      if (i < 0 || i > 0x10ffff || (i > 0xd7ff && i < 0xe000))
      {
        return AssertionViolation("interger->char", "not a valid unicode value", obj);
      }
      char c = char.ConvertFromUtf32(i)[0];
      return c;
    }

  }
}
