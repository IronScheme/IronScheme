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
using System.Collections;
using Microsoft.Scripting;
using System.IO;

namespace IronScheme.Runtime
{
  public partial class Builtins
  {
    /*

; 6.3.3
- symbol?
- string->symbol
- symbol->string

     */


    [Builtin("symbol?")]
    public static object IsSymbol(object obj)
    {
      return GetBool(obj is SymbolId);
    }


    [Builtin("symbol->string")]
    public static string SymbolToString(object obj)
    {
      if (obj is SymbolId)
      {
        return SymbolTable.IdToString((SymbolId)obj);
      }
      else
      {
        return AssertionViolation(GetCaller(), "not a symbol", obj) as string;
      }
    }


    [Builtin("string->symbol")]
    public static object StringToSymbol(object obj)
    {
      if (obj is StringBuilder)
      {
        obj = obj.ToString();
      }
      if (obj is string)
      {
        return SymbolTable.StringToId((string)obj);
      }
      else
      {
        return AssertionViolation(GetCaller(), "not a string or stringbuilder", obj);
      }
    }

  }
}
