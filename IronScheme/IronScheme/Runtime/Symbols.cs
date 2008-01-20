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
      return obj is SymbolId;
    }


    [Builtin("symbol->string")]
    public static object SymbolToString(object obj)
    {
      if (obj is SymbolId)
      {
        return SymbolTable.IdToString((SymbolId)obj);
      }
      else
      {
        return false;
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
        return false;
      }
    }


    [Builtin("symbol=?")]
    public static object IsAllSameSymbol(object obj1, object obj2)
    {
      return Equals(obj1, obj2);
    }

    [Builtin("symbol=?")]
    public static object IsAllSameSymbol(object obj1, object obj2, object obj3, params object[] rest)
    {
      bool h = Equals(obj1, obj2) && Equals(obj2, obj3);
      if (h)
      {
        foreach (object r in rest)
        {
          if (!Equals(r, obj3))
          {
            return false;
          }
        }
      }
      return h;
    }


  }
}
