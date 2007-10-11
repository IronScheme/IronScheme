#region License
/* ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 
 *
 * This source code is subject to terms and conditions of the Microsoft Permissive License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Permissive License.
 *
 * You must not remove this notice, or any other, from this software.
 * ***************************************************************************/
#endregion

using System;
using System.Collections.Generic;
using System.Text;
using System.ComponentModel;
using Microsoft.Scripting;
using System.Diagnostics;

namespace IronScheme.Runtime
{
  public static partial class Builtins
  {
    [Builtin("equal?")]
    public static bool IsEquivalent(object first, object second)
    {
      if (IsEqualValue(first, second))
      {
        return true;
      }

      if (first == null || second == null)
      {
        return false;
      }

      string s1 = WriteFormat(first);
      string s2 = WriteFormat(second);

      bool result = s1 == s2;

      if (!result)
      {
        Debug.WriteLine(s1 + " != " + s2);
      }

      return result;
    }

    [Builtin("eq?")]
    public static bool IsEqual(object first, object second)
    {
      // one exception, symbols
      if (first is SymbolId && second is SymbolId)
      {
        return Equals(((SymbolId)first).CaseInsensisitveId, ((SymbolId)second).CaseInsensisitveId);
      }
      return ReferenceEquals(first, second);
    }

    [Builtin("eqv?")]
    public static bool IsEqualValue(object first, object second)
    {
      return Equals(first, second);
    }

  }
}
