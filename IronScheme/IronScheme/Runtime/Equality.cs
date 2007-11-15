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
using System.ComponentModel;
using Microsoft.Scripting;
using System.Diagnostics;

namespace IronScheme.Runtime
{
  public partial class Builtins
  {
    [Builtin("equal?")]
    public static object IsEquivalent(object first, object second)
    {
      if ((bool)IsEqualValue(first, second))
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

      return result;
    }

    [Builtin("eq?")]
    public static object IsEqual(object first, object second)
    {
      // one exception, symbols
      if (first is SymbolId && second is SymbolId)
      {
        return Equals(((SymbolId)first).CaseInsensisitveId, ((SymbolId)second).CaseInsensisitveId);
      }
      if (first is ValueType && second is ValueType)
      {
        return Equals(first, second);
      }
      return ReferenceEquals(first, second);
    }

    [Builtin("eqv?")]
    public static object IsEqualValue(object first, object second)
    {
      return ((bool)IsEqual(first, second)) || Equals(first, second);
    }

  }
}
