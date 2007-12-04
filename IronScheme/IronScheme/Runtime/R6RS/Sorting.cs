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
  public class Sorting : Builtins
  {
    [Builtin("list-sort")]
    public static object ListSort(object proc, object lst)
    {
      return false;
    }

    [Builtin("vector-sort")]
    public static object VectorSort(object proc, object vec)
    {
      return false;
    }
    
    [Builtin("vector-sort!")]
    public static object VectorSortD(object proc, object vec)
    {
      return Unspecified;
    }
  }
}
#endif
