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
using System.Reflection;
using System.Diagnostics;
using Microsoft.Scripting.Ast;
using Microsoft.Scripting;
using IronScheme.Runtime;
using IronScheme.Compiler;

using Generator = IronScheme.Compiler.Generator;

[assembly: Extension(GeneratorType = typeof(IronScheme.Clr.ClrGenerator))]

namespace IronScheme.Clr
{
  static class ClrGenerator
  {
    [Generator]
    public static Expression Call(object args, CodeBlock cb)
    {
      return null;
    }

  }
}
