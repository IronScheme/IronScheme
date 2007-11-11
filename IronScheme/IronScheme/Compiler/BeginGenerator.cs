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
using Microsoft.Scripting.Ast;
using IronScheme.Runtime;

namespace IronScheme.Compiler
{
  [Generator("begin")]
  public class BeginGenerator : SimpleGenerator
  {
    public override Expression Generate(object args, CodeBlock cb)
    {
      if ((bool)Builtins.IsNull(args))
      {
        return Ast.ReadField(null, Unspecified);
      }
      else
      {
        return Ast.Comma(GetAstList(args as Cons, cb));
      }
    }
  }
}
