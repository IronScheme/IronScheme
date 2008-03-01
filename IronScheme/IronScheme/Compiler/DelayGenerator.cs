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
  [Generator("delay")]
  public sealed class DelayGenerator : SimpleGenerator
  {
    public override Expression Generate(object args, CodeBlock cb)
    {
      CodeBlock c = Ast.CodeBlock(GetLambdaName(cb));
      c.Parent = cb;

      FillBody(c, new List<Statement>(), args as Cons, true);

      return Ast.Call(null,
        Promise_Make, Ast.CodeContext(), Ast.CodeBlockExpression(c, false, false));
    }
  }
}
