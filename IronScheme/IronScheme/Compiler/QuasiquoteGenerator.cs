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
using System.Diagnostics;

namespace IronScheme.Compiler
{
  [Generator("quasiquote")]
  public class QuasiquoteGenerator : SimpleGenerator
  {
    public override Expression Generate(object args, CodeBlock cb)
    {
      NestingLevel++;
      try
      {
        if (NestingLevel == 1)
        {
          return GetCons(Builtins.First(args), cb);
        }
        else
        {
          // does this ever get hit?
          Debugger.Break();
          return Ast.SimpleCallHelper(Builtins_Cons2, Ast.Constant(quasiquote), GetCons(args, cb));
        }
      }
      finally
      {
        NestingLevel--;
      }
    }
  }
}
