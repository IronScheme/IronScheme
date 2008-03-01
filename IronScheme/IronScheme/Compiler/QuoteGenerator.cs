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
  [Generator("quote")]
  public sealed class QuoteGenerator : SimpleGenerator
  {
    public override Expression Generate(object args, CodeBlock cb)
    {
      int t = NestingLevel;
      NestingLevel = int.MaxValue / 2;
      try
      {
        return GetCons(Builtins.Car(args), cb);
      }
      finally
      {
        NestingLevel = t;
      }
    }
  }
}
