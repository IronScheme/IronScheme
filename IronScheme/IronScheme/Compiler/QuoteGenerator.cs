#region License
/* Copyright (c) 2007,2008,2009,2010 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using IronScheme.Runtime;
using Microsoft.Scripting.Ast;

namespace IronScheme.Compiler
{
  [Generator("quote")]
  public sealed class QuoteGenerator : SimpleGenerator
  {
    public override Expression Generate(object args, CodeBlock cb)
    {
      return GetCons(Builtins.Car(args), cb);
    }
  }
}
