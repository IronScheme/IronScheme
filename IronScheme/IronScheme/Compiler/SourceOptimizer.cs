#region License
/* Copyright (c) 2007,2008,2009,2010 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using IronScheme.Runtime;

namespace IronScheme.Compiler
{
  static class SourceOptimizer
  {
    public static Cons Optimize(Cons expr)
    {
      return expr;
    }
  }
}
