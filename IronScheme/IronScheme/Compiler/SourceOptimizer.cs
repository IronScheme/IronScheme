using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting.Ast;
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
