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
