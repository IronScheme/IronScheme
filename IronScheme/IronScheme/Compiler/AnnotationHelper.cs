using System;
using System.Collections.Generic;
using System.Text;
using IronScheme.Runtime.psyntax;
using Microsoft.Scripting;

namespace IronScheme.Compiler
{
  static class AnnotationHelper
  {
    public static object Annotate(object obj, SourceSpan loc)
    {
      return obj;
    }
  }
}
