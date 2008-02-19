using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting;

namespace IronScheme.Runtime.psyntax
{
  public class Serialization : Builtins
  {
    [Builtin("load-serialized-library")]
    public static object LoadLibrary(object filename, object sk)
    {
      return FALSE;
    }

    [Builtin("serialize-library")]
    public static object SaveLibrary(object filename, object contents)
    {
      return FALSE;
    }

    [Builtin("compile-core-expr")]
    public static object CompileLibrary(object coreexpr)
    {
      return FALSE;
    }

  }
}
