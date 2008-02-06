using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting;

namespace IronScheme.Runtime.psyntax
{
  public class LibraryManager : Builtins
  {
    [Builtin("clr-library-locator")]
    public static object GetCLRLibrary(object libspec)
    {
      return FALSE;
    }
  }
}
