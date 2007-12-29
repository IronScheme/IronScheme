using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting;

namespace IronScheme.Runtime.psyntax
{
  public class LibraryManager : Builtins
  {
    static SymbolId system = SymbolTable.StringToId("clr");

    [Builtin("clr-library-locator")]
    public static object GetCLRLibrary(object libspec)
    {

      Cons c = libspec as Cons;
      if (c != null)
      {
        if ((bool)IsEqual(c.car, system))
        {
          return Read(OpenInputString(@"
;;generated
(library (clr) 
  (export)
  (import (rnrs)))"));
        }
      }
      return false;
    }
  }
}
