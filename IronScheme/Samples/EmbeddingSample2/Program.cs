using System;
using System.Collections.Generic;
using System.Text;
using IronScheme.Hosting;
using Microsoft.Scripting.Hosting;
using Microsoft.Scripting;
using IronScheme;
using IronScheme.Runtime;
using System.Diagnostics;

namespace EmbeddingConsole
{
  class Program
  {
    static void Main(string[] args)
    {
      try
      {
        var r = "foo".Eval();
        Debug.Fail("Cannot get here");
      }
      catch
      {
        // testing exceptions, must be thrown back here
      }
@"
(define (foo x) 
  (let ((x (string-append x ""\n"")))
    (display x)
    x))
".Eval();

      var foo = "foo".Eval<Callable>();
      var foodel = foo.ToDelegate< System.Func<object, object>>();
      var result = foodel("hello world");
      Console.Write(result);

      // this should become quite funky in C# 4.0  :)
      var bar = "(lambda x (for-each display (reverse x))(newline))".Eval<Callable>();
      bar.Call(1, 2, 3, 4, 5);

      Console.ReadLine();
    }
  }
}
