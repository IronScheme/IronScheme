using System;
using System.Collections.Generic;
using System.Text;
using IronScheme.Hosting;
using Microsoft.Scripting.Hosting;
using Microsoft.Scripting;
using IronScheme;
using IronScheme.Runtime;

namespace EmbeddingConsole
{
  class Program
  {
    static void Main(string[] args)
    {
@"
(define (foo x) 
  (let ((x (string-append x ""\n"")))
    (display x)
    x))
".EvalInScheme();

      var foo = "foo".EvalInScheme<ICallable>();
      var foodel = foo.ToDelegate<Func<object, object>>();
      var result = foodel("hello world");
      Console.Write(result);

      // this should become quite funky in C# 4.0  :)
      var bar = "(lambda x (for-each display (reverse x))(newline))".EvalInScheme<ICallable>();
      bar.Call(1, 2, 3, 4, 5);

      Console.ReadLine();
    }
  }
}
