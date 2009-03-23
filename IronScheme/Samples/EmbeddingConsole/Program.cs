using System;
using System.Collections.Generic;
using System.Text;
using IronScheme.Hosting;
using Microsoft.Scripting.Hosting;
using Microsoft.Scripting;
using IronScheme.Runtime;

namespace EmbeddingConsole
{
  class Program
  {
    static void Main(string[] args)
    {
      var slp = new IronSchemeLanguageProvider();
      var se = slp.GetEngine();

      se.Evaluate(@"
(define (foo x) 
  (let ((x (string-append x ""\n"")))
    (display x)
    x))
");

      var foo = se.Evaluate("foo") as ICallable;
      var result = foo.Call("hello world");
      Console.Write(result);

      // this should become quite funky in C# 4.0  :)
      var bar = se.Evaluate("(lambda x (for-each display (reverse x))(newline))") as ICallable;
      bar.Call(1, 2, 3, 4, 5);
      
      Console.ReadLine();
    }
  }
}
