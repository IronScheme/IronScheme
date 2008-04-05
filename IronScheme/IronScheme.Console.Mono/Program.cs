using System;
using System.Collections.Generic;
using System.Text;
using IronScheme.Hosting;

namespace IronScheme.Console.Mono
{
  class Program
  {
    static int Main(string[] args)
    {
      List<string> newargs = new List<string>(args);
      newargs.Insert(0, "-notabcompletion");

      return new IronSchemeConsoleHost().Run(newargs.ToArray());
    }
  }
}
