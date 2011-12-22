using System;
using System.Collections.Generic;
using System.Text;
using IronScheme.Remoting;
using IronScheme.Remoting.Client;
using System.Diagnostics;

namespace RemotingSample
{
  class Program
  {
    static void StartServer()
    {
      var isp = new Process
      {
        StartInfo = new ProcessStartInfo
        {
          FileName = @"c:\dev\IronScheme\IronScheme.Console\bin\Release\IronScheme.Console.exe",
          Arguments = "-emacs --remoting-server",
          CreateNoWindow = true,
          UseShellExecute = false,
          RedirectStandardError = true,
          RedirectStandardOutput = true,
          RedirectStandardInput = true,
        }
      };

      isp.Start();
    }

    static void Main(string[] args)
    {
      //StartServer();

      var sbs = ServiceManager.GetSymbolBindingService();

      var result = sbs.GetBindings("(ironscheme linq)");
      Console.WriteLine(result);

      var pi = sbs.GetProcedureInfo("cons", "(rnrs)");

      try
      {
        // not a procedure
        var pi2 = sbs.GetProcedureInfo("from", "(ironscheme linq)");
      }
      catch //(EvaluationException ex)
      {
        //Console.WriteLine(ex.Message);
      }

      var ii = ServiceManager.GetInteractionService();

      var sres = ii.EvalToString("(+ 1 1)");
      var ores = ii.Eval("(* 99 2)");
      Console.WriteLine(ores);

      var wres = ii.Eval("(lambda (x)(+ {0} x))", ores);
      var xres = ii.Eval("({0} {1})", wres, ores);

      Console.WriteLine(xres);

      Console.ReadLine();
    }
  }
}
