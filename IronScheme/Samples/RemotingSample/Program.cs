﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using IronScheme.Remoting;
using IronScheme.Remoting.Client;

namespace RemotingSample
{
  class Program
  {
    static void Main(string[] args)
    {
      var sbs = ServiceManager.GetSymbolBindingService();

      var result = sbs.GetBindings("(ironscheme linq2)");
      Console.WriteLine(result);

      var pi = sbs.GetProcedureInfo("cons", "(rnrs)");

      try
      {
        // not a procedure
        var pi2 = sbs.GetProcedureInfo("from", "(ironscheme linq2)");
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
