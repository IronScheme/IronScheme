#region License
/* Copyright (c) 2007-2014 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.IO;
using System.Linq;
using IronScheme.Web.Hosting;

namespace IronScheme.WebServer
{
  class Program
  {
    static int Main(string[] args)
    {
      var s = Properties.Settings.Default;
      var dir = Path.GetFullPath(args.Length == 0 ? s.WebRoot : args[0]);

      if (File.Exists(Path.Combine(dir, "web.config")))
      {
        //if (File.Exists(Path.Combine(Path.Combine(dir, "bin"), "IronScheme.Web.Runtime.dll")))
        if (File.Exists(new [] {dir, "bin", "IronScheme.Web.Runtime.dll"}.Aggregate(Path.Combine)))
        {
          var ctl = new HttpListenerController(
            new string[] { string.Format("http://{0}:{1}/", s.IP, s.Port) },
            "/", dir + @"\");

          ctl.Start();

          Console.WriteLine("Ctrl-C to stop");
          Console.ReadLine();
          //Environment.Exit(0); // wierd, not really  :)
          return 0;
        }
        else
        {
          Console.Error.WriteLine("IronScheme not properly installed. This is normal on Windows XP, follow the instructions in the docs/webserver-xp.txt");
          return 2;
        }
      }
      else
      {
        Console.Error.WriteLine("Not a web directory: {0}", dir);
        return 1;
      }
    }
  }
}
