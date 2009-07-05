using System;
using System.Collections.Generic;
using IronScheme.Web.Hosting;
using System.Text;
using System.IO;

namespace IronScheme.WebServer
{
  class Program
  {
    static void Main(string[] args)
    {
      var s = Properties.Settings.Default;
      var dir = Path.GetFullPath(args.Length == 0 ? s.WebRoot : args[0]);
      var ctl = new HttpListenerController(
        new string[] { string.Format("http://{0}:{1}/", s.IP, s.Port) }, 
        "/", dir + @"\");

      ctl.Start();

      Console.WriteLine("Press enter to stop");
      Console.ReadLine();

      Environment.Exit(0);
    }
  }
}
