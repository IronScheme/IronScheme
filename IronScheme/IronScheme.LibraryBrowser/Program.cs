using System;
using System.Diagnostics;
using System.Windows.Forms;
using System.IO;
using System.Threading;

namespace IronScheme.LibraryBrowser
{
  static class Program
  {
    /// <summary>
    /// The main entry point for the application.
    /// </summary>
    [STAThread]
    static void Main()
    {
      var ps = new Process
      {
        StartInfo = new ProcessStartInfo
        {
          UseShellExecute = false,
          CreateNoWindow = true,
          FileName = "IronScheme.WebServer.exe"
        }
      };

      if (ps.Start())
      {
        Thread.Sleep(1000);
        Application.EnableVisualStyles();
        Application.SetCompatibleTextRenderingDefault(false);
        Application.Run(new MainForm());

        ps.Kill();
      }
    }
  }
}
