#region License
/* Copyright (c) 2007-2015 Llewellyn Pritchard
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Diagnostics;
using System.Threading;
using System.Windows.Forms;

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
          FileName = "IronScheme.WebServer.exe",
          RedirectStandardError = true
          
        }
      };

      if (ps.Start())
      {
        Thread.Sleep(1000);

        if (!ps.HasExited)
        {
          Application.EnableVisualStyles();
          Application.SetCompatibleTextRenderingDefault(false);
          Application.Run(new MainForm());

          ps.Kill();
        }
        else
        {
          MessageBox.Show(ps.StandardError.ReadToEnd(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
        }
      }
    }
  }
}
