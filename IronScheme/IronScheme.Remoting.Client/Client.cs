#region License
/* Copyright (c) 2007,2008,2009,2010,2011 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.Remoting.Channels;
using System.Runtime.Remoting.Channels.Ipc;
using System.Diagnostics;
using System.Runtime.Remoting;
using System.Threading;

namespace IronScheme.Remoting.Client
{
  public static class ServiceManager
  {
    static Process isp;
    static ServiceManager()
    {
      BinaryClientFormatterSinkProvider sbs = new BinaryClientFormatterSinkProvider();

      IpcClientChannel client = new IpcClientChannel("IronScheme", sbs);

      ChannelServices.RegisterChannel(client, false);

      isp = new Process
      {
        StartInfo = new ProcessStartInfo
        {
          FileName = @"c:\dev\IronScheme\IronScheme.Console\bin\Debug\IronScheme.Console.exe",
          Arguments = "-emacs",
          CreateNoWindow = true,
          UseShellExecute = false,
          RedirectStandardError = true,
          RedirectStandardOutput = true,
          RedirectStandardInput = true,
        }
      };

      //isp.Start();

    }

    public static ISymbolBindingService GetSymbolBindingService()
    {
      var sbs = RemotingServices.Connect(typeof(ISymbolBindingService), "ipc://IronScheme/SymbolBindingService") as ISymbolBindingService;
      return sbs;
    }

    public static IInteractionService GetInteractionService()
    {
      var iis = RemotingServices.Connect(typeof(IInteractionService), "ipc://IronScheme/InteractionService") as IInteractionService;
      return iis;
    }

    public static bool InstanceExists
    {
      get
      {
        bool isnew;
        var m = new Mutex(false, "IronScheme", out isnew);
        m.ReleaseMutex();
        m.Close();
        return isnew;
      }
    }
  }
}
