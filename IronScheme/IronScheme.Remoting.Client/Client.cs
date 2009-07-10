#region License
/* ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 
 *
 * This source code is subject to terms and conditions of the Microsoft Public License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Public License.
 *
 * You must not remove this notice, or any other, from this software.
 * ***************************************************************************/
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
