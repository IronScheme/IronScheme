#region License
/* Copyright (c) 2007-2014 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.Remoting;
using System.Runtime.Remoting.Channels;
using System.Runtime.Remoting.Channels.Ipc;
using System.Text;
using System.Threading;

namespace IronScheme.Remoting.Client
{
  public static class ServiceManager
  {
    private const string MutexName = "IronScheme";

    static ServiceManager()
    {
      BinaryClientFormatterSinkProvider sbs = new BinaryClientFormatterSinkProvider();

      IpcClientChannel client = new IpcClientChannel("IronScheme", sbs);

      ChannelServices.RegisterChannel(client, false);
   }

    public static ISymbolBindingService GetSymbolBindingService()
    {
      var sbs = (ISymbolBindingService)RemotingServices.Connect(typeof(ISymbolBindingService), "ipc://IronScheme/SymbolBindingService");
      return sbs;
    }

    public static IInteractionService GetInteractionService()
    {
      var iis = (IInteractionService)RemotingServices.Connect(typeof(IInteractionService), "ipc://IronScheme/InteractionService");
      return iis;
    }

    public static bool InstanceExists
    {
      get
      {
        bool isNew;
        using (var m = new Mutex(false, MutexName, out isNew))
        {
          m.ReleaseMutex();
        }
        return isNew;
      }
    }
  }
}
