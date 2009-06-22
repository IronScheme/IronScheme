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
using System.Runtime.Remoting;
using System.Runtime.Serialization;
using System.Threading;
using System.Diagnostics;

namespace IronScheme.Remoting
{
  public interface ISymbolBindingService  
  {
    SymbolBinding[] GetBindings();
    SymbolBinding[] GetBindings(string importspec);
    ProcedureInfo GetProcedureInfo(string proc);
    ProcedureInfo GetProcedureInfo(string proc, string importspec);
  }

  public interface IInteractionService
  {
    string EvalToString(string expr);
    string EvalToString(string expr, string importspec);
    object Eval(string expr, params object[] args);
    object Eval(string expr, string importspec, params object[] args);
  }

  public interface IRemoteSchemeObject : IObjectHandle
  {
    string Type { get; }
    string ToString();
  }

  [Serializable]
  public sealed class EvaluationException : Exception
  {
    internal EvaluationException(SerializationInfo info, StreamingContext context)
      : base(info, context)
    {
    }

    public EvaluationException(string msg) : base(msg)
    {

    }
  }


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

      isp.Start();

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

  [Serializable]
  public class ProcedureInfo
  {
    public string Name { get; set; }
    public string[] Forms { get; set; }
  }

  public enum BindingType
  {
    Procedure,
    Syntax,
    Record
  }

  [Serializable]
  public class SymbolBinding
  {
    public string       Name { get; set; }
    public BindingType  Type { get; set; } 
  }
}
