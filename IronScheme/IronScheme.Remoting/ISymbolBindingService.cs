using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.Remoting.Channels;
using System.Runtime.Remoting.Channels.Ipc;
using System.Runtime.Remoting;
using System.Runtime.Serialization;

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
    static ServiceManager()
    {
      BinaryClientFormatterSinkProvider sbs = new BinaryClientFormatterSinkProvider();
      
      IpcClientChannel client = new IpcClientChannel("IronScheme", sbs);

      ChannelServices.RegisterChannel(client, false);
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
