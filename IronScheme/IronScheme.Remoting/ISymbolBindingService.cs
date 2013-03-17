#region License
/* Copyright (c) 2007-2013 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
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
    object EvalWithEnvironment(string expr, string importspec, params object[] args);
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
