#region License
/* Copyright (c) 2007,2008,2009,2010,2011 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.Remoting;
using System.Runtime.Remoting.Channels;
using System.Runtime.Remoting.Channels.Ipc;
using System.Threading;
using IronScheme.Runtime;
using Microsoft.Scripting;

namespace IronScheme.Remoting.Server
{
  public static class Host
  {
    static Mutex m;

    public static bool Start()
    {
      // only on first instance for now
      bool created;
      m = new Mutex(true, "IronScheme", out created);

      if (created)
      {
        BinaryServerFormatterSinkProvider sbs = new BinaryServerFormatterSinkProvider();
        sbs.TypeFilterLevel = System.Runtime.Serialization.Formatters.TypeFilterLevel.Full;

        IpcServerChannel server = new IpcServerChannel("IronScheme", "IronScheme", sbs);

        ChannelServices.RegisterChannel(server, false);

        RemotingConfiguration.RegisterWellKnownServiceType(typeof(SymbolBindingService), "SymbolBindingService", WellKnownObjectMode.Singleton);
        RemotingConfiguration.RegisterWellKnownServiceType(typeof(InteractionService), "InteractionService", WellKnownObjectMode.Singleton);
      }

      return created;
    }
  }


  public abstract class RemoteService : MarshalByRefObject
  {
    public RemoteService()
    {
    }

    protected string GetImportSpec(string spec)
    {
      return string.Format("(apply environment '({0}))", spec);
    }

    protected string GetInteractionEnv()
    {
      return RuntimeExtensions.INTERACTION_ENVIRONMENT;
    }

    public override object InitializeLifetimeService()
    {
      return null;
    }

  }

  class RemoteSchemeObject : ObjectHandle, IRemoteSchemeObject
  {
    public RemoteSchemeObject(object value)
      : base(value)
    {

    }

    public string Type
    {
      get
      {
        object v = Unwrap();
        if (v == null || v is Cons)
        {
          return "pair";
        }
        return v.GetType().ToString();
      }
    }

    public override string ToString()
    {
      return InteractionService.WriteFormat(Unwrap());
    }
  }

  public sealed class InteractionService : RemoteService, IInteractionService
  {

    #region IInteractionService Members

    internal static string WriteFormat(object obj)
    {
      var w = new IronScheme.Runtime.StringWriter();
      "(write {0} {1})".Eval(obj, w);
      return w.GetBuffer();
    }

    public string EvalToString(string expr)
    {
      object result = EvalInternal(expr, GetInteractionEnv());
      return WriteFormat(result);
    }

    public string EvalToString(string expr, string importspec)
    {
      object result = EvalInternal(expr, GetImportSpec(importspec));
      return WriteFormat(result);
    }

    object ToRemote(object obj)
    {
      Type t = obj.GetType();

      if (t.Assembly == typeof(object).Assembly && t.IsSerializable)
      {
        return obj;
      }

      return new RemoteSchemeObject(obj);
    }

    object FromRemote(object obj)
    {
      if (obj is RemoteSchemeObject)
      {
        return ((RemoteSchemeObject)obj).Unwrap();
      }
      return obj;
    }

    public object Eval(string expr, params object[] args)
    {
      object result = EvalInternal(expr, GetInteractionEnv(), Array.ConvertAll(args, x => FromRemote(x)));
      return ToRemote(result);
    }

    public object Eval(string expr, string importspec, params object[] args)
    {
      object result = EvalInternal(expr, GetImportSpec(importspec), Array.ConvertAll(args, x => FromRemote(x)));
      return ToRemote(result);
    }

    object EvalInternal(string expr, string importspec, params object[] args)
    {
      try
      {
        return expr.Eval(importspec, args);
      }
      catch (Exception ex)
      {
        throw new EvaluationException(ex.ToString());
      }
    }

    #endregion
  }

  public sealed class SymbolBindingService : RemoteService, ISymbolBindingService
  {
    public SymbolBinding[] GetBindings(string importspec)
    {
      return GetBindingsInternal(GetImportSpec(importspec));
    }

    public SymbolBinding[] GetBindings()
    {
      return GetBindingsInternal(GetImportSpec("(ironscheme)"));
    }


    SymbolBinding[] GetBindingsInternal(string importspec)
    {
      if (string.IsNullOrEmpty(importspec))
      {
        throw new ArgumentException("importspec cannot be null or empty");
      }
      try
      {
        Callable c =
@"(lambda (maker)
    (map (lambda (b) 
            (maker (car b) (cdr b)))
         (environment-bindings {0})))".Eval<Callable>(importspec.Eval());

        CallTarget2 maker = (n, t) =>
          {
            return new SymbolBinding
            {
              Name = SymbolTable.IdToString((SymbolId)n),
              Type = (BindingType)Enum.Parse(typeof(BindingType), SymbolTable.IdToString((SymbolId)t), true),
            };
          };

        var r = c.Call(Closure.Create(maker)) as IEnumerable;

        List<SymbolBinding> sbs = new List<SymbolBinding>();

        foreach (SymbolBinding sb in r)
        {
          sbs.Add(sb);
        }

        return sbs.ToArray();
      }
      catch (Exception ex)
      {
        throw new EvaluationException(ex.ToString());
      }
    }


    ProcedureInfo GetProcedureInfoInternal(string proc, string importspec)
    {
      if (string.IsNullOrEmpty(proc))
      {
        throw new ArgumentException("proc cannot be null or empty");
      }
      if (string.IsNullOrEmpty(importspec))
      {
        throw new ArgumentException("importspec cannot be null or empty");
      }

      try
      {
        Callable c = 
@"(lambda (maker)
    (let ((p (eval {0} {1})))
      (let ((forms (call-with-values (lambda () (procedure-form p)) vector)))
        (maker (symbol->string {0}) forms))))".Eval<Callable>(SymbolTable.StringToObject(proc), importspec.Eval());

        CallTarget2 maker = (n, forms) =>
          {
            var f = forms as object[];
            List<string> ff = new List<string>();
            foreach (Cons fc in f)
            {
              ff.Add(fc.PrettyPrint.TrimEnd('\n'));
            }

            var pi = new ProcedureInfo
            {
              Name = n as string,
              Forms = ff.ToArray()
            };
            return pi;
          };

        return c.Call(Closure.Create(maker)) as ProcedureInfo;
      }
      catch (Exception ex)
      {
        throw new EvaluationException(ex.ToString());
      }
    }

    public ProcedureInfo GetProcedureInfo(string proc)
    {
      return GetProcedureInfoInternal(proc, GetInteractionEnv());
    }

    public ProcedureInfo GetProcedureInfo(string proc, string importspec)
    {
      return GetProcedureInfoInternal(proc, GetImportSpec(importspec));
    }
  }
}
