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
using Microsoft.Scripting.Generation;
using Microsoft.Scripting;
using System.Reflection;
using Microsoft.Scripting.Utils;
using System.Reflection.Emit;
using System.Collections;
using System.Diagnostics;

namespace IronScheme.Runtime.R6RS
{
  public class Exceptions : Builtins
  {
#if !CPS
    static Stack<Callable> handlerstack = new Stack<Callable>();
    static Callable defaulthandler;

    static Callable CurrentHandler
    {
      get
      {
        if (handlerstack.Count > 0)
        {
          return handlerstack.Peek();
        }
        return null;
      }
    }

    //(with-exception-handler handler thunk)
    [Builtin("with-exception-handler")]
    public static object WithExceptionHandler(object handler, object thunk)
    {
      Callable h = RequiresNotNull<Callable>(handler);
      Callable t = RequiresNotNull<Callable>(thunk);

      InitDefaultHandler();

      handlerstack.Push(h);

      try
      {
        var result = t.Call();
        return result;
      }
      catch (NonContinuation cc)
      {
        if (contstack.Count > 0)
        {
          Continuation c = contstack.Peek();
          c.Value = cc.Value;
          throw c;
        }
        throw (Exception)cc.Value;
      }
      catch (Continuation cc)
      {
        if (contstack.Count > 0)
        {
          Continuation c = contstack.Pop();
          if (cc == c)
          {
            return c.Value;
          }
          else
          {
            throw;
          }
        }
        throw;
      }

      catch (Condition)
      {
        throw;
      }
      catch (Exception ex)
      {
        try
        {
          return h.Call(ex);
        }
        catch (Continuation ccc)
        {
          if (contstack.Count > 0)
          {
            Continuation c = contstack.Pop();
            if (ccc == c)
            {
              return c.Value;
            }
            else
            {
              throw;
            }
          }
          throw;
        }
        finally { }
      }      
      finally
      {
        handlerstack.Pop();
      }
    }


    static void InitDefaultHandler()
    {
      if (defaulthandler == null)
      {
        SymbolId dh = SymbolTable.StringToId("default-exception-handler");
        if (cc.Scope.ContainsName(dh))
        {
          defaulthandler = SymbolValue(dh) as Callable;
          handlerstack.Push(defaulthandler);
        }
      }
    }


    static object GetStackTrace(StackTrace sf)
    {
      List<string> newst = new List<string>();
      var sfs = sf.ToString().Split(Environment.NewLine.ToCharArray(), StringSplitOptions.RemoveEmptyEntries);

      for (int i = 0; i < sfs.Length; i++)
      {
        if (sfs[i].StartsWith(@"   at Microsoft.Scripting.Hosting.ScriptEngine.ExecuteCommand(String code, IScriptModule module)"))
        {
          break;
        }
        newst.Add(sfs[i].
          Replace("   at #.", string.Empty).
          Replace("   at ", string.Empty).
          Replace("CodeContext $context, ", string.Empty).
          Replace("Object ", string.Empty));
      }

      return newst.ToArray();
    }

    [Builtin("raise")]
    public static object Raise(object obj)
    {
      InitDefaultHandler();
      if (obj is CompoundCondition)
      {
        var sf = new StackTrace(1, true);
        Callable st = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&stacktrace-rcd"))) as Callable;
        var values = new ArrayList(((CompoundCondition)obj).conds);

        foreach (var val in values)
        {
          if (val.GetType().Name.Contains("stacktrace"))
          {
            values.Remove(val);
            break;
          }
        }

        values.Add(st.Call(GetStackTrace(sf)));
        ((CompoundCondition)obj).conds = values.ToArray();
      }

      Callable ch = CurrentHandler;
      object hmm = null;
      if (ch != null)
      {
        try
        {
          handlerstack.Pop();
          hmm = ch.Call(obj);
          //Console.WriteLine(hmm);
        }
        finally
        {
          handlerstack.Push(ch);
        }
      }

      if (handlerstack.Count == 0)
      {

        if (!(obj is Exception))
        {
          obj = new NonCondition(obj);
        }

        Exception ex = RequiresNotNull<Exception>(obj);
        throw ex;
      }
      else
      {
        Callable e = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&non-continuable-rcd"))) as Callable;
        Callable w = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&who-rcd"))) as Callable;
        Callable m = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&message-rcd"))) as Callable;

        var result = R6RS.Conditions.Condition(e.Call(), w.Call("raise"), m.Call("handler returned"));
        throw new NonContinuation { Value = result };
      }
    }

    class NonCondition : Condition
    {
      public object value;

      public NonCondition(object value)
      {
        this.value = value;
      }
    }

    // erk??
    [Builtin("raise-continuable")]
    public static object RaiseContinueable(object obj)
    {
      InitDefaultHandler();

      if (obj is CompoundCondition)
      {
        var sf = new StackTrace(1, true);
        Callable st = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&stacktrace-rcd"))) as Callable;
        var values = new ArrayList(((CompoundCondition)obj).conds);

        foreach (var val in values)
        {
          if (val.GetType().Name.Contains("stacktrace"))
          {
            values.Remove(val);
            break;
          }
        }

        values.Add(st.Call(GetStackTrace(sf)));
        ((CompoundCondition)obj).conds = values.ToArray();
      }


      Callable ch = CurrentHandler;
      if (ch != null)
      {
        try
        {
          handlerstack.Pop();
          var result = ch.Call(obj);
          return result;
        }
        finally
        {
          handlerstack.Push(ch);
        }
      }

      if (!(obj is Exception))
      {
        obj = new NonCondition(obj);
      }

      Exception ex = RequiresNotNull<Exception>(obj);
      throw ex;
    }

#endif
  }


}

