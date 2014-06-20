#region License
/* Copyright (c) 2007-2014 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;

namespace IronScheme.Runtime
{
  [Procedure]
  public abstract class Callable
  {
    public abstract object Call();
    public abstract object Call(object arg1);
    public abstract object Call(object arg1, object arg2);
    public abstract object Call(object arg1, object arg2, object arg3);
    public abstract object Call(object arg1, object arg2, object arg3, object arg4);
    public abstract object Call(object arg1, object arg2, object arg3, object arg4, object arg5);
    public abstract object Call(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6);
    public abstract object Call(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7);
    public abstract object Call(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8);

    public abstract object Call(object[] args);

    public abstract object Arity { get; }
    public abstract object Form { get; }

    public bool AllowConstantFold { get; set; }
  }

  [AttributeUsage(AttributeTargets.Class | AttributeTargets.Method | AttributeTargets.Interface, AllowMultiple = false,  Inherited = true)]
  public class ProcedureAttribute : Attribute
  {
    public bool AllowTailCall { get; set; }

    public ProcedureAttribute() : this(true)
    {
      
    }

    public ProcedureAttribute(bool allowtailcall)
    {
      AllowTailCall = allowtailcall;
    }
  }

  namespace Typed
  {
    public interface ITypedCallable
    {

    }

    [Procedure]
    public interface ITypedCallable<R> : ITypedCallable
    {
      R Invoke();
    }

    [Procedure]
    public interface ITypedCallable<A1, R> : ITypedCallable
    {
      R Invoke(A1 a1);
    }

    [Procedure]
    public interface ITypedCallable<A1, A2, R> : ITypedCallable
    {
      R Invoke(A1 a1, A2 a2);
    }

    [Procedure]
    public interface ITypedCallable<A1, A2, A3, R> : ITypedCallable
    {
      R Invoke(A1 a1, A2 a2, A3 a3);
    }

    [Procedure]
    public interface ITypedCallable<A1, A2, A3, A4, R> : ITypedCallable
    {
      R Invoke(A1 a1, A2 a2, A3 a3, A4 a4);
    }

    [Procedure]
    public interface ITypedCallable<A1, A2, A3, A4, A5, R> : ITypedCallable
    {
      R Invoke(A1 a1, A2 a2, A3 a3, A4 a4, A5 a5);
    }

    [Procedure]
    public interface ITypedCallable<A1, A2, A3, A4, A5, A6, R> : ITypedCallable
    {
      R Invoke(A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6);
    }

    [Procedure]
    public interface ITypedCallable<A1, A2, A3, A4, A5, A6, A7, R> : ITypedCallable
    {
      R Invoke(A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6, A7 a7);
    }

    [Procedure]
    public interface ITypedCallable<A1, A2, A3, A4, A5, A6, A7, A8, R> : ITypedCallable
    {
      R Invoke(A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6, A7 a7, A8 a8);
    }
  }

}
