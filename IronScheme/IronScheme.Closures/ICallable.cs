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
using System.Reflection;

namespace IronScheme.Runtime
{
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

  namespace Typed
  {
    public interface ITypedCallable
    {

    }

    public interface ITypedCallable<R> : ITypedCallable
    {
      R Invoke();
    }

    public interface ITypedCallable<A1, R> : ITypedCallable
    {
      R Invoke(A1 a1);
    }

    public interface ITypedCallable<A1, A2, R> : ITypedCallable
    {
      R Invoke(A1 a1, A2 a2);
    }

    public interface ITypedCallable<A1, A2, A3, R> : ITypedCallable
    {
      R Invoke(A1 a1, A2 a2, A3 a3);
    }

    public interface ITypedCallable<A1, A2, A3, A4, R> : ITypedCallable
    {
      R Invoke(A1 a1, A2 a2, A3 a3, A4 a4);
    }

    public interface ITypedCallable<A1, A2, A3, A4, A5, R> : ITypedCallable
    {
      R Invoke(A1 a1, A2 a2, A3 a3, A4 a4, A5 a5);
    }

    public interface ITypedCallable<A1, A2, A3, A4, A5, A6, R> : ITypedCallable
    {
      R Invoke(A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6);
    }

    public interface ITypedCallable<A1, A2, A3, A4, A5, A6, A7, R> : ITypedCallable
    {
      R Invoke(A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6, A7 a7);
    }

    public interface ITypedCallable<A1, A2, A3, A4, A5, A6, A7, A8, R> : ITypedCallable
    {
      R Invoke(A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6, A7 a7, A8 a8);
    }
  }

}
