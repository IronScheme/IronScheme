using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

namespace IronScheme.Runtime
{
  public interface ICallable
  {
    object Call();
    object Call(object arg1);
    object Call(object arg1, object arg2);
    object Call(object arg1, object arg2, object arg3);
    object Call(object arg1, object arg2, object arg3, object arg4);
    object Call(object arg1, object arg2, object arg3, object arg4, object arg5);
    object Call(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6);
    object Call(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7);
    object Call(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8);
    
    object Call(object[] args);

    object Arity { get; }
    object Form { get; }
    bool AllowConstantFold { get; set; }
  }

  public interface ITypedCallable// : ICallable
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
