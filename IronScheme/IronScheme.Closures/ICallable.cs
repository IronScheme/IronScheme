using System;
using System.Collections.Generic;
using System.Text;

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
}
