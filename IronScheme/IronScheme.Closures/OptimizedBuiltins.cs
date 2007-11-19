using System;
using System.Collections.Generic;
using System.Text;

namespace IronScheme.Runtime
{
  public static class OptimizedBuiltins
  {
    readonly static object[] EMPTYARRAY = { };
 
    //[Builtin("call-with-values")]
    public static object CallWithValues(object producer, object consumer)
    {
      ICallable pro = (ICallable)producer;
      ICallable con = (ICallable)consumer;

      object r = pro.Call(EMPTYARRAY);

      if (r is object[])
      {
        return con.Call((object[])r);
      }

      return con.Call(new object[] { r });
    }


  }


}
