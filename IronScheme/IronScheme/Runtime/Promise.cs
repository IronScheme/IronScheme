using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting;

namespace IronScheme.Runtime
{
  public sealed class Promise
  {
    static readonly object uninitialized = new object();
    Delegate target;
    CodeContext cc;
    object result = uninitialized;
    Promise(CodeContext cc, Delegate target)
    {
      this.cc = cc;
      this.target = target;
    }

    public static Promise Make(CodeContext cc, Delegate target)
    {
      return new Promise(cc, target);
    }

    public object Force()
    {
      if (result == uninitialized)
      {
        if (target is CallTarget0)
        {
          result = ((CallTarget0)target)();
        }
        else
        {
          result = ((CallTargetWithContext0)target)(cc);
        }
      }
      return result;

    }
  }
}
