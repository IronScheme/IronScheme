using System;
using System.Collections.Generic;
using System.Text;

namespace IronScheme.Runtime
{
  public interface ICallable
  {
    object Call(params object[] args);
  }
}
