using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting;
#if CCIDEA
namespace IronScheme.Runtime
{
  public class Continuation : Generator
  {
    public Continuation(CodeContext cc, Type delegateType) : base(cc)
    {
    }

    public override bool MoveNext()
    {
      return false;
    }
  }
}
#endif