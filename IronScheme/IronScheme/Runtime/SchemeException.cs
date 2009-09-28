using System;
using System.Collections.Generic;
using System.Text;
using IronScheme.Runtime.R6RS;

namespace IronScheme.Runtime
{
  public class SchemeException : Exception
  {
    public object Condition { get; private set; }

    public SchemeException(object cond)
    {
      Condition = cond;
    }

    public override string ToString()
    {
      var sw = new StringWriter();
      "(display {0} {1})".Eval(Condition, sw);
      return sw.ToString();
    }
  }
}
