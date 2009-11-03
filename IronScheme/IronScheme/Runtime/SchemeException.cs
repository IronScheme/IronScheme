using System;
using System.Collections.Generic;
using System.Text;
using IronScheme.Runtime.R6RS;

namespace IronScheme.Runtime
{
  public sealed class SchemeException : Exception
  {
    public object Condition { get; private set; }

    public SchemeException(object cond)
    {
      Condition = cond;
    }

    public string Who
    {
      get
      {
        return "(and (who-condition? {0}) (condition-who {0}))"
          .Eval(Condition).ToString();
      }
    }

    public override string StackTrace
    {
      get
      {
        return "(and (stacktrace-condition? {0}) (condition-stacktrace {0}))"
          .Eval(Condition) as string;
      }
    }

    public override string Message
    {
      get
      {
        return "(and (message-condition? {0}) (condition-message {0}))"
          .Eval(Condition) as string;
      }
    }

    public override string ToString()
    {
      var sw = new StringWriter();
      "(display {0} {1})".Eval(Condition, sw);
      return sw.ToString();
    }
  }
}
