#region License
/* Copyright (c) 2007-2016 Llewellyn Pritchard
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;

namespace IronScheme.Runtime
{
  public sealed class SchemeException : Exception
  {
    const string IMPORT_SPEC = "(environment '(ironscheme))";
    readonly string _who, _message, _stacktrace;

    public object Condition { get; private set; }

    public SchemeException(object cond)
    {
      Condition = cond;

      _who = "(and (who-condition? {0}) (condition-who {0}))".EvalWithEnvironment(IMPORT_SPEC, Condition).ToString();
      _message = "(and (message-condition? {0}) (condition-message {0}))".EvalWithEnvironment(IMPORT_SPEC, Condition) as string;
      _stacktrace = "(and (stacktrace-condition? {0}) (condition-stacktrace {0}))".EvalWithEnvironment(IMPORT_SPEC, Condition) as string;
    }

    public string Who
    {
      get
      {
        return _who;
      }
    }

    public override string StackTrace
    {
      get
      {
        return _stacktrace ?? base.StackTrace;
      }
    }

    public override string Message
    {
      get
      {
        return _message ?? base.Message;
      }
    }

    public override string ToString()
    {
      var sw = new StringWriter();
      "(display {0} {1})".EvalWithEnvironment(IMPORT_SPEC, Condition, sw);
      return sw.ToString();
    }
  }
}
