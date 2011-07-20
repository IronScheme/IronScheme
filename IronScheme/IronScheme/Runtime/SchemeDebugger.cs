using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting.Debugging;
using Microsoft.Scripting;

namespace IronScheme.Runtime
{
  public class SchemeDebugger : IDebuggerCallback
  {
    readonly Callable callback;

    public SchemeDebugger(Callable callback)
    {
      this.callback = callback;
    }

    public void Notify(NotifyReason reason, string filename, Microsoft.Scripting.SourceSpan span)
    {
      callback.Call(SymbolTable.StringToId(reason.ToString()), filename, span.Start.Line, span.Start.Column, span.End.Line, span.End.Column);
    }
  }
}
