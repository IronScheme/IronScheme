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

    static object ReasonToSymbol(NotifyReason reason)
    {
      switch (reason)
      {
        case NotifyReason.ProcedureEnter: return SymbolTable.StringToObject("proc-enter");
        case NotifyReason.ProcedureExit: return SymbolTable.StringToObject("proc-exit");
        case NotifyReason.ExpressionIn: return SymbolTable.StringToObject("expr-in");
        case NotifyReason.ExpressionOut: return SymbolTable.StringToObject("expr-out");
        case NotifyReason.ExpressionInTail: return SymbolTable.StringToObject("expr-in-tail");
        default:
          throw new ArgumentException("Invalid reason");
      }
    }

    public void Notify(NotifyReason reason, string filename, Microsoft.Scripting.SourceSpan span)
    {
      callback.Call(ReasonToSymbol(reason), filename ?? Builtins.FALSE, span.Start.Line, span.Start.Column, span.End.Line, span.End.Column);
    }
  }
}
