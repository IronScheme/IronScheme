using System;
using System.Collections.Generic;
using System.Reflection;

namespace Microsoft.Scripting.Debugging
{
  class StackFrame
  {
    public MethodBase Method { get; internal set; }
    public string Filename { get; internal set; }
    public SourceSpan Span { get; internal set; }
    public CodeContext Context { get; internal set; }
  }

  public enum NotifyReason
  {
    ProcedureEnter,
    ProcedureExit,
    ExpressionIn,
    ExpressionOut,
    ExpressionInTail
  }

  public interface IDebuggerCallback
  {
    void Notify(NotifyReason reason, string filename, SourceSpan span);
  }

  [CLSCompliant(false)]
  public static class Debug
  {
    static readonly Stack<StackFrame> stack = new Stack<StackFrame>();

    public static IDebuggerCallback Debugger { get; set; }

    static string CurrentFilename
    {
      get 
      {
        if (stack.Count == 0)
        {
          return null;
        }
        var top = stack.Peek();
        return top.Filename; 
      }
    }

    static SourceSpan LongToSpan(long span)
    {
      var uspan = (ulong)span;
      var st = (uint)(uspan & 0xffffffff);
      var en = (uint)(uspan >> 32);
      var sc = (int)(st & 0x3ff);
      var ec = (int)(en & 0x3ff);
      if (sc == 0 && ec == 0)
      {
        return SourceSpan.Invalid;
      }
      var start = new SourceLocation(0, (int)(st >> 10), sc);
      var end = new SourceLocation(0, (int)(en >> 10), ec);
      return new SourceSpan(start, end);
    }

    public static void ProcedureEnter(RuntimeMethodHandle meth, string filename, long span, CodeContext context)
    {
      var frame = new StackFrame
      {
        Method = MethodBase.GetMethodFromHandle(meth),
        Filename = filename,
        Span = LongToSpan(span),
        Context = context
      };
      stack.Push(frame);

      if (Debugger != null && frame.Span.IsValid)
      {
        Debugger.Notify(NotifyReason.ProcedureEnter, filename, frame.Span);
      }
    }

    public static void ProcedureExit()
    {
      if (Debugger != null)
      {
        Debugger.Notify(NotifyReason.ProcedureExit, CurrentFilename, SourceSpan.Invalid);
      }

      stack.Pop();
    }

    public static void ExpressionIn(long span)
    {
      if (Debugger != null)
      {
        var s = LongToSpan(span);
        Debugger.Notify(NotifyReason.ExpressionIn, CurrentFilename, s);
      }
    }

    public static void ExpressionOut(long span)
    {
      if (Debugger != null)
      {
        var s = LongToSpan(span);
        Debugger.Notify(NotifyReason.ExpressionOut, CurrentFilename, s);
      }
    }

    public static void ExpressionInTail(long span)
    {
      if (Debugger != null)
      {
        var s = LongToSpan(span);
        Debugger.Notify(NotifyReason.ExpressionInTail, CurrentFilename, s);
      }

      stack.Pop();
    }

    
  }

  static class DebugMethods
  {
    public static readonly MethodInfo ProcedureEnter = typeof(Debug).GetMethod("ProcedureEnter");
    public static readonly MethodInfo ProcedureExit = typeof(Debug).GetMethod("ProcedureExit");
    public static readonly MethodInfo ExpressionIn = typeof(Debug).GetMethod("ExpressionIn");
    public static readonly MethodInfo ExpressionOut = typeof(Debug).GetMethod("ExpressionOut");
    public static readonly MethodInfo ExpressionInTail = typeof(Debug).GetMethod("ExpressionInTail");
  }
}
