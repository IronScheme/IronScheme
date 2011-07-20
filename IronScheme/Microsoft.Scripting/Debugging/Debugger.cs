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
        var top = stack.Peek();
        if (top == null)
        {
          return null;
        }
        return top.Filename; 
      }
    }

    static SourceSpan LongToSpan(ulong span)
    {
      var st = (uint)(span & 0xffffffff);
      var en = (uint)(span >> 32);
      var start = new SourceLocation(0, (int)(st >> 10), (int)(st & 0x3ff));
      var end = new SourceLocation(0, (int)(en >> 10), (int)(en & 0x3ff));
      return new SourceSpan(start, end);
    }
    
    public static void ProcedureEnter(RuntimeMethodHandle meth, string filename, ulong span)
    {
      if (Debugger != null)
      {
        var frame = new StackFrame
        {
          Method = MethodBase.GetMethodFromHandle(meth),
          Filename = filename,
          Span = LongToSpan(span)
        };
        stack.Push(frame);

        Debugger.Notify(NotifyReason.ProcedureEnter, filename, frame.Span);
      }
    }

    public static void ProcedureExit()
    {
      if (Debugger != null)
      {
        Debugger.Notify(NotifyReason.ProcedureExit, CurrentFilename, SourceSpan.None);
        stack.Pop();
      }
    }

    public static void ExpressionIn(ulong span)
    {
      if (Debugger != null)
      {
        var s = LongToSpan(span);
        Debugger.Notify(NotifyReason.ExpressionIn, CurrentFilename, s);
      }
    }

    public static void ExpressionOut(ulong span)
    {
      if (Debugger != null)
      {
        var s = LongToSpan(span);
        Debugger.Notify(NotifyReason.ExpressionOut, CurrentFilename, s);
      }
    }

    public static void ExpressionInTail(ulong span)
    {
      if (Debugger != null)
      {
        var s = LongToSpan(span);
        Debugger.Notify(NotifyReason.ExpressionInTail, CurrentFilename, s);
        stack.Pop();
      }
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
