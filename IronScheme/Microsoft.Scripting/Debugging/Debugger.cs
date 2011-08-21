using System;
using System.Collections.Generic;
using System.Reflection;

namespace Microsoft.Scripting.Debugging
{
  public class StackFrame
  {
    public MethodBase Method { get; internal set; }
    public string Filename { get; internal set; }
    public SourceSpan Span { get; internal set; }
    public CodeContext Context { get; internal set; }

    public int StartLine { get { return Span.Start.Line; } }
    public int StartColumn { get { return Span.Start.Column; } }
    public int EndLine { get { return Span.End.Line; } }
    public int EndColumn { get { return Span.End.Column; } }

    public override string ToString()
    {
      return string.Format("{0} {1} ({2}:{3}-{4}:{5})",
        Method,
        Filename,
        Span.Start.Line,
        Span.Start.Column,
        Span.End.Line,
        Span.End.Column);
    }
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

  public static class Debug
  {
    const int TRACE_LENGTH = 128;
    static readonly Stack<StackFrame> stack = new Stack<StackFrame>();
    static readonly Stack<SourceSpan> locationstack = new Stack<SourceSpan>();
    static readonly Queue<SourceSpan> locationtrace = new Queue<SourceSpan>(TRACE_LENGTH);

    public static IDebuggerCallback Debugger { get; set; }

    static void AddLocationTrace(SourceSpan s)
    {
      if (s.IsValid)
      {
        if (locationtrace.Count == TRACE_LENGTH)
        {
          locationtrace.Dequeue();
        }
        locationtrace.Enqueue(s);
      }
    }

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

    public static IEnumerable<StackFrame> CallStack
    {
      get { return stack; }
    }

    public static IEnumerable<SourceSpan> LocationTrace
    {
      get { return locationtrace; }
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

    public static void ProcedureExit(long span)
    {
      var s = LongToSpan(span);
      AddLocationTrace(s);

      if (Debugger != null && s.IsValid)
      {
        Debugger.Notify(NotifyReason.ProcedureExit, CurrentFilename, s);
      }

      stack.Pop();
    }

    public static void ExpressionIn(long span)
    {
      var s = LongToSpan(span);
      locationstack.Push(s);
      AddLocationTrace(s);
      
      if (Debugger != null && s.IsValid)
      {
        Debugger.Notify(NotifyReason.ExpressionIn, CurrentFilename, s);
      }
    }

    // is this needed or interesting?
    public static void ExpressionOut(long span)
    {
      var s = LongToSpan(span);

      if (Debugger != null && s.IsValid)
      {
        Debugger.Notify(NotifyReason.ExpressionOut, CurrentFilename, s);
      }

      locationstack.Pop();
    }

    public static void ExpressionInTail(long span)
    {
      var s = LongToSpan(span);
      AddLocationTrace(s);

      if (Debugger != null && s.IsValid)
      {
        Debugger.Notify(NotifyReason.ExpressionInTail, CurrentFilename, s);
      }

      stack.Pop();
    }

    public static void Reset()
    {
      stack.Clear();
      locationstack.Clear();
      locationtrace.Clear();
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
