#region License
/* ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 
 *
 * This source code is subject to terms and conditions of the Microsoft Permissive License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Permissive License.
 *
 * You must not remove this notice, or any other, from this software.
 * ***************************************************************************/
#endregion

using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting.Math;
using System.Reflection;
using Microsoft.Scripting.Utils;
using System.Collections;
using Microsoft.Scripting;
using System.IO;
using Microsoft.Scripting.Hosting;
using Microsoft.Scripting.Types;

namespace IronScheme.Runtime
{
  public static partial class Builtins
  {

    [Builtin("with-input-from-file")]
    public static object WithInputFromFile(object filename, object thunk)
    {
      throw new NotImplementedException();
    }


    [Builtin("with-output-to-file")]
    public static object WithOutputToFile(object filename, object thunk)
    {
      throw new NotImplementedException();
    }

    [Builtin("transcript-off")]
    public static object TranscriptOff()
    {
      throw new NotImplementedException();
    }

    [Builtin("transcript-on")]
    public static object TranscriptOn(object filename)
    {
      throw new NotImplementedException();
    }

    [Builtin("load")]
    public static object Load(CodeContext cc, object filename)
    {
      string path = filename as string;
      SourceUnit su = ScriptDomainManager.CurrentManager.Host.TryGetSourceFileUnit(cc.LanguageContext.Engine, path, Encoding.Default);
      
      ScriptModule sm = ScriptDomainManager.CurrentManager.CompileModule(Path.GetFileNameWithoutExtension(path), su);
      
      object result = sm.GetScripts()[0].Run(cc.Scope, cc.ModuleContext);
      return Unspecified;
    }

    [Builtin("read")]
    public static object Read()
    {
      return Read(CurrentInputPort());
    }

    [Builtin("read")]
    public static object Read(object port)
    {
      StringBuilder input = new StringBuilder();
      TextReader r = port as TextReader;

      string i = null;

      while ((i = r.ReadLine()) != null)
      {
        input.AppendLine(i);
        object result = IronSchemeLanguageContext.ReadExpressionString(input.ToString());
        if (result != null)
        {
          return result;
        }
      }

      return EOF;
    }

    readonly static object EOF = new object();

    [Builtin("char-ready?")]
    public static bool IsCharReady()
    {
      return IsCharReady(CurrentInputPort());
    }
    
    [Builtin("char-ready?")]
    public static bool IsCharReady(object port)
    {
      return PeekChar(port) != EOF;
    }

    [Builtin("eof-object?")]
    public static bool IsEof(object obj)
    {
      return obj == EOF;
    }

    [Builtin("read-char")]
    public static object ReadChar()
    {
      return ReadChar(CurrentInputPort());
    }

    [Builtin("read-char")]
    public static object ReadChar(object port)
    {
      int r = ((TextReader)port).Read();
      if (r == -1)
      {
        return EOF;
      }
      return (char)r;
    }

    [Builtin("peek-char")]
    public static object PeekChar()
    {
      return PeekChar(CurrentInputPort());
    }

    [Builtin("peek-char")]
    public static object PeekChar(object port)
    {
      TextReader r = RequiresNotNull<TextReader>(port);
      int i = r.Peek();
      if (i == -1)
      {
        return EOF;
      }
      return (char)i;
    }

    [Builtin("write-char")]
    public static object WriteChar(object ch)
    {
      return WriteChar(CurrentOutputPort());
    }

    [Builtin("write-char")]
    public static object WriteChar(object ch, object port)
    {
      TextWriter w = RequiresNotNull<TextWriter>(port);
      w.Write((char)ch);
      return Unspecified;
    }


    [Builtin]
    public static object Newline()
    {
      return Newline(CurrentOutputPort());
    }

    [Builtin]
    public static object Newline(object port)
    {
      TextWriter w = RequiresNotNull<TextWriter>(port);
      w.WriteLine();
      return Unspecified;
    }

    internal static string DisplayFormat(object obj)
    {
      if (obj == null)
      {
        return "";
      }
      if (obj is BuiltinFunction)
      {
        return ((BuiltinFunction)obj).Name;
      }
      //if (obj is Closure)
      //{
      //  return ((Closure)obj).Name;
      //}
      //if (obj is Macro)
      //{
      //  return ((Macro)obj).Name;
      //}

      if (obj is FastCallable)
      {
        FieldInfo fi = obj.GetType().GetField("name", BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.FlattenHierarchy);
        if (fi != null)
        {
          return fi.GetValue(obj) as string;
        }
      }
      MethodInfo mi = obj as MethodInfo;
      if (mi != null)
      {
        return mi.ToString();
      }
      if (obj is bool)
      {
        return ((bool)obj) ? "#t" : "#f";
      }
      if (obj is string)
      {
        return string.Format("{0}", obj);
      }
      if (obj is char)
      {
        return string.Format("{0}", obj);
      }

      if (obj is Cons)
      {
        List<string> v = new List<string>();
        Cons s = obj as Cons;

        while (s != null)
        {
          v.Add(DisplayFormat(s.Car));
          if (s.Cdr != null && !(s.Cdr is IEnumerable))
          {
            v.Add(".");
            v.Add(DisplayFormat(s.Cdr));
            break;
          }
          s = s.Cdr as Cons;
        }
        return string.Format("({0})", string.Join(" ", v.ToArray()));
      }

      if (obj is IList)
      {
        List<string> v = new List<string>();
        foreach (object io in (IList)obj)
        {
          v.Add(DisplayFormat(io));
        }

        return string.Format("#({0})", string.Join(" ", v.ToArray()));
      }

      if (obj is IEnumerable)
      {
        List<string> v = new List<string>();
        foreach (object io in (IEnumerable)obj)
        {
          v.Add(DisplayFormat(io));
        }

        return string.Format("({0})", string.Join(" ", v.ToArray()));
      }
      if (obj is SymbolId)
      {
        return SymbolTable.IdToString((SymbolId)obj);
      }

      return obj.ToString();
    }

    [Builtin("display")]
    public static object Display(object obj)
    {
      return Display(obj, CurrentOutputPort());
    }

    [Builtin("display")]
    public static object Display(object obj, object port)
    {
      TextWriter w = RequiresNotNull<TextWriter>(port);
      string s = DisplayFormat(obj);
      w.Write(s);
      return Unspecified;
    }

    internal static string WriteFormat(object obj)
    {
      if (obj == null)
      {
        return "()";
      }
      if (obj is BuiltinFunction)
      {
        return ((BuiltinFunction)obj).Name;
      }
      //if (obj is Closure)
      //{
      //  return ((Closure)obj).Name;
      //}
      //if (obj is Macro)
      //{
      //  return ((Macro)obj).Name;
      //}

      if (obj is FastCallable)
      {
        FieldInfo fi = obj.GetType().GetField("name", BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.FlattenHierarchy);
        if (fi != null)
        {
          return fi.GetValue(obj) as string;
        }
      }
      if (obj is bool)
      {
        return ((bool)obj) ? "#t" : "#f";
      }
      if (obj is string || obj is StringBuilder)
      {
        return string.Format("\"{0}\"", obj);
      }
      if (obj is char)
      {
        return string.Format("#\\{0}", obj);
      }

      if (obj is Cons)
      {
        List<string> v = new List<string>();
        Cons s = obj as Cons;

        if (s.Car == null && s.Cdr == null)
        {
          return "()";
        }

        while (s != null)
        {
          v.Add(WriteFormat(s.Car));
          if (s.Cdr != null && !(s.Cdr is IEnumerable))
          {
            v.Add(".");
            v.Add(WriteFormat(s.Cdr));
            break;
          }
          s = s.Cdr as Cons;
        }
        return string.Format("({0})", string.Join(" ", v.ToArray()));
      }

      if (obj is IList)
      {
        List<string> v = new List<string>();
        foreach (object io in (IList)obj)
        {
          v.Add(WriteFormat(io));
        }

        return string.Format("#({0})", string.Join(" ", v.ToArray()));
      }

      if (obj is IEnumerable)
      {
        List<string> v = new List<string>();
        foreach (object io in (IEnumerable)obj)
        {
          v.Add(WriteFormat(io));
        }

        return string.Format("({0})", string.Join(" ", v.ToArray()));
      }
      if (obj is SymbolId)
      {
        return SymbolTable.IdToString((SymbolId)obj);
      }

      if (obj is BuiltinFunction)
      {
        return ((BuiltinFunction)obj).Name;
      }

      return obj.ToString();
    }

    [Builtin("write")]
    public static object Write(object obj)
    {
      return Write(obj, CurrentOutputPort());
    }

    [Builtin("write")]
    public static object Write(object obj, object port)
    {
      TextWriter w = RequiresNotNull<TextWriter>(port);
      string s = WriteFormat(obj);
      w.Write(s);
      return Unspecified;
    }


    [Builtin("input-port?")]
    public static bool IsInputPort(object obj)
    {
      return obj is TextReader; 
    }

    [Builtin("output-port?")]
    public static bool IsOutputPort(object obj)
    {
      return obj is TextWriter;
    }

    [Builtin("call-with-input-file")]
    public static object CallWithInputFile(CodeContext cc, object filename, object fc1)
    {
      try
      {
        FastCallable f = RequiresNotNull<FastCallable>(fc1);
        string path = RequiresNotNull<string>(filename);
        using (TextReader r = File.OpenText(path))
        {
          f.Call(cc, r);
          return true;
        }
      }
      catch
      {
        return false;
      }
    }

    [Builtin("call-with-output-file")]
    public static object CallWithOutputFile(CodeContext cc, object filename, object fc1)
    {
      try
      {
        FastCallable f = RequiresNotNull<FastCallable>(fc1);
        string path = RequiresNotNull<string>(filename);
        using (TextWriter w = File.CreateText(path))
        {
          f.Call(cc, w);
          return true;
        }
      }
      catch
      {
        return false;
      }
    }

    [Builtin("current-input-port")]
    public static TextReader CurrentInputPort()
    {
      return Console.In;
    }

    [Builtin("current-output-port")]
    public static TextWriter CurrentOutputPort()
    {
      return Console.Out;
    }
    
    [Builtin("open-input-file")]
    public static TextReader OpenInputFile(object filename)
    {
      string fn = RequiresNotNull<string>(filename);
      return File.OpenText(fn);
    }

    [Builtin("open-output-file")]
    public static TextWriter OpenOutputFile(object filename)
    {
      string fn = RequiresNotNull<string>(filename);
      return File.CreateText(fn);
    }

    [Builtin("close-input-port")]
    public static object CloseInputPort(object port)
    {
      RequiresNotNull<TextReader>(port).Close();
      return Unspecified;
    }

    [Builtin("close-output-port")]
    public static object CloseOutputPort(object port)
    {
      RequiresNotNull<TextWriter>(port).Close();
      return Unspecified;
    }



  }
}
