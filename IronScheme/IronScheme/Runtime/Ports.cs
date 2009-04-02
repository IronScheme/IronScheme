#region License
/* ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 
 *
 * This source code is subject to terms and conditions of the Microsoft Public License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Public License.
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
using Microsoft.Scripting.Actions;
using System.Diagnostics;
using IronScheme.Runtime.R6RS;
using System.Globalization;
using System.Net.Sockets;

namespace IronScheme.Runtime
{
  public sealed class StringWriter : System.IO.StringWriter
  {
    public string GetBuffer()
    {
      StringBuilder sb = GetStringBuilder();
      string r = sb.ToString();
      sb.Length = 0;
      return r;
    }

    public override string ToString()
    {
      return string.Format("#<string-output-port {0}>", Builtins.WriteFormat(base.ToString()));
    }
  }

  public partial class Builtins
  {
    static Assembly AssemblyLoad(string path)
    {
      string fn = Path.GetFullPath(path);
      if (File.Exists(fn))
      {
        return Assembly.LoadFrom(fn);

      }
      FileNotFoundViolation(FALSE, "file not found", path);
      return null;
    }

    [Builtin("load")] // this is patched in r6rs mode, but its needed to bootstrap
    [Builtin("load-r5rs")] // is this used anywhere?
    public static object Load(object filename)
    {
      CodeContext cc = IronScheme.Compiler.BaseHelper.cc; // sneaky....

      string path = GetPath(filename as string);

      switch (Path.GetExtension(path))
      {
        case ".exe":
        case ".dll":

          Assembly ext = AssemblyLoad(path);
          if (Attribute.IsDefined(ext, typeof(ExtensionAttribute)))
          {
            IronScheme.Compiler.Generator.AddGenerators(cc, ext);

            foreach (ExtensionAttribute ea in ext.GetCustomAttributes(typeof(ExtensionAttribute), false))
            {
              if (ea.BuiltinsType != null)
              {
                IronScheme.Compiler.Generator.AddBuiltins(cc, ea.BuiltinsType);
              }
              if (ea.ScriptResource != null)
              {
                //TODO: ExtensionAttribute.ScriptResource
              }
            }
          }
          else
          {
            // just reference.?
            MethodInfo entry = null;
            foreach (Type t in ext.GetExportedTypes())
            {
              if (t.BaseType == typeof(CustomSymbolDictionary))
              {
                List<Type> ii = new List<Type>(t.GetInterfaces());
                if (ii.Contains(typeof(IModuleDictionaryInitialization)))
                {
                  entry = t.GetMethod("Initialize");
                  if (entry != null)
                  {
                    break;
                  }
                }
              }
            }

            if (entry == null)
            {
              // what now?
            }
            else
            {
              IModuleDictionaryInitialization init = Activator.CreateInstance(entry.DeclaringType) as
                IModuleDictionaryInitialization;

              CodeContext ccc = new CodeContext(cc, init as IAttributesCollection);
              init.InitializeModuleDictionary(ccc);

              CallTargetWithContext0 t = Delegate.CreateDelegate(typeof(CallTargetWithContext0), entry) as CallTargetWithContext0;
              return t(ccc);
            }
          }
          break;
        default:

          // check for already compiled version
          string cfn = Path.ChangeExtension(path, ".dll");
          if (File.Exists(cfn))
          {
            DateTime ct = File.GetLastWriteTime(cfn);
            if (!File.Exists(path) || ct >= File.GetLastWriteTime(path))
            {
              if (File.GetLastWriteTime(Path.Combine(ApplicationDirectory, "IronScheme.dll")) <= ct || cfn.EndsWith("ironscheme.boot.dll"))
              {
                path = cfn;
                goto case ".dll";
              }
            }
          }

          if (!File.Exists(path))
          {
            return FileNotFoundViolation("load", "file not found", path);
          }

          SourceUnit su = ScriptDomainManager.CurrentManager.Host.TryGetSourceFileUnit(cc.LanguageContext.Engine, path, Encoding.Default);
#if DEBUG
          Stopwatch sw = Stopwatch.StartNew();
#endif
          ScriptModule sm = ScriptDomainManager.CurrentManager.CompileModule(Path.GetFileNameWithoutExtension(path), su);

          Compiler.SimpleGenerator.ClearGlobals();
#if DEBUG
          Trace.WriteLine(sw.ElapsedMilliseconds, "Compile module: " + sm.FileName);
          sw = Stopwatch.StartNew();
#endif
          object result = sm.GetScripts()[0].Run(sm);
#if DEBUG
          Trace.WriteLine(sw.ElapsedMilliseconds, "Run script: " + sm.GetScripts()[0].SourceUnit);
#endif
          return result;

      }

      return Unspecified;
    }

    [Builtin("read")]
    public static object Read()
    {
      return Read(CurrentInputPort());
    }

    //[ThreadStatic]
    static Dictionary<object, Cons> readcache = new Dictionary<object, Cons>();

    [Builtin("read")]
    [Builtin("get-datum")]
    public static object Read(object port)
    {
      return ReadNext(port);
    }

    protected interface ITranscodedPort
    {
      Stream BinaryPort { get; }
    }

    static object ReadNext(object port)
    {
      Cons c;
      if (readcache.TryGetValue(port, out c))
      {
        if (c.cdr == null)
        {
          readcache.Remove(port);
        }
        else
        {
          readcache[port] = c.cdr as Cons;
        }
        return c.car;
      }
      else
      {
        TextReader r = RequiresNotNull<TextReader>(port);

        try
        {
          object result = null;

          if (r is StreamReader)
          {
            StreamReader rr = (StreamReader)r;
            Stream s = rr.BaseStream;
            if (s == null)
            {
              return IOPortViolation("read", "port has already been closed", r);
            }
            // check if stream has been read by reader
            if (s.Position == 0)
            {

              result = IronSchemeLanguageContext.ReadExpressions(s, GetContext(s, Context.ModuleContext.CompilerContext));
            }
            else
            {
              string input = r.ReadToEnd();
              if (input.Length > 0)
              {
                result = IronSchemeLanguageContext.ReadExpressions(input, GetContext(s, Context.ModuleContext.CompilerContext));
              }
            }
          }
          //else if (r is ITranscodedPort)
          //{
          //  result = IronSchemeLanguageContext.ReadExpressions(((ITranscodedPort)r).BinaryPort, Context.ModuleContext.CompilerContext);
          //}
          else
          {
            string input = r.ReadToEnd();
            if (input.Length > 0)
            {
              result = IronSchemeLanguageContext.ReadExpressions(input, Context.ModuleContext.CompilerContext);
            }
          }

          if (result is Cons)
          {
            c = (Cons)result;
            if (c.cdr is Cons)
            {
              readcache[port] = c.cdr as Cons;
            }
            return c.car;
          }
          else if (result != null)
          {
            return result;
          }

          return EOF;
        }
        catch (IOException ex)
        {
          return IOPortViolation("read", ex.Message, port);
        }
        catch (Condition)
        {
          throw;
        }
      }
    }

    static CompilerContext GetContext(Stream s, CompilerContext old)
    {
      if (s is FileStream)
      {
        return new CompilerContext(SourceUnit.CreateFileUnit(ScriptEngine, ((FileStream)s).Name), old.Options, old.Errors, old.ParserSink);
      }
      return old;
    }


    sealed class Eof 
    {
      public override string ToString()
      {
        return "#<eof>";
      }
    }

    protected readonly static object EOF = new Eof();

    [Builtin("eof-object")]
    public static object EofObject()
    {
      return EOF;
    }

    [Builtin("eof-object?")]
    public static object IsEof(object obj)
    {
      return GetBool(obj == EOF);
    }

    static readonly object quote = SymbolTable.StringToObject("quote");
    static readonly object unquote_splicing = SymbolTable.StringToObject("unquote-splicing");
    static readonly object quasiquote = SymbolTable.StringToObject("quasiquote");
    static readonly object unquote = SymbolTable.StringToObject("unquote");

    static readonly object syntax = SymbolTable.StringToObject("syntax");
    static readonly object unsyntax_splicing = SymbolTable.StringToObject("unsyntax-splicing");
    static readonly object quasisyntax = SymbolTable.StringToObject("quasisyntax");
    static readonly object unsyntax = SymbolTable.StringToObject("unsyntax");

    static string ReaderFormat(Cons s, Function<object, string> Format)
    {
      object scar = s.car;
      if (IsSymbol(scar) && s.cdr is Cons && (int)Length(s.cdr) == 1)
      {
        if (IsTrue(IsEqual(quote, scar)))
        {
          return "'" + Format(Second(s));
        }
        if (IsTrue(IsEqual(quasiquote, scar)))
        {
          return "`" + Format(Second(s));
        }
        if (IsTrue(IsEqual(unquote, scar)))
        {
          return "," + Format(Second(s));
        }
        if (IsTrue(IsEqual(unquote_splicing, scar)))
        {
          return ",@" + Format(Second(s));
        }
        if (IsTrue(IsEqual(syntax, scar)))
        {
          return "#'" + Format(Second(s));
        }
        if (IsTrue(IsEqual(quasisyntax, scar)))
        {
          return "#`" + Format(Second(s));
        }
        if (IsTrue(IsEqual(unsyntax, scar)))
        {
          return "#," + Format(Second(s));
        }
        if (IsTrue(IsEqual(unsyntax_splicing, scar)))
        {
          return "#,@" + Format(Second(s));
        }
      }
      return null;
    }
    
    public static string DisplayFormat(object obj)
    {
      if (obj == null)
      {
        return "()";
      }
      if (obj is string)
      {
        return (string)obj;
      }
      if (obj is StringBuilder)
      {
        return obj.ToString();
      }

      if (obj is bool)
      {
        return IsTrue(obj) ? "#t" : "#f";
      }
      if (obj is char)
      {
        return string.Format("{0}", obj);
      }

      if (obj is SymbolId)
      {
        return SymbolTable.IdToString((SymbolId)obj);
      }

      if (IsTrue(IsNumber(obj)))
      {
        return NumberToString(obj) as string;
      }

      if (obj == EOF)
      {
        return "#<eof>";
      }
      if (obj == Builtins.Unspecified)
      {
        return "#<unspecified>";
      }
      if (obj is ICallable)
      {
        return "#<procedure " + obj + ">";
      }

      if (obj is Type)
      {
        Type t = (Type)obj;
        switch (Type.GetTypeCode(t))
        {
          case TypeCode.Boolean:
            return "Boolean";
          case TypeCode.Char:
            return "Character";
          case TypeCode.DateTime:
          case TypeCode.DBNull:
          case TypeCode.Empty:
          case TypeCode.Object:
            {
              if (t == typeof(SymbolId))
              {
                return "Symbol";
              }
              if (typeof(TextReader).IsAssignableFrom(t) || typeof(TextWriter).IsAssignableFrom(t))
              {
                return "Port";
              }
              if (t == typeof(Cons))
              {
                return "List";
              }
              if (t == typeof(Complex64) || t == typeof(Fraction))
              {
                return "Number";
              }
              if (typeof(ICallable).IsAssignableFrom(t))
              {
                return "Procedure";
              }
              if (t == typeof(object[]))
              {
                return "Vector";
              }
              return t.Name;
            }
          case TypeCode.String:
            return "String";
          default:
            return "Number";
        }

      }


      if (obj is Cons)
      {
        List<string> v = new List<string>();
        Cons s = obj as Cons;

        if (s != null)
        {
          string val = ReaderFormat(s, DisplayFormat);
          if (val != null)
          {
            return val;
          }
        }

        while (s != null)
        {
          v.Add(DisplayFormat(s.car));
          if (s.cdr != null && !(s.cdr is Cons))
          {
            v.Add(".");
            v.Add(DisplayFormat(s.cdr));
            break;
          }
          s = s.cdr as Cons;
        }
        return string.Format("({0})", string.Join(" ", v.ToArray()));
      }

      if (obj is byte[])
      {
        List<string> v = new List<string>();
        foreach (object io in (IList)obj)
        {
          v.Add(DisplayFormat(io));
        }

        return string.Format("#vu8({0})", string.Join(" ", v.ToArray()));
      }

      if (obj is object[])
      {
        List<string> v = new List<string>();
        foreach (object io in (IList)obj)
        {
          v.Add(DisplayFormat(io));
        }

        return string.Format("#({0})", string.Join(" ", v.ToArray()));
      }

      if (obj is Hashtable)
      {
        List<string> v = new List<string>();
        int llen = 0, rlen = 0;
        foreach (DictionaryEntry de in (Hashtable)obj)
        {
          llen = Math.Max(llen, DisplayFormat(de.Key).Length);
          rlen = Math.Max(rlen, DisplayFormat(de.Value).Length);
        }
        string format = string.Format("[{{0,-{0}}} => {{1,-{1}}}]", llen, rlen);

        foreach (DictionaryEntry de in (Hashtable)obj)
        {
          v.Add( string.Format(format,  DisplayFormat(de.Key), DisplayFormat(de.Value)));
        }

        if (v.Count == 0)
        {
          return "#[hashtable]";
        }

        return string.Join(Environment.NewLine, v.ToArray());
      }

      if (obj is NetworkStream)
      {
        return string.Format("#<tcp-binary-input/output-port>");
      }

      if (obj is Exception)
      {
        return cc.LanguageContext.Engine.FormatException(obj as Exception);
      }

      if (obj is R6RS.CompoundCondition)
      {
        R6RS.CompoundCondition cc = (R6RS.CompoundCondition)obj;

        List<string> ll = new List<string>();

        foreach (object o in cc.conds)
        {
          ll.Add(DisplayFormat(o));
        }

        return string.Format("<{0}>", string.Join(" ", ll.ToArray()));
      }

      if (obj is R6RS.Condition)
      {
        return obj.ToString();
      }

      if (obj is Exception)
      {
        return obj.GetType().Name.Replace("$", "&");
      }

      ICallable printer;
      if (R6RS.Records.printers.TryGetValue(obj.GetType().FullName, out printer))
      {
        StringWriter p = new StringWriter();
        printer.Call(obj, p, TRUE);
        return p.GetBuffer();
      }

      //finally check if this is some constructed type
      if (R6RS.Records.IsRecordAny(obj))
      {
        return R6RS.Records.PrintRecord(obj);
      }

      if (obj is MultipleValues)
      {
        object[] values = ((MultipleValues)obj).ToArray();
        return string.Join("\n", Array.ConvertAll<object, string>(values, DisplayFormat));
      }

      if (obj is IntPtr)
      {
        IntPtr p = (IntPtr)obj;
        return string.Format("#<pointer #x{0:X8}>", p.ToInt32());
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
      if (obj is BigInteger)
      {
        ((BigInteger)obj).Write(w);
      }
      else
      {
        string s = DisplayFormat(obj);
        w.Write(s);
      }
      return Unspecified;
    }

    public static string WriteFormat(object obj)
    {
      if (obj == null)
      {
        return "()";
      }

      if (obj == EOF)
      {
        return "#<eof>";
      }
      if (obj == Builtins.Unspecified)
      {
        return "#<unspecified>";
      }
      if (obj is ICallable)
      {
        return "#<procedure " + obj + ">";
      }

      if (obj is bool)
      {
        return IsTrue(obj) ? "#t" : "#f";
      }
      if (obj is string || obj is StringBuilder)
      {
        return string.Format("\"{0}\"", obj.ToString().
          Replace("\\", "\\\\").
          Replace("\"", "\\\"").
          Replace("\n", "\\n").
          Replace("\r", "\\r").
          Replace("\t", "\\t"));
      }
      if (obj is char)
      {
        switch ((char)obj)
        {
          case (char) 0: return "#\\nul";
          case (char) 7: return "#\\alarm";
          case (char) 8: return "#\\backspace";
          case (char) 9: return "#\\tab";
          case (char) 10: return "#\\newline";
          case (char) 11: return "#\\vtab";
          case (char) 12: return "#\\page";
          case (char) 13: return "#\\return";
          case (char) 27: return "#\\esc";
          case (char) 32: return "#\\space";
          case (char) 127: return "#\\delete";
        }

        return string.Format("#\\{0}", obj);
      }

      if (obj is Cons)
      {
        List<string> v = new List<string>();
        Cons s = obj as Cons;

        if (s != null)
        {
          string val = ReaderFormat(s, WriteFormat);
          if (val != null)
          {
            return val;
          }
        }

        while (s != null)
        {
          v.Add(WriteFormat(s.car));
          if (s.cdr != null && !(s.cdr is Cons))
          {
            v.Add(".");
            v.Add(WriteFormat(s.cdr));
            break;
          }
          s = s.cdr as Cons;
        }
        return string.Format("({0})", string.Join(" ", v.ToArray()));
      }

      if (obj is byte[])
      {
        List<string> v = new List<string>();
        foreach (object io in (IList)obj)
        {
          v.Add(WriteFormat(io));
        }

        return string.Format("#vu8({0})", string.Join(" ", v.ToArray()));
      }

      if (obj is object[])
      {
        List<string> v = new List<string>();
        foreach (object io in (IList)obj)
        {
          v.Add(WriteFormat(io));
        }

        return string.Format("#({0})", string.Join(" ", v.ToArray()));
      }

      if (obj is SymbolId)
      {
        string s = SymbolTable.IdToString((SymbolId)obj);
        StringBuilder sb = new StringBuilder();

        foreach (char c in s)
        {
          UnicodeCategory cat = char.GetUnicodeCategory(c);
          if (cat == UnicodeCategory.OtherNotAssigned || char.IsWhiteSpace(c))
          {
            sb.AppendFormat(@"\x{0:X};", (int)c);
          }
          else
          {
            sb.Append(c);
          }
        }

        var first = sb[0];

        if (char.IsDigit(first))
        {
          return string.Format(@"\x{0:X};{1}", (int) first, sb.ToString(1, sb.Length - 1));
        }

        return sb.ToString();
      }

      if (obj is NetworkStream)
      {
        return string.Format("#<tcp-binary-input/output-port>");
      }

      if (IsTrue(IsNumber(obj)))
      {
        return NumberToString(obj) as string;
      }

      ICallable printer;
      if (R6RS.Records.printers.TryGetValue(obj.GetType().FullName, out printer))
      {
        StringWriter p = new StringWriter();
#if CPS
        OptimizedBuiltins.Call(printer, obj, p, TRUE);
#else
        printer.Call(obj, p, TRUE);
#endif
        return p.GetBuffer();
      }

      //finally check if this is some constructed type
      if (R6RS.Records.IsRecordAny(obj))
      {
        return R6RS.Records.PrintRecord(obj);
      }

      if (obj is MultipleValues)
      {
        object[] values = ((MultipleValues)obj).ToArray();
        return string.Join("\n", Array.ConvertAll<object, string>(values, WriteFormat));
      }

      if (obj is IntPtr)
      {
        IntPtr p = (IntPtr)obj;
        return string.Format("#<pointer #x{0:X8}>", p.ToInt32());
      }

      return obj.ToString();
    }

    [Builtin("put-datum")]
    public static object PutDatum(object port, object datum)
    {
      return Write(datum, port);
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
      if (obj is BigInteger)
      {
        ((BigInteger)obj).Write(w);
      }
      else
      {
        string s = WriteFormat(obj);
        try
        {
          w.Write(s);
        }
        catch (Exception ex)
        {
          AssertionViolation("write", ex.Message, obj, port);
        }
      }
      return Unspecified;
    }


    [Builtin("input-port?")]
    public static object IsInputPort(object obj)
    {
      if (obj is Stream)
      {
        return GetBool(((Stream)obj).CanRead);
      }
      return GetBool(obj is TextReader || obj is R6RS.IO.CustomTextReaderWriter); 
    }

    [Builtin("output-port?")]
    public static object IsOutputPort(object obj)
    {
      if (obj is Stream)
      {
        return GetBool(((Stream)obj).CanWrite);
      }
      return GetBool(obj is TextWriter || obj is R6RS.IO.CustomTextReaderWriter);
    }

    //probably a good idea to make these threadstatic
    //[ThreadStatic]
    [Obsolete]
    static TextReader currentinputport = Console.In;
    //[ThreadStatic]
    [Obsolete]
    static TextWriter currentoutputport = Console.Out;
    //[ThreadStatic]
    [Obsolete]
    static TextWriter currenterrorport = Console.Error;

    [Builtin("current-input-port")]
    [Obsolete]
    public static object CurrentInputPort(object newport)
    {
      currentinputport = newport as TextReader;
      return Unspecified;
    }

    [Builtin("current-output-port")]
    [Obsolete]
    public static object CurrentOutputPort(object newport)
    {
      currentoutputport = newport as TextWriter;
      return Unspecified;
    }


    [Builtin("current-input-port")]
    [Obsolete]
    public static object CurrentInputPort()
    {
      return currentinputport;
    }

    [Builtin("current-output-port")]
    [Obsolete]
    public static object CurrentOutputPort()
    {
      return currentoutputport;
    }

    [Builtin("current-error-port")]
    [Obsolete]
    public static object CurrentErrorPort(object newport)
    {
      currenterrorport = newport as TextWriter;
      return Unspecified;
    }


    [Builtin("current-error-port")]
    [Obsolete]
    public static object CurrentErrorPort()
    {
      return currenterrorport;
    }
    
    static string GetPath(string filename)
    {
      if (filename.StartsWith("~"))
      {
        if (ApplicationDirectory != Environment.CurrentDirectory)
        {
          filename = Path.Combine(ApplicationDirectory, filename.Substring(2));
        }
        else
        {
          filename = filename.Substring(2);
        }
      }
      return filename;
    }
    
    [Builtin("open-input-file")]
    public static object OpenInputFile(object filename)
    {
      string fn = RequiresNotNull<string>(filename);
      try
      {
        return File.OpenText(GetPath(fn));
      }
      catch (FileNotFoundException ex)
      {
        return FileNotFoundViolation("open-input-file", ex.Message, filename);
      }
      catch (Exception ex)
      {
        return AssertionViolation("open-input-file", ex.Message, filename);
      }      
    }

    [Builtin("open-output-file")]
    public static object OpenOutputFile(object filename)
    {
      string fn = RequiresNotNull<string>(filename);
      try
      {
        return File.CreateText(GetPath(fn));
      }
      catch (FileNotFoundException ex)
      {
        return FileNotFoundViolation("open-output-file", ex.Message, filename);
      }
      catch (Exception ex)
      {
        return AssertionViolation("open-output-file", ex.Message, filename);
      }
    }

    [Builtin("close-input-port")]
    public static object CloseInputPort(object port)
    {
      if (readcache.ContainsKey(port))
      {
        readcache.Remove(port);
      }
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
