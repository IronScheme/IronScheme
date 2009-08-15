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
using IronScheme.Runtime.psyntax;

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
  }

  public partial class Builtins
  {
    static Assembly AssemblyLoad(string path)
    {
      string fn = Path.GetFullPath(path);
      var pdb = Path.ChangeExtension(fn, ".pdb");
      if (File.Exists(pdb))
      {
        return Assembly.Load(File.ReadAllBytes(fn), File.ReadAllBytes(pdb));
      }
      else
      {
        return Assembly.LoadFrom(fn);
      }
    }

    internal static object Load(object filename)
    {
      CodeContext cc = IronScheme.Compiler.BaseHelper.cc; // sneaky....

      string path = GetPath(filename as string);

      switch (Path.GetExtension(path))
      {
        case ".fasl":
          {
#if DEBUG
            Stopwatch sw = Stopwatch.StartNew();
#endif
            Cons c = null;
            using (Stream s = File.OpenRead(path))
            {
              c = psyntax.Serialization.DeserializePort(s) as Cons;
            }

            var cb = IronSchemeLanguageContext.Compile(c);

            ScriptCode sc = cc.LanguageContext.CompileSourceCode(cb);

            var sm = ScriptDomainManager.CurrentManager.CreateModule("boot", sc);

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
        case ".exe":
        case ".dll":
          const string newbf = "ironscheme.boot.new.dll";
          if (File.Exists(newbf) && File.GetLastWriteTime(newbf) > File.GetLastWriteTime(path))
          {
            if (File.Exists("ironscheme.boot.old.dll"))
            {
              File.Delete("ironscheme.boot.old.dll");
            }

            File.Move("ironscheme.boot.dll", "ironscheme.boot.old.dll");
            File.Move(newbf, "ironscheme.boot.dll");
          }

          if (File.Exists("ironscheme.boot.new.pdb"))
          {
            if (File.Exists("ironscheme.boot.pdb"))
            {
              File.Delete("ironscheme.boot.pdb");
            }

            File.Move("ironscheme.boot.new.pdb", "ironscheme.boot.pdb");
          }

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
          {
            // check for already compiled version
            string cfn = Path.ChangeExtension(path, ".dll");
            if (File.Exists(cfn))
            {
              DateTime ct = File.GetLastWriteTime(cfn);
              if (!File.Exists(path) || ct >= File.GetLastWriteTime(path))
              {
                if (File.GetLastWriteTime(Path.Combine(ApplicationDirectory, "IronScheme.dll")) <= ct ||
                  cfn.EndsWith("ironscheme.boot.dll"))
                {
                  path = cfn;
                  goto case ".dll";
                }
              }
            }

            cfn = Path.ChangeExtension(path, ".new.dll");
            if (File.Exists(cfn))
            {
              DateTime ct = File.GetLastWriteTime(cfn);
              if (!File.Exists(path) || ct >= File.GetLastWriteTime(path))
              {
                if (File.GetLastWriteTime(Path.Combine(ApplicationDirectory, "IronScheme.dll")) <= ct ||
                  cfn.EndsWith("ironscheme.boot.dll"))
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

      }

      return Unspecified;
    }

    [Builtin("read")]
    [Obsolete]
    public static object Read()
    {
      return Read(CurrentInputPort());
    }

    //[ThreadStatic]
    static Dictionary<object, Cons> readcache = new Dictionary<object, Cons>();

    [Builtin("read")]
    [Obsolete]
    public static object Read(object port)
    {
      var r = ReadAnnotatedNext(port);
      if (r is Annotation)
      {
        return ((Annotation)r).stripped;
      }
      return r;
    }

    [Builtin("read-annotated")]
    public static object ReadAnnotated(object port)
    {
      string filename = "";
      if (port is StreamReader)
      {
        StreamReader r = (StreamReader)port;
        if (r.BaseStream is FileStream)
        {
          FileStream fs = (FileStream)r.BaseStream;
          filename = fs.Name.Replace(Environment.CurrentDirectory + "\\", string.Empty);
        }
        
      }

      IronScheme.Compiler.AnnotationHelper.Filename = filename;

      var res = ReadAnnotatedNext(port);
      return res;
    }


    [Builtin("make-annotation")]
    public static object MakeAnnotation(object expr, object source, object stripped)
    {
      if (expr is Cons && stripped is Cons)
      {
        return new ConsAnnotation(expr as Cons, source, stripped as Cons);
      }
      else
      {
        return new Annotation(expr, source, stripped);
      }
    }


    [Builtin("annotation?")]
    [Obsolete]
    public static object IsAnnotation(object obj)
    {
      return GetBool(obj is Annotation);
    }

    [Builtin("annotation-expression")]
    [Obsolete]
    public static object AnnotationExpression(object obj)
    {
      return RequiresNotNull<Annotation>(obj).expression;
    }

    [Builtin("annotation-source")]
    [Obsolete]
    public static object AnnotationSource(object obj)
    {
      return RequiresNotNull<Annotation>(obj).source;
    }

    [Builtin("annotation-stripped")]
    [Obsolete]
    public static object AnnotationStripped(object obj)
    {
      return RequiresNotNull<Annotation>(obj).stripped;
    }

    static object ReadAnnotatedNext(object port)
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


    [Builtin("input-port?")]
    [Obsolete]
    public static object IsInputPort(object obj)
    {
      if (obj is Stream)
      {
        return GetBool(((Stream)obj).CanRead);
      }
      return GetBool(obj is TextReader || obj is R6RS.CustomTextReaderWriter); 
    }

    [Builtin("output-port?")]
    [Obsolete]
    public static object IsOutputPort(object obj)
    {
      if (obj is Stream)
      {
        return GetBool(((Stream)obj).CanWrite);
      }
      return GetBool(obj is TextWriter || obj is R6RS.CustomTextReaderWriter);
    }

    //probably a good idea to make these threadstatic
    //[ThreadStatic]
    //[Obsolete]
    static TextReader currentinputport = Console.In;
    //[ThreadStatic]
    [Obsolete]
    static TextWriter currentoutputport = Console.Out;
    //[ThreadStatic]
    [Obsolete]
    static TextWriter currenterrorport = Console.Error;

    [Builtin("current-input-port")]
    //[Obsolete]
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
    //[Obsolete]
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
    [Obsolete]
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
    [Obsolete]
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
    [Obsolete]
    public static object CloseInputPort(object port)
    {
      if (port is Stream)
      {
        ((Stream)port).Close();
      }
      else
      {
        if (readcache.ContainsKey(port))
        {
          readcache.Remove(port);
        }
        RequiresNotNull<TextReader>(port).Close();
      }
      return Unspecified;
    }

    [Builtin("close-output-port")]
    [Obsolete]
    public static object CloseOutputPort(object port)
    {
      if (port is Stream)
      {
        ((Stream)port).Close();
      }
      else
      {
        RequiresNotNull<TextWriter>(port).Close();
      }
      return Unspecified;

    }
  }
}
