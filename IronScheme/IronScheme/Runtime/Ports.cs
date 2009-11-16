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

    internal static Assembly BootfileAssembly { get; set; }

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

          // just reference.?
          MethodInfo entry = null;

          Type ilmergefixup = typeof(Builtins).Assembly.GetType("#", false);

          if (ilmergefixup != null)
          {
            entry = ilmergefixup.GetMethod("Initialize");
            BootfileAssembly = typeof(Builtins).Assembly;
          }
          else
          {

            Assembly ext = AssemblyLoad(path);

            BootfileAssembly = ext;

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
          }

          if (entry == null)
          {
            // what now?
            throw new ArgumentException("No entry point");
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
              throw new FileNotFoundException("Not found", path);
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
    }

    //[ThreadStatic]
    static Dictionary<object, Cons> readcache = new Dictionary<object, Cons>();

    //[Builtin("read")]
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
        catch (SyntaxErrorException ex)
        {
          return LexicalError(ex.Message, ex.SourceUnit.ToString());
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
  }
}
