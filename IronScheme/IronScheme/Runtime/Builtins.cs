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
using Microsoft.Scripting;
using System.Diagnostics;
using System.Reflection;
using System.Collections;
using Microsoft.Scripting.Hosting;
using System.ComponentModel;
using Microsoft.Scripting.Utils;
using IronScheme.Compiler;
using System.IO;
using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Generation;
using System.Xml;
using System.Net;

namespace IronScheme.Runtime
{
  [AttributeUsage(AttributeTargets.Method, AllowMultiple=true)]
  public sealed class BuiltinAttribute : Attribute
  {
    string name;

    public string Name
    {
      get { return name; }
      set { name = value; }
    }

    public BuiltinAttribute()
    {

    }

    public BuiltinAttribute(string name)
    {
      this.name = name;
    }
  }

  public partial class Builtins : BaseHelper
  {
    protected internal readonly static object TRUE = RuntimeHelpers.True;
    protected internal readonly static object FALSE = RuntimeHelpers.False;

    [ThreadStatic]
    internal static Exception lastException = null;

    protected static Exception LastException
    {
      get { return lastException; }
    }

    public static bool IsTrue(object arg)
    {
      if (arg == FALSE)
      {
        return false;
      }
      else if (arg == TRUE)
      {
        return true;
      }
      else if (arg is bool)
      {
        return (bool)arg;
      }

      return true;
    }

    sealed class UnspecifiedObject 
    {
      internal UnspecifiedObject() { }

      public override string ToString()
      {
        return "#<unspecified>";
      }
    }

    [Builtin("optimization-level")]
    public static object OptLevel()
    {
      return Helpers.EnumToSymbol<OptimizationLevel>(IronScheme.Compiler.Generator.Optimization);
    }

    [Builtin("optimization-level")]
    public static object OptLevel(object level)
    {
      IronScheme.Compiler.Generator.Optimization = Helpers.SymbolToEnum<OptimizationLevel>(level);
      return Unspecified;
    }
    

    [Builtin("serialize-port")]
    public static object Serialize(object o, object binaryport)
    {
      Stream s = Requires<Stream>(binaryport);
      psyntax.Serialization.SERIALIZER.Serialize(s, o);
      return Unspecified;
    }

    [Builtin("deserialize-port")]
    public static object DeserializePort(object binaryport)
    {
      Stream s = Requires<Stream>(binaryport);
      return psyntax.Serialization.SERIALIZER.Deserialize(s);
    }


    public static readonly object Unspecified = new UnspecifiedObject();


    [Builtin("unspecified?")]
    public static object IsUnspecified(object o)
    {
      return GetBool(o == Unspecified);
    }

    internal static string[] commandline = { "not set" };

    [Builtin("command-line")]
    [Builtin("get-command-line")]
    public static object CommandLine()
    {
      return List(commandline);
    }

    [Builtin("clr-type?")]
    public static object IsClrType(object o)
    {
      return GetBool(o is Type);
    }


    [Builtin("get-clr-type")]
    public static object GetClrType(object name, params object[] typeargs)
    {
      SymbolId s = RequiresNotNull<SymbolId>(name);

      string tn = SymbolTable.IdToString(s);

      List<Type> candidates = new List<Type>();

      foreach (Assembly ass in AppDomain.CurrentDomain.GetAssemblies())
      {
        try
        {
          if (ass.ManifestModule.Name != "<In Memory Module>")
          {
            foreach (Type t in ass.GetExportedTypes())
            {
              string tnl = (t.Namespace + "." + t.Name);

              if (t.IsGenericType)
              {
                tnl = tnl.Substring(0, tnl.Length - 2);
              }

              if (tnl == tn)
              {
                candidates.Add(t);

                // we have right name and namespace, now for possible type args, confusing logic :|
                if (typeargs != null && typeargs.Length > 0)
                {
                  if (t.IsGenericType)
                  {
                    Type[] ga = t.GetGenericArguments();
                    if (typeargs.Length == ga.Length)
                    {
                      Type[] ta = new Type[typeargs.Length];
                      for (int i = 0; i < ta.Length; i++)
                      {
                        ta[i] = typeargs[i] as Type;
                        if (!ta[i].IsSubclassOf(ga[i].BaseType))
                        {
                          continue;
                        }
                      }

                      try
                      {
                        return t.MakeGenericType(ta);
                      }
                      catch
                      {
                        continue;
                      }
                    }
                  }
                }
                else if (!t.IsGenericType)
                {
                  return t;
                }
              }
            }
          }
        }
        catch (Exception)
        {
          //mono?
        }
      }

      return AssertionViolation("get-clr-type", "type not found",
        Cons( name, List( typeargs)),
        Runtime.Cons.FromList(candidates));
    }


    [Builtin]
    public static Type Typeof(object o)
    {
      if (o == null)
      {
        return typeof(Cons);
      }
      return o.GetType();
    }

    public static object ListToByteVector(object obj)
    {
      object[] bytes = ListToVector(obj) as object[];
      byte[] buffer = new byte[bytes.Length];
      for (int i = 0; i < buffer.Length; i++)
      {
        buffer[i] = Convert.ToByte(bytes[i]);
      }

      return buffer;
    }

    public static string ApplicationDirectory
    {
      get
      {
        return Path.GetDirectoryName(typeof(Builtins).Assembly.CodeBase).Replace("file:\\", "").Replace("file:", "");
      }
    }

    /// <summary>
    /// Displays the license of IronScheme.
    /// </summary>
    [Builtin]
    public static object License()
    {
      return Display(@"Microsoft Public License (Ms-PL)
================================

This license governs use of the accompanying software. If you use the software, you accept this license. If you do not accept the license, do not use the software.

1. Definitions
The terms ""reproduce,"" ""reproduction,"" ""derivative works,"" and ""distribution"" have the same meaning here as under U.S. copyright law.
A ""contribution"" is the original software, or any additions or changes to the software.
A ""contributor"" is any person that distributes its contribution under this license.
""Licensed patents"" are a contributor's patent claims that read directly on its contribution.

2. Grant of Rights
(A) Copyright Grant- Subject to the terms of this license, including the license conditions and limitations in section 3, each contributor grants you a non-exclusive, worldwide, royalty-free copyright license to reproduce its contribution, prepare derivative works of its contribution, and distribute its contribution or any derivative works that you create.
(B) Patent Grant- Subject to the terms of this license, including the license conditions and limitations in section 3, each contributor grants you a non-exclusive, worldwide, royalty-free license under its licensed patents to make, have made, use, sell, offer for sale, import, and/or otherwise dispose of its contribution in the software or derivative works of the contribution in the software.

3. Conditions and Limitations
(A) No Trademark License- This license does not grant you rights to use any contributors' name, logo, or trademarks.
(B) If you bring a patent claim against any contributor over patents that you claim are infringed by the software, your patent license from such contributor to the software ends automatically.
(C) If you distribute any portion of the software, you must retain all copyright, patent, trademark, and attribution notices that are present in the software.
(D) If you distribute any portion of the software in source code form, you may do so only under this license by including a complete copy of this license with your distribution. If you distribute any portion of the software in compiled or object code form, you may only do so under a license that complies with this license.
(E) The software is licensed ""as-is."" You bear the risk of using it. The contributors give no express warranties, guarantees or conditions. You may have additional consumer rights under your local laws which this license cannot change. To the extent permitted under your local laws, the contributors exclude the implied warranties of merchantability, fitness for a particular purpose and non-infringement.
");
    }

    [Builtin]
    public static object StackTrace()
    {
      Exception ex = LastException;
      if (ex != null)
      {
        ConsoleColor old = Console.ForegroundColor;
        Console.ForegroundColor = ConsoleColor.Red;
        Console.WriteLine(ex.StackTrace);
        Console.ForegroundColor = old;
      }
      return Unspecified;
    }

    [Builtin("make-guid")]
    public static object MakeGuid()
    {
      return Guid.NewGuid();
    }


    [Builtin("get-library-paths")]
    public static Cons GetLibraryPaths()
    {
      if (Environment.CurrentDirectory == ApplicationDirectory)
      {
        return List(
          ApplicationDirectory,
          Path.Combine(ApplicationDirectory, "lib"));
      }
      else
      {
        return List(
          ".",
          ApplicationDirectory,
          Path.Combine(ApplicationDirectory, "lib"));
      }
    }

    [Builtin("procedure-arity")]
    public static object ProcArity(object proc)
    {
      ICallable c = RequiresNotNull<ICallable>(proc);
      return c.Arity;
    }

    [Builtin("procedure-form")]
    public static object ProcForm(object proc)
    {
      ICallable c = RequiresNotNull<ICallable>(proc);
      return FALSE;
    }

    [Builtin("procedure-name")]
    public static object ProcName(object proc)
    {
      ICallable c = RequiresNotNull<ICallable>(proc);
      return SymbolTable.StringToId(c.ToString());
    }

    [Builtin("procedure-environment")]
    public static object ProcEnv(object proc)
    {
      ICallable c = RequiresNotNull<ICallable>(proc);
      return FALSE;
    }

    [Builtin("make-traced-procedure")]
    [CLSCompliant(false)]
    public static ICallable MakeTraceProcedure(object name, object proc)
    {
      return MakeTraceProcedure(name, proc, FALSE);
    }

    [Builtin("make-traced-procedure")]
    [CLSCompliant(false)]
    public static ICallable MakeTraceProcedure(object name, object proc, object filter)
    {
      ICallable p = RequiresNotNull<ICallable>(proc);
      SymbolId n = RequiresNotNull<SymbolId>(name);
      ICallable f = filter as ICallable;
      return new TraceClosure(p, n, f);
    }

    static Process selfprocess;
    
    [Builtin("time-it")]
    public static object TimeIt(object who, object thunk)
    {
      ICallable c = RequiresNotNull<ICallable>(thunk);

      int colcount = 0;
      for (int i = 0; i < 3; i++)
      {
        colcount += GC.CollectionCount(i);
      }

      if (selfprocess == null)
      {
        selfprocess = Process.GetCurrentProcess();
      }

      TimeSpan totalts = selfprocess.TotalProcessorTime,
        userts = selfprocess.UserProcessorTime;
      
      Stopwatch sw = Stopwatch.StartNew();
      try
      {
        return c.Call();
      }
      finally
      {
        sw.Stop();
        TimeSpan totalts2 = selfprocess.TotalProcessorTime,
          userts2 = selfprocess.UserProcessorTime;

        int colcountafter = 0;
        for (int i = 0; i < 3; i++)
        {
          colcountafter += GC.CollectionCount(i);
        }

        Console.WriteLine(@"Statistics for '{0}':
  Real Time:  {1:f0}ms
  CPU Time:   {2:f0}ms
  User Time:  {3:f0}ms
  GC's:       {4}", who, 
                     sw.Elapsed.TotalMilliseconds, 
                     (userts2 - userts).TotalMilliseconds,
                     (totalts2 - totalts).TotalMilliseconds,
                     colcountafter - colcount);
      }
    }

    static int evalcounter = 0;

    [Builtin("compile-core")]
    public static object CompileCore(CodeContext cc, object expr)
    {
      AssemblyGenAttributes aga = ScriptDomainManager.Options.AssemblyGenAttributes;

      ScriptDomainManager.Options.AssemblyGenAttributes &= ~AssemblyGenAttributes.GenerateDebugAssemblies;
      ScriptDomainManager.Options.AssemblyGenAttributes &= ~AssemblyGenAttributes.EmitDebugInfo;
      ScriptDomainManager.Options.AssemblyGenAttributes &= ~AssemblyGenAttributes.DisableOptimizations;
      // if you ever want to inspect the emitted dll's comment this out, use with care
      ScriptDomainManager.Options.AssemblyGenAttributes &= ~AssemblyGenAttributes.SaveAndReloadAssemblies;

      int c = ++evalcounter;

#if DEBUG_FULL

      // bad for ASP.NET
      if (Assembly.GetEntryAssembly() != null)
      {
        System.Threading.ThreadPool.QueueUserWorkItem(delegate(object state)
        {
          ICallable prettyprint = null;
          SymbolId pp = SymbolTable.StringToId("pretty-print");
          if (cc.Scope.ContainsName(pp))
          {
            prettyprint = SymbolValue(pp) as ICallable;
          }

          if (!Directory.Exists("evaldump"))
          {
            Directory.CreateDirectory("evaldump");
          }

          string fn = string.Format("evaldump/{0:D3}.ss", c);

          if (File.Exists(fn))
          {
            File.Delete(fn);
          }

          using (TextWriter w = File.CreateText(fn))
          {
            if (prettyprint == null)
            {
              Write(expr, w);
            }
            else
            {
              prettyprint.Call(expr, w);
            }
          }
        });
      }

#endif

#if DEBUG
      Stopwatch sw = Stopwatch.StartNew();
#endif
      CodeBlock cb = IronSchemeLanguageContext.CompileExpr(new Cons(expr));
      cb.ExplicitCodeContextExpression = null;

      ScriptCode sc = cc.LanguageContext.CompileSourceCode(cb); //wrap

#if DEBUG
      Trace.WriteLine(sw.Elapsed.TotalMilliseconds, string.Format("compile - eval-core({0:D3})", c));
      sw = Stopwatch.StartNew();
#endif
      try
      {
        ScriptModule sm = ScriptDomainManager.CurrentManager.CreateModule(string.Format("eval-core({0:D3})", c), sc);
        sc = sm.GetScripts()[0];
      }
      catch (Variable.UnInitializedUsageException ex)
      {
        ScriptDomainManager.Options.AssemblyGenAttributes = aga;
        CallTarget0 err = delegate
        {
          return AssertionViolation(ex.Variable.Block.Name, ex.Message, UnGenSym(ex.Variable.Name));
        };

        return Closure.Make(cc, err);
      }
#if DEBUG
      Trace.WriteLine(sw.Elapsed.TotalMilliseconds, string.Format("compile*- eval-core({0:D3})", c));
#endif
      CallTarget0 compiled = delegate
      {
        try
        {
#if DEBUG
          sw = Stopwatch.StartNew();
#endif
          return sc.Run(cc.ModuleContext.Module);
        }
        finally
        {
#if DEBUG
          Trace.WriteLine(sw.Elapsed.TotalMilliseconds, string.Format("run     - eval-core({0:D3})", c));
#endif
        }
      };

      ScriptDomainManager.Options.AssemblyGenAttributes = aga;
      Compiler.SimpleGenerator.ClearGlobals();

      return Closure.Make(cc, compiled);
    }

    [Builtin("eval-core")]
    public static object EvalCore(CodeContext cc, object expr)
    {
      ICallable compiled = CompileCore(cc, expr) as ICallable;
      return compiled.Call();
    }


    [Builtin("gc-collect")]
    public static object GcCollect()
    {
      GC.Collect();
      return Unspecified;
    }

    [Builtin]
    public static object Void()
    {
      return Unspecified;
    }

    static object ListStarHelper(object a, object rest)
    {
      return (rest == null) ? a : new Cons(a, ListStarHelper(Car(rest), Cdr(rest)));
    }

    [Builtin("list*")]
    public static object ListStar(object a, params object[] rest)
    {
      return ListStarHelper(a, List(rest));
    }

    [Builtin("list*")]
    public static Cons ListStar(object a, object b)
    {
      return new Cons(a, b);
    }

    [Builtin("list*")]
    public static Cons ListStar(object a, object b, object c)
    {
      return new Cons(a, new Cons(b, c));
    }

    [Builtin("list*")]
    public static Cons ListStar(object a, object b, object c, object d)
    {
      return new Cons(a, new Cons(b, new Cons(c , d)));
    }

    static Scope ModuleScope;

    [Builtin("symbol-value")]
    public static object SymbolValue(SymbolId symbol)
    {
      if (ModuleScope == null)
      {
        ModuleScope = BaseHelper.cc.Scope.ModuleScope;
      }
      return ModuleScope.LookupName(symbol);
    }

    [Builtin("set-symbol-value!")]
    public static object SetSymbolValue(SymbolId symbol, object value)
    {
      if (ModuleScope == null)
      {
        ModuleScope = BaseHelper.cc.Scope.ModuleScope;
      }
      ModuleScope.SetName(symbol, value);
      return Unspecified;
    }


    [Builtin("string-split")]
    public static object StringSplit(object str, params object[] del)
    {
      return Requires<string>(str).Split(del as string[], StringSplitOptions.None);
    }

    [Builtin("string->xml")]
    public static object StringToXml(object str)
    {
      string s = Requires<string>(str);
      XmlDocument doc = new XmlDocument();
      doc.LoadXml(s);

      Cons c = ReadElement(doc.DocumentElement);

      return c;
    }



    static Cons ReadElement(XmlElement e)
    {
      List<object> all = new List<object>();
      all.Add(SymbolTable.StringToId(e.Name));

      foreach (XmlAttribute a in e.Attributes)
      {
        all.Add(ReadAttribute(a));    
      }

      foreach (XmlNode n in e.ChildNodes)
      {
        if (n.NodeType == XmlNodeType.Text)
        {
          all.Add(n.InnerText);
        }
        else
        {
          all.Add(ReadElement(n as XmlElement));
        }
      }

      return List(all.ToArray());
    }


    static Cons ReadAttribute(XmlAttribute a)
    {
      return Cons(SymbolTable.StringToId(a.Name), a.Value);
    }

    [Builtin("download-string")]
    public static object DownloadString(object str)
    {
      string s = Requires<string>(str);
      using (WebClient wc = new WebClient())
      {
        return wc.DownloadString(s);
      }
    }
    

    static void RequiresCondition(bool condition, string message)
    {
      if (!condition)
      {
        AssertionViolation(GetCaller(), message);
      }
    }

    protected static object RequiresNotNull(object obj)
    {
      if (obj == null)
      {
        return AssertionViolation(GetCaller(), "argument cannot be null");
      }
      return obj;
    }

    protected static T Requires<T>(object obj)
    {
      if (obj != null && !(obj is T))
      {
        return (T) AssertionViolation(GetCaller(), "expected type: " + typeof(T).Name, obj);
      }
      if (obj == null)
      {
        return default(T);
      }
      return (T)obj;
    }

    protected static SymbolId GetCaller()
    {
      StackTrace st = new StackTrace(2);
      MethodBase m = st.GetFrame(0).GetMethod();
      foreach (BuiltinAttribute ba in m.GetCustomAttributes(typeof(BuiltinAttribute), false))
      {
        return SymbolTable.StringToId(ba.Name ?? m.Name.ToLower());
      }
      return UnGenSymInternal(SymbolTable.StringToId(m.Name));
    }

    protected static T RequiresNotNull<T>(object obj)
    {
      if (obj == null)
      {
        return (T) AssertionViolation(GetCaller(), "argument cannot be null");
      }

      if (typeof(T) == typeof(string) && obj is StringBuilder)
      {
        return (T)(object)obj.ToString();
      }

      if (typeof(T) == typeof(TextWriter) && obj is R6RS.IO.CustomTextReaderWriter)
      {
        return (T)(object)((R6RS.IO.CustomTextReaderWriter)obj).output;
      }

      if (typeof(T) == typeof(TextReader) && obj is R6RS.IO.CustomTextReaderWriter)
      {
        return (T)(object)((R6RS.IO.CustomTextReaderWriter)obj).input;
      }

      if (obj != null && !(obj is T))
      {
        return (T) AssertionViolation(GetCaller(), "expected type: " + typeof(T).Name,  obj);
      }

      return (T)obj;
    }

 

 

  }
}
