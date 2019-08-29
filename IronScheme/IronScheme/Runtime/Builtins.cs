#region License
/* Copyright (c) 2007-2016 Llewellyn Pritchard
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.CodeDom.Compiler;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Net.Sockets;
using System.Reflection;
using System.Reflection.Emit;
using System.Text;
using System.Threading;
using System.Xml;
using IronScheme.Compiler;
using Microsoft.Scripting;
using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Generation;

namespace IronScheme.Runtime
{
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

    //[DebuggerHidden]
    [DebuggerStepThrough]
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

    delegate object Func();

    [Builtin("ironscheme-version")]
    public static object Version()
    {
      return IronScheme.Hosting.IronSchemeConsoleHost.VERSION;
    }

    [Builtin("generate-executable-wrapper")]
    public static object GenWrapper(object filename)
    {
      var tmpl = Path.Combine(ApplicationDirectory, "Executable.cs.template");
      var fn = RequiresNotNull<string>(filename);
      using (var p = new Microsoft.CSharp.CSharpCodeProvider())
      {
        var supportfn = filename + ".cs";
        File.WriteAllText(supportfn, string.Format(
@"
namespace IronScheme
{{
  partial class ExecutableTemplate
  {{
    const string PATH = @""{0}"";
    const string RESOURCE = @""{1}"";
  }}
}}", ApplicationDirectory, fn));

        var cp = new CompilerParameters
        {
          EmbeddedResources = { fn },
          OutputAssembly = Path.GetFileNameWithoutExtension(fn) + ".exe",
          ReferencedAssemblies = { "System.dll",  "System.Configuration.dll" },
          GenerateExecutable = true,
          //IncludeDebugInformation = true
        };

        Console.Write("compiling executable wrapper '{0}'.... ", cp.OutputAssembly);
        var results = p.CompileAssemblyFromFile(cp, tmpl, supportfn);
        if (results.Errors.Count > 0)
        {
          Console.WriteLine("failed.");
          foreach (var error in results.Errors)
          {
            Console.Error.WriteLine(error);
          }
        }
        else
        {
          Console.WriteLine("done.");
          Console.Write("generating config file .... ");
          File.WriteAllText(cp.OutputAssembly + ".config", string.Format(
@"<?xml version=""1.0""?>
<configuration>
  <appSettings>
    <add key=""IronScheme.Directory"" value=""{0}""/>
  </appSettings>
</configuration>", ApplicationDirectory));
          Console.WriteLine("done.");
        }
        File.Delete(supportfn);
      }
      return Unspecified;
    }


    [Builtin("with-timeout")]
    public static object WithTimeout(object proc, object duration)
    {
      var d = Requires<int>(duration);
      var p = new Func(Requires<Callable>(proc).Call);

      var reset = new AutoResetEvent(false);
      object r = null;
      Exception ex = null;

      var t = new Thread(() =>
      {
        try
        {
          r = p();
        }
        catch (Exception e)
        {
          ex = e;
        }
        reset.Set();
      });

      t.Start();

      if (!reset.WaitOne(d))
      {
        t.Abort();
        return AssertionViolation("with-timeout", string.Format("call exceeded limit: {0}ms", d), null);
      }

      if (ex != null)
      {
        var sex = ex as SchemeException;
        if (sex != null)
        {
          return Raise(sex.Condition);
        }
        throw ex;
      }

      return r;
    }

    [Builtin("serialize-port")]
    [UnspecifiedReturn]
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
      var o = psyntax.Serialization.SERIALIZER.UnsafeDeserialize(s, null);
      return o;
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
      return Runtime.Cons.FromList(commandline);
    }

    public static MethodBuilder MakeMethod(string name, Type returntype, Type[] paramtypes)
    {
      var mb = ModuleBuilder;
      var tb = mb.DefineType(Guid.NewGuid().ToString(), TypeAttributes.Public);

      var meth = tb.DefineMethod(name, MethodAttributes.Public | MethodAttributes.Static, returntype, paramtypes);

      return meth;
    }

    public static Delegate CreateDelegate(MethodBuilder mb, Type deltype)
    {
      var tb = mb.DeclaringType as TypeBuilder;
      var t = tb.CreateType();

      var meth = t.GetMethod(mb.Name);

      return Delegate.CreateDelegate(deltype, meth);
    }

    public static ModuleBuilder ModuleBuilder
    {
      get
      {
        return ScriptDomainManager.CurrentManager.Snippets.Assembly.ModuleBuilder;
      }
    }

    [Builtin("get-clr-type")]
    public static object GetClrType(object name)
    {
      return GetClrType(name, new object[0]);
    }

    [Builtin("get-clr-type")] 
#warning params could be dangerous to the calling convention. check me. this will probably never called with a vector argument
    public static object GetClrType(object name, params object[] typeargs)
    {
      SymbolId s = RequiresNotNull<SymbolId>(name);

      string tn = SymbolTable.IdToString(s);

      var t = ClrGenerator.GetTypeFast(tn);

      if (t == null)
      {
        return FALSE;
      }

      if (typeargs.Length == 0)
      {
        return t;
      }

      return t.MakeGenericType(Array.ConvertAll(typeargs, x => (Type)x));
    }

    static internal bool compressConstants = false;

    [Builtin("compress-constants?")]
    public static object CompressConstants()
    {
      return RuntimeHelpers.BooleanToObject(compressConstants);
    }

    [Builtin("compress-constants?")]
    public static object CompressConstants(object value)
    {
      compressConstants = IsTrue(value);
      return Unspecified;
    }
    
    [Builtin("strict-mode?")]
    public static object StrictMode()
    {
      return RuntimeHelpers.BooleanToObject(ScriptDomainManager.Options.StrictMode);
    }

    [Builtin("strict-mode?")]
    public static object StrictMode(object value)
    {
      ScriptDomainManager.Options.StrictMode = IsTrue(value);
      return Unspecified;
    } 

    public static object ListToByteVector(object obj)
    {
      object[] bytes = ListToVector(obj);
      byte[] buffer = new byte[bytes.Length];
      for (int i = 0; i < buffer.Length; i++)
      {
        try
        {
          buffer[i] = Convert.ToByte(bytes[i]);
        }
        catch (Exception ex)
        {
          return LexicalError(ex.Message, bytes[i]);
        }
      }

      return buffer;
    }

    public static string ApplicationDirectory
    {
      get
      {
        var loc = typeof(Builtins).Assembly.CodeBase;
        //Replace("file:///","") breaks on Mono...
        if (!IsMono)
        {
          loc = loc.Replace("file:///","");
        }

        return Path.GetDirectoryName(loc.Replace("file:\\", "").Replace("file:", ""));
      }
    }

    [Builtin("debug-mode?")]
    public static object IsDebugMode()
    {
      return GetBool(ScriptDomainManager.Options.DebugMode);
    }

    [Builtin("debug-mode?")]
    public static object IsDebugMode(object newmode)
    {
      ScriptDomainManager.Options.DebugMode = IsTrue(newmode);
      return Unspecified;
    }

    [Builtin("lw-debug-mode?")]
    public static object IsLightWeightDebugMode()
    {
      return GetBool(ScriptDomainManager.Options.LightweightDebugging);
    }

    [Builtin("lw-debug-mode?")]
    [UnspecifiedReturn]
    public static object IsLightWeightDebugMode(object newmode)
    {
      ScriptDomainManager.Options.LightweightDebugging = IsTrue(newmode);
      return Unspecified;
    }

    [Builtin("lw-debugger")]
    public static object LightWeightDebugger()
    {
      return Microsoft.Scripting.Debugging.Debug.Debugger ?? FALSE;
    }

    [Builtin("lw-debugger")]
    [UnspecifiedReturn]
    public static object LightWeightDebugger(object debugger)
    {
      if (debugger == FALSE)
      {
        ScriptDomainManager.Options.LightweightDebugging = false;
        Microsoft.Scripting.Debugging.Debug.Debugger = null;
      }
      else
      {
        ScriptDomainManager.Options.LightweightDebugging = true;
        Microsoft.Scripting.Debugging.Debug.Reset();
        Microsoft.Scripting.Debugging.Debug.Debugger = new SchemeDebugger(RequiresNotNull<Callable>(debugger));
      }
      return Unspecified;
    }

    [Builtin("lw-debugger-call-stack")]
    public static object LightWeightDebuggerCallStack()
    {
      return Runtime.Cons.FromList(Microsoft.Scripting.Debugging.Debug.CallStack);
    }

    [Builtin("lw-debugger-location-trace")]
    public static object LightWeightDebuggerLocationTrace()
    {
      return Runtime.Cons.FromList(Microsoft.Scripting.Debugging.Debug.LocationTrace);
    }

    [Builtin("lw-debugger-stackframe-variables")]
    public static object LightWeightDebuggerStackframeVariables(object stackframe)
    {
      var sf = RequiresNotNull<Microsoft.Scripting.Debugging.StackFrame>(stackframe);
      return sf.Context.GetEnvironmentVariables();
    }

    internal readonly static List<string> includepaths = new List<string>();

    public static void AddIncludePath(string path)
    {
      includepaths.Add(path);
    }

    [Builtin("get-library-paths")]
    public static object GetLibraryPaths()
    {
      List<string> paths = new List<string>();

      paths.AddRange(includepaths);

      if (Environment.CurrentDirectory != ApplicationDirectory)
      {
        paths.Add(".");
      }

      paths.Add(Path.Combine(ApplicationDirectory, "lib"));
      paths.Add(ApplicationDirectory);

      return Runtime.Cons.FromList(paths);
    }

    static Process selfprocess;
    
    [Builtin("time-it")]
    public static object TimeIt(object who, object thunk)
    {
      Callable c = RequiresNotNull<Callable>(thunk);

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
      
      Stopwatch sw = new Stopwatch();
      try
      {
        if (Equals(c.Arity, 1))
        {
          return c.Call(sw);
        }
        else
        {
          sw.Start();
          var result = c.Call();
          sw.Stop();
          return result;
        }
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

        "(display {0} (current-error-port))".Eval(string.Format(@"Statistics for '{0}':
  Real Time:  {1:f0}ms
  CPU Time:   {2:f0}ms
  User Time:  {3:f0}ms
  GC's:       {4}
",                   who, 
                     sw.Elapsed.TotalMilliseconds, 
                     (userts2 - userts).TotalMilliseconds,
                     (totalts2 - totalts).TotalMilliseconds,
                     colcountafter - colcount));
        GC.Collect(); // to prevent subsequent loading
      }
    }

    public static object MakePrimlocHashtable(Cons map)
    {
      var ht = new Hashtable(1024);

      foreach (Cons m in map)
      {
        ht[m.car] = m.cdr;
      }

      return ht;
    }

    [Builtin("load-library-dll")]
    public static object LoadLibraryDll(object filename)
    {
      if (!File.Exists(filename as string))
      {
        return FALSE;
      }
      try
      {
        var o = Load(filename, false);
        return o;
      }
      catch
      {
        return FALSE;
      }
    }

    [Builtin("compile-bootfile")]
    public static object CompileBootfile(object libs)
    {
      AssemblyGenAttributes aga = ScriptDomainManager.Options.AssemblyGenAttributes;
      ScriptDomainManager.Options.AssemblyGenAttributes &= ~AssemblyGenAttributes.EmitDebugInfo;
      ScriptDomainManager.Options.AssemblyGenAttributes &= ~AssemblyGenAttributes.GenerateDebugAssemblies;
      ScriptDomainManager.Options.AssemblyGenAttributes &= ~AssemblyGenAttributes.DisableOptimizations;

      if (ScriptDomainManager.Options.DebugMode)
      {
        ScriptDomainManager.Options.AssemblyGenAttributes |= AssemblyGenAttributes.EmitDebugInfo;
        ScriptDomainManager.Options.AssemblyGenAttributes |= AssemblyGenAttributes.GenerateDebugAssemblies;
        ScriptDomainManager.Options.AssemblyGenAttributes |= AssemblyGenAttributes.DisableOptimizations;

        ScriptDomainManager.Options.DebugCodeGeneration = true;
      }
      else
      {
        ScriptDomainManager.Options.DebugCodeGeneration = false;
      }

      ScriptDomainManager.Options.AssemblyGenAttributes |= AssemblyGenAttributes.SaveAndReloadAssemblies;


      ScriptDomainManager.CurrentManager.Snippets.CurrentAssembly =
        Compiler.Generator.CurrentAssemblyGen = AssemblyGen.CreateModuleAssembly("ironscheme.boot.new.dll");

      //Console.WriteLine(new Cons(libs).PrettyPrint);

      CodeBlock cb = IronSchemeLanguageContext.CompileExpr(libs as Cons);
      cb.ExplicitCodeContextExpression = null;
      cb.Name = "ironscheme.boot.new";

      ScriptCode sc = cc.LanguageContext.CompileSourceCode(cb);

      sc.LibraryGlobals = Compiler.SimpleGenerator.libraryglobals;
      sc.LibraryGlobalsN = Compiler.SimpleGenerator.libraryglobalsN;
      sc.LibraryGlobalsX = Compiler.SimpleGenerator.libraryglobalsX;

      sc.SourceUnit.IsVisibleToDebugger = true;

      ScriptModule sm = ScriptDomainManager.CurrentManager.CreateModule("ironscheme.boot.new", sc);

      ScriptDomainManager.Options.AssemblyGenAttributes = aga;

      sc.ClearCache();
      Compiler.SimpleGenerator.ClearGlobals();

      return TRUE;
    }

    [Builtin("compile-library")]
    public static object CompileLibrary(object filename, object content)
    {
      IronScheme.Compiler.Generator.LocationHint = null;
      AssemblyGenAttributes aga = ScriptDomainManager.Options.AssemblyGenAttributes;
      ScriptDomainManager.Options.AssemblyGenAttributes &= ~AssemblyGenAttributes.EmitDebugInfo;
      ScriptDomainManager.Options.AssemblyGenAttributes &= ~AssemblyGenAttributes.GenerateDebugAssemblies;
      ScriptDomainManager.Options.AssemblyGenAttributes &= ~AssemblyGenAttributes.DisableOptimizations;

      if (ScriptDomainManager.Options.DebugMode)
      {
        ScriptDomainManager.Options.AssemblyGenAttributes |= AssemblyGenAttributes.EmitDebugInfo;
        ScriptDomainManager.Options.AssemblyGenAttributes |= AssemblyGenAttributes.GenerateDebugAssemblies;
        ScriptDomainManager.Options.AssemblyGenAttributes |= AssemblyGenAttributes.DisableOptimizations;

        ScriptDomainManager.Options.DebugCodeGeneration = true;
      }
      else
      {
        ScriptDomainManager.Options.DebugCodeGeneration = false;
      }

      ScriptDomainManager.Options.AssemblyGenAttributes |= AssemblyGenAttributes.SaveAndReloadAssemblies;

      //object[] arr = ListToVector(content as Cons);
      //0: id
      //1: name
      //2: version
      //3: imp*
      //4: vis*
      //5: inv*
      //6: subst
      //7: env
      //8: visit code
      //9: invoke code
      //10: guard code
      //11: guard req?
      //12: visible?


      Compiler.Generator.AllowTransientBinding = false;

      ScriptDomainManager.CurrentManager.Snippets.CurrentAssembly =
        Compiler.Generator.CurrentAssemblyGen = AssemblyGen.CreateModuleAssembly(filename as string);

      try
      {

        CodeBlock cb = IronSchemeLanguageContext.CompileExpr(new Cons(content));
        cb.ExplicitCodeContextExpression = null;
        cb.Name = "";

        ScriptCode sc = cc.LanguageContext.CompileSourceCode(cb, filename as string);

        sc.LibraryGlobals = Compiler.SimpleGenerator.libraryglobals;
        sc.LibraryGlobalsN = Compiler.SimpleGenerator.libraryglobalsN;
        sc.LibraryGlobalsX = Compiler.SimpleGenerator.libraryglobalsX;

        sc.SourceUnit.IsVisibleToDebugger = true;

        ScriptModule sm = ScriptDomainManager.CurrentManager.CreateModule(Path.GetFileNameWithoutExtension(filename as string), sc);

        // clean up transient non-generative types, could maybe also have used AllowTransientBinding to check?
        IronScheme.Runtime.R6RS.Records.ClearTypesFrom(Compiler.Generator.CurrentAssemblyGen);

        ScriptDomainManager.Options.AssemblyGenAttributes = aga;

        Compiler.SimpleGenerator.ClearGlobals();

        

        return TRUE;
      }
      finally
      {
        Compiler.Generator.AllowTransientBinding = true;
      }
    }

    static int evalcounter = 0;

    [Builtin("compile-core")]
    public static object CompileCore(object expr)
    {
      // fast path for really simple stuff
      if (expr is SymbolId)
      {
        CallTarget0 n = delegate
        {
          return SymbolValue(expr);
        };
        return Closure.Create(n);
      }

      AssemblyGenAttributes aga = ScriptDomainManager.Options.AssemblyGenAttributes;
      ScriptDomainManager.Options.AssemblyGenAttributes &= ~AssemblyGenAttributes.EmitDebugInfo;
      ScriptDomainManager.Options.AssemblyGenAttributes &= ~AssemblyGenAttributes.GenerateDebugAssemblies;
      ScriptDomainManager.Options.AssemblyGenAttributes &= ~AssemblyGenAttributes.DisableOptimizations;

      if (ScriptDomainManager.Options.DebugMode)
      {
        ScriptDomainManager.Options.AssemblyGenAttributes |= AssemblyGenAttributes.EmitDebugInfo;
        ScriptDomainManager.Options.AssemblyGenAttributes |= AssemblyGenAttributes.GenerateDebugAssemblies;
        ScriptDomainManager.Options.AssemblyGenAttributes |= AssemblyGenAttributes.DisableOptimizations;
        ScriptDomainManager.Options.DebugCodeGeneration = true;
      }
      else
      {
        ScriptDomainManager.Options.DebugCodeGeneration = false;
      }
      
      ScriptDomainManager.Options.AssemblyGenAttributes |= AssemblyGenAttributes.SaveAndReloadAssemblies;
      // if you ever want to inspect the emitted dll's comment the following out (or skip in the debugger), use with care
      ScriptDomainManager.Options.AssemblyGenAttributes &= ~AssemblyGenAttributes.SaveAndReloadAssemblies;

      var prevt = IronScheme.Compiler.Generator.AllowTransientBinding;
      var prevag = Compiler.Generator.CurrentAssemblyGen;

      if ((ScriptDomainManager.Options.AssemblyGenAttributes & AssemblyGenAttributes.SaveAndReloadAssemblies) != 0)
      {
        IronScheme.Compiler.Generator.AllowTransientBinding = false;

        ScriptDomainManager.CurrentManager.Snippets.CurrentAssembly =
          Compiler.Generator.CurrentAssemblyGen = AssemblyGen.CreateModuleAssembly(null);
      }
      else
      {
        Compiler.Generator.CurrentAssemblyGen = ScriptDomainManager.Options.DebugMode ? 
          ScriptDomainManager.CurrentManager.Snippets.DebugAssembly :
          ScriptDomainManager.CurrentManager.Snippets.Assembly;
      }

      int c = Interlocked.Increment(ref evalcounter); 
      
#if DEBUG
      Stopwatch sw = Stopwatch.StartNew();
#endif
      //Console.WriteLine(new Cons(expr).PrettyPrint);
      try
      {

        CodeBlock cb = IronSchemeLanguageContext.CompileExpr(new Cons(expr));
        cb.ExplicitCodeContextExpression = null;

        ScriptCode sc = Context.LanguageContext.CompileSourceCode(cb); //wrap

#if DEBUG
      sw.Stop();

      Trace.WriteLine(sw.Elapsed.TotalMilliseconds, string.Format("compile - eval-core({0:D3})", c));
      sw = Stopwatch.StartNew();
#endif
        try
        {
          sc.LibraryGlobals = Compiler.SimpleGenerator.libraryglobals;
          sc.LibraryGlobalsN = Compiler.SimpleGenerator.libraryglobalsN;
          sc.LibraryGlobalsX = Compiler.SimpleGenerator.libraryglobalsX;

          ScriptModule sm = ScriptDomainManager.CurrentManager.CreateModule(string.Format("eval-core({0:D3})", c), sc);
          sc = sm.GetScripts()[0];

#if DEBUG
      sw.Stop();

      Trace.WriteLine(sw.Elapsed.TotalMilliseconds, string.Format("compile*- eval-core({0:D3})", c));
#endif
          CallTarget0 compiled = delegate
          {
#if DEBUG
        try
        {
          sw = Stopwatch.StartNew();
#endif
            return sc.Run(Context.ModuleContext.Module);
#if DEBUG
          }

        finally
        {

          sw.Stop();
          Trace.WriteLine(sw.Elapsed.TotalMilliseconds, string.Format("run     - eval-core({0:D3})", c));
        }
#endif
          };
          return Closure.Create(compiled);
        }
        catch (Variable.UnInitializedUsageException ex)
        {
          CallTarget0 err = delegate
          {
            return AssertionViolation(ex.Variable.Block.Name, ex.Message, UnGenSym(ex.Variable.Name));
          };

          return Closure.Create(err);
        }
        finally
        {
          BoundExpression.Fixups.Clear();
          BoundExpression.FixupTypes.Clear();
          Compiler.Generator.CurrentAssemblyGen = prevag;
          ScriptDomainManager.Options.AssemblyGenAttributes = aga;
          IronScheme.Compiler.Generator.AllowTransientBinding = prevt;
          sc.ClearCache();
          Compiler.SimpleGenerator.ClearGlobals();
          Compiler.ClrGenerator.compiletimetypes.Clear();
        }
      }
      catch (Continuation)
      {
        throw;
      }
      catch (Exception ex)
      {
        var who = ex.Data["Who"];
        return SyntaxError(who ?? FALSE, ex.Message, FALSE, FALSE);
      }
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

    [Builtin("list*", UsedInternallyByCompiler = true)]
    public static object ListStar(object a, params object[] rest)
    {
      return ListStarHelper(a, Runtime.Cons.FromArray(rest));
    }

    [Builtin("list*", UsedInternallyByCompiler = true)]
    public static object ListStar(object a, object b)
    {
      return new Cons(a, b);
    }

    [Builtin("list*", UsedInternallyByCompiler = true)]
    public static object ListStar(object a, object b, object c)
    {
      return new Cons(a, new Cons(b, c));
    }

    [Builtin("list*", UsedInternallyByCompiler = true)]
    public static object ListStar(object a, object b, object c, object d)
    {
      return new Cons(a, new Cons(b, new Cons(c , d)));
    }

    [Builtin("pointer+")]
    public static object PointerAdd(object ptr, object ofs)
    {
      IntPtr p = RequiresNotNull<IntPtr>(ptr);
      int o = RequiresNotNull<int>(ofs);

      return (IntPtr)((long)p + o);
    }

    [Builtin("remove-location")]
    [UnspecifiedReturn]
    public static object RemoveLocation(object symbol)
    {
      if (ModuleScope == null)
      {
        ModuleScope = BaseHelper.cc.Scope.ModuleScope;
      }
      if (ModuleScope.ContainsName((SymbolId)symbol))
      {
        ModuleScope.RemoveName((SymbolId)symbol);
      }
      return Unspecified;
    }

    static Scope ModuleScope;

    [Builtin("symbol-value")]
    [UnspecifiedReturn]
    public static object SymbolValue(object symbol)
    {
      if (ModuleScope == null)
      {
        ModuleScope = BaseHelper.cc.Scope.ModuleScope;
      }
      object value;
      if (ModuleScope.TryLookupName((SymbolId)symbol, out value))
      {
        return value;
      }
      else
      {
        return UndefinedError(symbol);
      }
    }

    [Builtin("symbol-bound?")]
    public static object IsSymbolBound(object symbol)
    {
      if (ModuleScope == null)
      {
        ModuleScope = BaseHelper.cc.Scope.ModuleScope;
      }

      return GetBool(ModuleScope.ContainsName((SymbolId)symbol));
    }

    [DebuggerNonUserCode]
    [DebuggerStepThrough]
    [UnspecifiedReturn]
    public static void SetSymbolValueFast(object symbol, object value)
    {
      if (ModuleScope == null)
      {
        ModuleScope = BaseHelper.cc.Scope.ModuleScope;
      }
      ModuleScope.SetName((SymbolId)symbol, value);
    }

    [Builtin("string-split")]
#warning params could be dangerous to the calling convention. check me. this will probably never called with a vector argument
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
      all.Add(SymbolTable.StringToObject(e.Name));

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

      return Runtime.Cons.FromList(all);
    }

    static Cons ReadAttribute(XmlAttribute a)
    {
      return new Cons(SymbolTable.StringToObject(a.Name), a.Value);
    }

    [Builtin("open-tcp-input/output-port")]
    public static object OpenTcpInputOutputPort(object host, object port, object maybetranscoder)
    {
      TcpClient tcp = new TcpClient();
      tcp.Connect(RequiresNotNull<string>(host), RequiresNotNull<int>(port));

      NetworkStream ns = tcp.GetStream();

      if (maybetranscoder is R6RS.Transcoder)
      {
        R6RS.Transcoder tc = maybetranscoder as R6RS.Transcoder;
        return new R6RS.CustomTextReaderWriter(
          SymbolTable.StringToObject(string.Format("#<tcp-textual-input/output-port {0}:{1}>", host, port)),
          new StreamReader(ns, tc.codec), new StreamWriter(ns, tc.codec));
      }
      else
      {
        return ns;
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

    static string GetTypeName(Type t)
    {
      // check if scheme record
      if (t.Namespace.StartsWith("record"))
      {
        return t.Name;
      }
      return t.ToString();
    }

    protected static T Requires<T>(object obj)
    {
      if (obj != null && !(obj is T))
      {
        return (T) AssertionViolation(GetCaller(), "expected type: " + GetTypeName(typeof(T)), obj);
      }
      if (obj == null)
      {
        return default(T);
      }
      return (T)obj;
    }

    protected static object GetCaller()
    {
      StackTrace st = new StackTrace(1);
      MethodBase v = st.GetFrame(0).GetMethod();

      for (int i = 1; i < st.FrameCount; i++)
      {
        var nv = st.GetFrame(i).GetMethod();
        if (v.Name == nv.Name)
        {
          v = nv;
        }
        else
        {
          v = nv;
          break;
        }
      }

      foreach (BuiltinAttribute ba in v.GetCustomAttributes(typeof(BuiltinAttribute), false))
      {
        return SymbolTable.StringToObject(ba.Name ?? v.Name.ToLower());
      }
      return UnGenSymInternal(SymbolTable.StringToId(v.Name));
    }

    protected static T RequiresNotNull<T>(object obj)
    {
      if (obj is T)
      {
        return (T)obj;
      }

      return RequiresNotNullExhaustive<T>(obj);
    }

    private static T RequiresNotNullExhaustive<T>(object obj)
    {
      if (obj == null)
      {
        return (T)AssertionViolation(GetCaller(), "argument cannot be null");
      }

      if (typeof(T) == typeof(string) && obj is StringBuilder)
      {
        return (T)(object)obj.ToString();
      }

      if (typeof(T) == typeof(TextWriter) && obj is R6RS.CustomTextReaderWriter)
      {
        return (T)(object)((R6RS.CustomTextReaderWriter)obj).output;
      }

      if (typeof(T) == typeof(TextReader) && obj is R6RS.CustomTextReaderWriter)
      {
        return (T)(object)((R6RS.CustomTextReaderWriter)obj).input;
      }

      return (T)AssertionViolation(GetCaller(), "expected type: " + GetTypeName(typeof(T)), obj);
    }
  }
}
