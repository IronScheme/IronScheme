#region License
/* Copyright (c) 2007,2008,2009,2010,2011 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Net.Sockets;
using System.Reflection;
using System.Reflection.Emit;
using System.Text;
using System.Xml;
using IronScheme.Compiler;
using Microsoft.Scripting;
using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Generation;

namespace IronScheme.Runtime
{
  [AttributeUsage(AttributeTargets.Method, AllowMultiple=true)]
  public sealed class BuiltinAttribute : Attribute
  {
    string name;
    bool allowcps = true;
    bool allowconstantfold = false;

    public bool AllowConstantFold
    {
      get { return allowconstantfold; }
      set { allowconstantfold = value; }
    }

    public bool AllowCPS
    {
      get { return allowcps; }
      set { allowcps = value; }
    }

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

    [Builtin("clr-type?")]
    public static object IsClrType(object o)
    {
      return GetBool(o is Type);
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
    

    [Builtin]
    public static object Typeof(object o)
    {
      if (o == null)
      {
        return typeof(object);
      }
      return o.GetType();
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
        return Path.GetDirectoryName(typeof(Builtins).Assembly.CodeBase).Replace("file:\\", "").Replace("file:", "");
      }
    }

    [Builtin("make-guid")]
    public static object MakeGuid()
    {
      return Guid.NewGuid();
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


    internal readonly static List<string> includepaths = new List<string>();


    [Builtin("get-library-paths")]
    public static object GetLibraryPaths()
    {
      List<string> paths = new List<string>();

      if (Environment.CurrentDirectory != ApplicationDirectory)
      {
        paths.Add(".");
      }

      paths.AddRange(includepaths);
      paths.Add(ApplicationDirectory);
      paths.Add(Path.Combine(ApplicationDirectory, "lib"));

      return Runtime.Cons.FromList(paths);
    }

    [Builtin("procedure-arity")]
    public static object ProcArity(object proc)
    {
      Callable c = RequiresNotNull<Callable>(proc);
      return c.Arity;
    }

    [Builtin("procedure-form")]
    public static object ProcForm(object proc)
    {
      Callable c = RequiresNotNull<Callable>(proc);
      return c.Form;
    }

    [Builtin("procedure-name")]
    public static object ProcName(object proc)
    {
      Callable c = RequiresNotNull<Callable>(proc);
      return SymbolTable.StringToObject(c.ToString());
    }

    [Builtin("procedure-environment")]
    public static object ProcEnv(object proc)
    {
      Callable c = RequiresNotNull<Callable>(proc);
      return FALSE;
    }

    [Builtin("make-traced-procedure")]
    [CLSCompliant(false)]
    public static object MakeTraceProcedure(object name, object proc)
    {
      return MakeTraceProcedure(name, proc, FALSE);
    }

    [Builtin("make-traced-procedure")]
    [CLSCompliant(false)]
    public static object MakeTraceProcedure(object name, object proc, object filter)
    {
      Callable p = RequiresNotNull<Callable>(proc);
      SymbolId n = RequiresNotNull<SymbolId>(name);
      Callable f = filter as Callable;
      return new TraceClosure(p, n, f);
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
      
      Stopwatch sw = Stopwatch.StartNew();
      try
      {
#if CPS
        return OptimizedBuiltins.Call(c);
#else
        return c.Call();
#endif
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
        var o = Load(filename, true);
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

#if CPS
    [Builtin("compile-core")]
    public static object CompileCore(object expr, object sk)
#else
    [Builtin("compile-core")]
    public static object CompileCore(object expr)
#endif
    {
#if !CPS
      // fast path for really simple stuff
      if (expr is SymbolId)
      {
        CallTarget0 n = delegate
        {
          return SymbolValue(expr);
        };
        return Closure.Create(n);
      }
#endif
#if CPS
      // this would look a ton sweeter on C# 4.0 :)
      Callable cps = SymbolValue(SymbolTable.StringToObject("convert->cps")) as Callable;
      expr = cps.Call(Closure.IdentityForCPS, expr, sk);
#endif

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

      // if you ever want to inspect the emitted dll's comment this out, use with care
      ScriptDomainManager.Options.AssemblyGenAttributes |= AssemblyGenAttributes.SaveAndReloadAssemblies;
      ScriptDomainManager.Options.AssemblyGenAttributes &= ~AssemblyGenAttributes.SaveAndReloadAssemblies;

      var prevt = IronScheme.Compiler.Generator.AllowTransientBinding;

      if ((ScriptDomainManager.Options.AssemblyGenAttributes & AssemblyGenAttributes.SaveAndReloadAssemblies) != 0)
      {
        IronScheme.Compiler.Generator.AllowTransientBinding = false;
      }

      int c = ++evalcounter;
      
#if DEBUG
      Stopwatch sw = Stopwatch.StartNew();
#endif
      //Console.WriteLine(new Cons(expr).PrettyPrint);

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
      }
      catch (Variable.UnInitializedUsageException ex)
      {
        ScriptDomainManager.Options.AssemblyGenAttributes = aga;
        CallTarget0 err = delegate
        {
          return AssertionViolation(ex.Variable.Block.Name, ex.Message, UnGenSym(ex.Variable.Name));
        };

        return Closure.Create(err);
      }
      finally
      {
        IronScheme.Compiler.Generator.AllowTransientBinding = prevt;
        sc.ClearCache();
        Compiler.SimpleGenerator.ClearGlobals();
      }
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
#if CPS
        catch (Exception ex)
        {
          Callable raise = SymbolValue(SymbolTable.StringToObject("raise")) as Callable;
          Callable k = SymbolValue(sk) as Callable;
          return OptimizedBuiltins.CallWithK(raise, k, ex);
        }
#endif
        finally
        {

          sw.Stop();
          Trace.WriteLine(sw.Elapsed.TotalMilliseconds, string.Format("run     - eval-core({0:D3})", c));
        }
#endif
      };

      ScriptDomainManager.Options.AssemblyGenAttributes = aga;
      Compiler.SimpleGenerator.ClearGlobals();

      return Closure.Create(compiled);
    }

#if CPS
    [Builtin("eval-core", AllowCPS=false)]
    public static object EvalCore(object k, object expr)
    {
      object sk = GenSym(SymbolTable.StringToObject("eval-core-k"));
      SetSymbolValue(sk, k);
      ICallable compiled = CompileCore(expr, sk) as ICallable;
      return compiled.Call();
    }
#else

    [Builtin("eval-core")]
    public static object EvalCore(object expr)
    {
      Callable compiled = CompileCore(expr) as Callable;
      return compiled.Call();
    }
#endif


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

    [Builtin("list*", AllowCPS=false)]
    public static object ListStar(object a, params object[] rest)
    {
      return ListStarHelper(a, Runtime.Cons.FromArray(rest));
    }

    [Builtin("list*", AllowCPS = false)]
    public static object ListStar(object a, object b)
    {
      return new Cons(a, b);
    }

    [Builtin("list*", AllowCPS = false)]
    public static object ListStar(object a, object b, object c)
    {
      return new Cons(a, new Cons(b, c));
    }

    [Builtin("list*", AllowCPS = false)]
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
    public static void SetSymbolValueFast(object symbol, object value)
    {
      if (ModuleScope == null)
      {
        ModuleScope = BaseHelper.cc.Scope.ModuleScope;
      }
      ModuleScope.SetName((SymbolId)symbol, value);
    }


    [Builtin("set-symbol-value!")]
    public static object SetSymbolValue(object symbol, object value)
    {
      SetSymbolValueFast(symbol, value);
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
      if (obj == null)
      {
        return (T) AssertionViolation(GetCaller(), "argument cannot be null");
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

      if (obj != null && !(obj is T))
      {
        return (T) AssertionViolation(GetCaller(), "expected type: " + typeof(T).Name,  obj);
      }

      return (T)obj;
    }
  }
}
