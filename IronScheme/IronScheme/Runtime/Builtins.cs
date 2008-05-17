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

    public static readonly object Unspecified = new UnspecifiedObject();


    [Builtin("unspecified?")]
    public static object IsUnspecified(object o)
    {
      return GetBool(o == Unspecified);
    }

    [Builtin("command-line")]
    public static object CommandLine()
    {
      return List(Environment.GetCommandLineArgs());
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
        return Path.GetDirectoryName(typeof(Builtins).Assembly.CodeBase).Replace("file:\\", "");
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
    public static object GetLibraryPaths()
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

    [Builtin("make-traced-procedure")]
    public static object MakeTraceProcedure(object name, object proc)
    {
      return MakeTraceProcedure(name, proc, FALSE);
    }

    [Builtin("make-traced-procedure")]
    public static object MakeTraceProcedure(object name, object proc, object filter)
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

      TimeSpan privts = selfprocess.PrivilegedProcessorTime,
        totalts = selfprocess.TotalProcessorTime,
        userts = selfprocess.UserProcessorTime;
      
      Stopwatch sw = Stopwatch.StartNew();
      try
      {
        return c.Call();
      }
      finally
      {
        TimeSpan privts2 = selfprocess.PrivilegedProcessorTime,
          totalts2 = selfprocess.TotalProcessorTime,
          userts2 = selfprocess.UserProcessorTime;

        int colcountafter = 0;
        for (int i = 0; i < 3; i++)
        {
          colcountafter += GC.CollectionCount(i);
        }

        Console.WriteLine(@"Statistics for '{0}':
  Priv Time:     {1:f0}ms
  User Time:     {2:f0}ms
  Total Time:    {3:f0}ms
  Gen collect:   {4}", who, 
                     (privts2 - privts).TotalMilliseconds, 
                     (userts2 - userts).TotalMilliseconds,
                     (totalts2 - totalts).TotalMilliseconds,
                     colcountafter - colcount);
      }
    }
   

    static bool IsSimple(Cons c)
    {
      if (IsTrue(IsAllSameSymbol(c.car, SymbolTable.StringToId("set!"))))
      {
        return false;
      }
      if (IsTrue(IsAllSameSymbol(c.car, SymbolTable.StringToId("begin"))))
      {
        return false;
      }


      if (IsTrue(IsAllSameSymbol(c.car, quote)))
      {
        return true;
      }
      foreach (object e in c)
      {
        
        if (e is Cons)
        {
          Cons ce = e as Cons;
          if (!IsTrue(IsAllSameSymbol(ce.car, quote)))
          {
            return false;
          }
        }
      }
      return true;
    }


    static int evalcounter = 0;

    [Builtin("eval-core")]
    public static object EvalCore(CodeContext cc, object expr)
    {
      AssemblyGenAttributes aga = ScriptDomainManager.Options.AssemblyGenAttributes;

      ScriptDomainManager.Options.AssemblyGenAttributes &= ~AssemblyGenAttributes.GenerateDebugAssemblies;
      ScriptDomainManager.Options.AssemblyGenAttributes &= ~AssemblyGenAttributes.EmitDebugInfo;
      ScriptDomainManager.Options.AssemblyGenAttributes &= ~AssemblyGenAttributes.DisableOptimizations;
      ScriptDomainManager.Options.AssemblyGenAttributes &= ~AssemblyGenAttributes.SaveAndReloadAssemblies;

      int c = ++evalcounter;

#if DEBUG_EXTRA

      // bad for ASP.NET
      if (Assembly.GetEntryAssembly() != null)
      {
        System.Threading.ThreadPool.QueueUserWorkItem(delegate(object state)
        {
          ICallable prettyprint = null;
          SymbolId pp = SymbolTable.StringToId("pretty-print");
          if (cc.Scope.ContainsName(pp))
          {
            prettyprint = SymbolValue(cc, pp) as ICallable;
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
      if (expr is Cons)
      {

        if (IsSimple(expr as Cons))
        {
#if DEBUG
          Stopwatch esw = Stopwatch.StartNew();
          try
          {
#endif
            Cons e = expr as Cons;

            if (IsTrue(IsAllSameSymbol(e.car, quote)))
            {
              return Car(e.cdr);
            }

            List<object> args = new List<object>();

            ICallable proc = SymbolValue(cc, e.car) as ICallable;

            e = e.cdr as Cons;
            while (e != null)
            {
              object arg = e.car;
              if (arg is Cons)
              {
                Cons cargs = arg as Cons;
                args.Add(Car(cargs.cdr));
              }
              else
              {
                args.Add(SymbolValue(cc, arg));
              }
              e = e.cdr as Cons;
            }

            return proc.Call(args.ToArray());
#if DEBUG
          }
          finally
          {
            Trace.WriteLine(esw.Elapsed.TotalMilliseconds, string.Format("eval    - eval-core({0:D3})", c));
          }
#endif
        }
      }
      else if (expr is SymbolId)
      {
        object o;
        // this could fail if the name is mangled
        if (cc.Scope.TryGetName((SymbolId)expr, out o))
        {
          return o;
        }
      }
      else
      {
        return expr;
      }


#if DEBUG
      Stopwatch sw = Stopwatch.StartNew();
#endif
      ScriptCode sc = cc.LanguageContext.CompileSourceCode(IronSchemeLanguageContext.CompileExpr(new Cons(expr))); //wrap
      
#if DEBUG
      Trace.WriteLine(sw.Elapsed.TotalMilliseconds, string.Format("compile - eval-core({0:D3})", c));
      sw = Stopwatch.StartNew();
#endif
      try
      {
        sc.EnsureCompiled();
      }
      catch (Variable.UnInitializedUsageException ex)
      {
        ScriptDomainManager.Options.AssemblyGenAttributes = aga;
        return AssertionViolation(ex.Variable.Block.Name, ex.Message, UnGenSym(ex.Variable.Name));
      }
#if DEBUG
      Trace.WriteLine(sw.Elapsed.TotalMilliseconds, string.Format("compile*- eval-core({0:D3})", c));
      sw = Stopwatch.StartNew();
#endif

      try
      {
        return sc.Run(cc.ModuleContext.Module); // try eval causes issues :(
      }
      finally
      {

#if DEBUG
        Trace.WriteLine(sw.Elapsed.TotalMilliseconds, string.Format("run     - eval-core({0:D3})", c));
#endif
        ScriptDomainManager.Options.AssemblyGenAttributes = aga;
      }
    }


    [Builtin("gc-collect")]
    public static object GcCollect()
    {
      GC.Collect();
      return Unspecified;
    }

#if BOOTSTRAP
    [Builtin("make-eq-hashtable")]
    public static object MakeEqHashtable()
    {
      return new Hashtable();
    }


    [Builtin("hashtable-ref")]
    public static object HashtableRef(object hashtable, object key, object value)
    {
      Hashtable h = RequiresNotNull<Hashtable>(hashtable);
      return h[key] ?? value;
    }

    [Builtin("hashtable-set!")]
    public static object HashtableSet(object hashtable, object key, object value)
    {
      Hashtable h = RequiresNotNull<Hashtable>(hashtable);
      h[key] = value;
      return Unspecified;
    }

    [Builtin("all-empty?")]
    public static object IsAllEmpty(object ls)
    {
      return ls == null || 
        (Car(ls) == null && 
        (bool)IsAllEmpty(Cdr(ls)));
    }

    [Builtin("file-exists?")]
    public static object FileExists(object filename)
    {
      string s = RequiresNotNull<string>(filename);
      return File.Exists(s);
    }

    [Builtin("delete-file")]
    public static object DeleteFile(object filename)
    {
      string s = RequiresNotNull<string>(filename);
      File.Delete(s);
      return Unspecified;
    }

    [Builtin("cons*")]
    public static object ConsStar(object a)
    {
      return a;
    }

#endif


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
    public static object ListStar(object a, object b)
    {
      return new Cons(a, b);
    }

    [Builtin("list*")]
    public static object ListStar(object a, object b, object c)
    {
      return new Cons(a, new Cons(b, c));
    }

    [Builtin("list*")]
    public static object ListStar(object a, object b, object c, object d)
    {
      return new Cons(a, new Cons(b, new Cons(c , d)));
    }

    static Scope ModuleScope;

    [Builtin("symbol-value")]
    public static object SymbolValue(CodeContext cc, object symbol)
    {
      SymbolId s = RequiresNotNull<SymbolId>(symbol);
      if (ModuleScope == null)
      {
        ModuleScope = BaseHelper.cc.Scope.ModuleScope;
      }
      return ModuleScope.LookupName(s);
    }

    [Builtin("set-symbol-value!")]
    public static object SetSymbolValue(CodeContext cc, object symbol, object value)
    {
      SymbolId s = RequiresNotNull<SymbolId>(symbol);
      if (ModuleScope == null)
      {
        ModuleScope = BaseHelper.cc.Scope.ModuleScope;
      }
      ModuleScope.SetName(s, value);
      return Unspecified;
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
        AssertionViolation(GetCaller(), "argument cannot be null");
      }
      return obj;
    }

    protected static T Requires<T>(object obj)
    {
      if (obj != null && !(obj is T))
      {
        AssertionViolation(GetCaller(), "expected type: " + typeof(T).Name, obj.GetType().Name, obj);
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
      return SymbolId.Invalid;
    }

    protected static T RequiresNotNull<T>(object obj)
    {
      if (obj == null)
      {
        AssertionViolation(GetCaller(), "argument cannot be null");
      }

      if (obj != null && !(obj is T))
      {
        AssertionViolation(GetCaller(), "expected type: " + typeof(T).Name, obj.GetType().Name, obj);
      }

      return (T)obj;
    }

 

 

  }
}
