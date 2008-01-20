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
      set {name = value;}
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

    internal static Exception lastException = null;

    protected static Exception LastException
    {
      get { return lastException; }
    }

    public static bool IsTrue(object arg)
    {
      if (arg is bool)
      {
        return (bool)arg;
      }

      return true;
    }

    sealed class UnspecifiedObject { }

    public static readonly object Unspecified = new UnspecifiedObject();

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

    [Builtin(":optional")]
    public static object Optional(object obj, object def)
    {
      return obj ?? def;
    }

    [Builtin("defined?")]
    public static object IsDefined(CodeContext cc, object sym)
    {
      SymbolId s = RequiresNotNull<SymbolId>(sym);
      return cc.Scope.ContainsName(s);
    }



    [Builtin("time-it")]
    public static object TimeIt(object who, object thunk)
    {
      ICallable c = RequiresNotNull<ICallable>(thunk);

      int[] colcount = new int[3];
      for (int i = 0; i < 3; i++)
      {
        colcount[i] = GC.CollectionCount(i);
      }
      Stopwatch sw = Stopwatch.StartNew();
      try
      {
        return c.Call();
      }
      finally
      {
        sw.Stop();
        int[] colcountafter = new int[3];
        for (int i = 0; i < 3; i++)
        {
          colcountafter[i] = GC.CollectionCount(i);
        }

        Console.WriteLine(@"Statistics for '{0}':
  Time:          {1}ms
  Gen0 collect:  {2}
  Gen1 collect:  {3}
  Gen2 collect:  {4}", who, sw.ElapsedMilliseconds, 
                     colcountafter[0] - colcount[0],
                     colcountafter[1] - colcount[1],
                     colcountafter[2] - colcount[2]);
      }
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

#if DEBUG

      System.Threading.ThreadPool.QueueUserWorkItem(delegate(object state)
      {
        ICallable prettyprint = SymbolValue(cc, SymbolTable.StringToId("pretty-print")) as ICallable;

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
          prettyprint.Call(expr, w);
        }
      });

#endif

      Stopwatch sw = Stopwatch.StartNew();

      ScriptCode sc = cc.LanguageContext.CompileSourceCode(IronSchemeLanguageContext.Compile(new Cons(expr))); //wrap

      Trace.WriteLine(sw.Elapsed.TotalMilliseconds, string.Format("compile - eval-core({0:D3})", c));
      sw = Stopwatch.StartNew();

      sc.EnsureCompiled();
      Trace.WriteLine(sw.Elapsed.TotalMilliseconds, string.Format("compile*- eval-core({0:D3})", c));
      sw = Stopwatch.StartNew();

      // this compiles the file, i think
      //ScriptModule sm = ScriptDomainManager.CurrentManager.CreateModule(string.Format("eval-core({0:D3})", c), sc);

      object cbr = sc.Run(cc.ModuleContext.Module); // try eval causes issues :(
      Trace.WriteLine(sw.Elapsed.TotalMilliseconds, string.Format("run     - eval-core({0:D3})", c));
      ScriptDomainManager.Options.AssemblyGenAttributes = aga;
      return cbr;

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
      return ListStarHelper(a, Runtime.Cons.FromArray(rest));
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
        ModuleScope = cc.Scope.ModuleScope;
      }
      return ModuleScope.LookupName(s);
    }

    [Builtin("set-symbol-value!")]
    public static object SetSymbolValue(CodeContext cc, object symbol, object value)
    {
      SymbolId s = RequiresNotNull<SymbolId>(symbol);
      if (ModuleScope == null)
      {
        ModuleScope = cc.Scope.ModuleScope;
      }
      ModuleScope.SetName(s, value);
      return Unspecified;
    }

    
    [Builtin("macro-expand1")]
    public static object MacroExpand1(CodeContext cc, object args)
    {
      object result = SyntaxExpander.Expand(args);
      return result;
    }

    #region console


    [Builtin]
    public static object prl(object obj1)
    {
      return prl(new object[] { obj1 });
    }

    [Builtin]
    public static object prl(object obj1, object obj2)
    {
      return prl(new object[] { obj1, obj2 });
    }

    [Builtin]
    public static object prl(object obj1, object obj2, object obj3)
    {
      return prl(new object[] { obj1, obj2, obj3 });
    }

    [Builtin]
    public static object prl(params object[] args)
    {
      Debug.Assert(args != null);
      object o = null;
      foreach (object arg in args)
      {
        string s = DisplayFormat(arg);
        Console.WriteLine(s);
        o = arg;
      }
      return o;
    }

    [Builtin]
    public static object cwl(object str)
    {
      Console.WriteLine(str);
      return str as string;
    }

    [Builtin]
    public static object cwl(object format, object arg1)
    {
      string r = string.Format(format as string, arg1);
      Console.WriteLine(r);
      return r;
    }

    [Builtin]
    public static object cwl(object format, object arg1, object arg2)
    {
      string r = string.Format(format as string, arg1, arg2);
      Console.WriteLine(r);
      return r;
    }

    [Builtin]
    public static object cwl(object format, object arg1, object arg2, object arg3)
    {
      string r = string.Format(format as string, arg1, arg2, arg3);
      Console.WriteLine(r);
      return r;
    }


    [Builtin]
    public static object cwl(object format, params object[] args)
    {
      string r = string.Format(format as string, args);
      Console.WriteLine(r);
      return r;
    }

    #endregion

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
