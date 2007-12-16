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

#if R6RS
    static int evalcounter = 0;

    [Builtin("eval-core")]
    public static object EvalCore(CodeContext cc, object expr)
    {
      string exprstr = null;
      int c = ++evalcounter;
#if DEBUG

      ICallable pp = cc.Scope.LookupName(SymbolTable.StringToId("pretty-print")) as ICallable;
      
      StringWriter w = new StringWriter();
      pp.Call(expr, w);

      exprstr = w.ToString();

      if (!Directory.Exists("evaldump"))
      {
        Directory.CreateDirectory("evaldump");
      }
      
      string fn = string.Format("evaldump/{0:D3}.ss", c);

      if (File.Exists(fn))
      {
        File.Delete(fn);
      }
      File.AppendAllText(fn, exprstr);

#else
      exprstr = WriteFormat(expr);
#endif
      //same speed :(
      //if (exprstr.Length < 64)
      //{
      //  return Compiler.Generator.GetAst(expr, Compiler.Generator.evalblock).Evaluate(cc);
      //}

      try
      {
        SourceUnit su = SourceUnit.CreateSnippet(ScriptEngine, exprstr);
        AssemblyGenAttributes aga = ScriptDomainManager.Options.AssemblyGenAttributes;

        ScriptDomainManager.Options.AssemblyGenAttributes &= ~AssemblyGenAttributes.GenerateDebugAssemblies;
        ScriptDomainManager.Options.AssemblyGenAttributes &= ~AssemblyGenAttributes.EmitDebugInfo;
        ScriptDomainManager.Options.AssemblyGenAttributes &= ~AssemblyGenAttributes.SaveAndReloadAssemblies;
        Stopwatch sw = Stopwatch.StartNew();
        //ScriptModule sm = ScriptDomainManager.CurrentManager.CompileModule("eval-core", su);
        ScriptCode sc = cc.LanguageContext.CompileSourceCode(su);
        Trace.WriteLine(sw.ElapsedMilliseconds, string.Format("Compile - eval-core({0:D3})", c));
        sw = Stopwatch.StartNew();
        //object result = sm.GetScripts()[0].Run(cc.Scope, cc.ModuleContext, false); // causes issues :(
        object result = sc.Run(cc.ModuleContext.Module); // causes issues :(
        Trace.WriteLine(sw.ElapsedMilliseconds, string.Format("Run     - eval-core({0:D3})", c));
        ScriptDomainManager.Options.AssemblyGenAttributes = aga;
        return result;

      }
      finally
      {
        GC.Collect(1, GCCollectionMode.Optimized);
      }
    }
#endif


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

    [Builtin("symbol-value")]
    public static object SymbolValue(CodeContext cc, object symbol)
    {
      SymbolId s = RequiresNotNull<SymbolId>(symbol);
      return cc.Scope.ModuleScope.LookupName(s);
    }

    [Builtin("set-symbol-value!")]
    public static object SetSymbolValue(CodeContext cc, object symbol, object value)
    {
      SymbolId s = RequiresNotNull<SymbolId>(symbol);
      cc.Scope.ModuleScope.SetName(s, value);
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

    [Conditional("DEBUG")]
    static void RequiresCondition(bool condition, string message)
    {
      Debug.Assert(condition, message);
    }

    protected static object RequiresNotNull(object obj)
    {
      if (obj == null)
      {
        throw new ArgumentNullException();
      }
      return obj;
    }

    protected static T Requires<T>(object obj)
    {
      if (obj != null && !(obj is T))
      {
        throw new ArgumentTypeException("Expected type '" + typeof(T).Name + "', but got '" + obj.GetType().Name + "': " + obj);
      }
      if (obj == null)
      {
        return default(T);
      }
      return (T)obj;
    }

    protected static T RequiresNotNull<T>(object obj)
    {
      RequiresNotNull(obj);
      return Requires<T>(obj);
    }

 

  }
}
