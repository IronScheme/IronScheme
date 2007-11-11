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

namespace IronScheme.Runtime
{
  public partial class Builtins
  {

    [Builtin("with-input-from-file")]
    public static object WithInputFromFile(CodeContext cc, object filename, object thunk)
    {
      ICallableWithCodeContext f = RequiresNotNull<ICallableWithCodeContext>(thunk);
      string path = RequiresNotNull<string>(filename);

      TextReader old = Console.In;

      try
      {
        using (TextReader r = File.OpenText(path))
        {
          Console.SetIn(r);
          return f.Call(cc, new object[] { });
        }
      }
      finally
      {
        Console.SetIn(old);
      }
    }


    [Builtin("with-output-to-file")]
    public static object WithOutputToFile(CodeContext cc, object filename, object thunk)
    {
      ICallableWithCodeContext f = RequiresNotNull<ICallableWithCodeContext>(thunk);
      string path = RequiresNotNull<string>(filename);

      TextWriter old = Console.Out;

      try
      {
        using (TextWriter w = File.CreateText(path))
        {
          Console.SetOut(w);
          return f.Call(cc, new object[] { });
        }
      }
      finally
      {
        Console.SetOut(old);
      }
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
      if (cc.Scope != Context.Scope && cc.Scope.ModuleScope != Context.Scope.ModuleScope)
      {
        try
        {
          foreach (KeyValuePair<SymbolId, object> kv in Context.Scope.Items)
          {
            cc.Scope.SetName(kv.Key, kv.Value);
          }
          IronScheme.Compiler.BaseHelper.cc = cc;
        }
        catch (InvalidOperationException)
        {
          ;
        }
      }

      string path = filename as string;

      switch (Path.GetExtension(path))
      {
        case ".dll":
          Assembly ext = Assembly.LoadFile(Path.GetFullPath(path));
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
          }
          break;
        case ".exe":
          Assembly mod = Assembly.LoadFile(Path.GetFullPath(path));
          MethodInfo entry = null;
          foreach (Type t in mod.GetExportedTypes())
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
            goto case ".dll";
          }
          else
          {
            IModuleDictionaryInitialization init = Activator.CreateInstance(entry.DeclaringType) as
              IModuleDictionaryInitialization;
            try
            {
              init.InitializeModuleDictionary(cc);
            }
            catch (InvalidOperationException)
            {
            }
            entry.Invoke(null, new object[] { cc });
          }
          break;
        default:
          // check for already compiled version
          string cfn = Path.ChangeExtension(path, ".exe");
          if (File.Exists(cfn))
          {
            DateTime ct = File.GetLastWriteTime(cfn);
            if (ct > File.GetLastWriteTime(path))
            {
              if (File.GetLastWriteTime(typeof(Builtins).Assembly.Location) < ct)
              {
                path = cfn;
                goto case ".exe";
              }
            }
          }

          // this still bombs out too much
          Compiler.Generator.CanAllowTailCall = true;

          try
          {
            SourceUnit su = ScriptDomainManager.CurrentManager.Host.TryGetSourceFileUnit(cc.LanguageContext.Engine, path, Encoding.Default);
            ScriptModule sm = ScriptDomainManager.CurrentManager.CompileModule(Path.GetFileNameWithoutExtension(path), su);

            object result = sm.GetScripts()[0].Run(cc.Scope, cc.ModuleContext);
          }
          finally
          {
            Compiler.Generator.CanAllowTailCall = false;
          }
          break;
      }

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
      TextReader r = RequiresNotNull<TextReader>(port);

      string i = null;

      while ((i = r.ReadLine()) != null)
      {
        input.AppendLine(i);
        object result = IronSchemeLanguageContext.ReadExpressionString(input.ToString(), Context.ModuleContext.CompilerContext);
        if (result != null)
        {
          return result;
        }
      }

      return EOF;
    }

    readonly static object EOF = new object();

    [Builtin("char-ready?")]
    public static object IsCharReady()
    {
      return IsCharReady(CurrentInputPort());
    }
    
    [Builtin("char-ready?")]
    public static object IsCharReady(object port)
    {
      return PeekChar(port) != EOF;
    }

    [Builtin("eof-object?")]
    public static object IsEof(object obj)
    {
      return obj == EOF;
    }

    [Builtin("read-all")]
    public static object ReadAll()
    {
      return ReadAll(CurrentInputPort());
    }

    [Builtin("read-all")]
    public static object ReadAll(object port)
    {
      TextReader r = RequiresNotNull<TextReader>(port);
      string c = r.ReadToEnd();
      if (c == null)
      {
        return EOF;
      }
      return c;
    }

    [Builtin("read-line")]
    public static object ReadLine()
    {
      return ReadLine(CurrentInputPort());
    }

    [Builtin("read-line")]
    public static object ReadLine(object port)
    {
      TextReader r = RequiresNotNull<TextReader>(port);
      string c = r.ReadLine();
      if (c == null)
      {
        return EOF;
      }
      return c;
    }

    [Builtin("read-char")]
    public static object ReadChar()
    {
      return ReadChar(CurrentInputPort());
    }

    [Builtin("read-char")]
    public static object ReadChar(object port)
    {
      TextReader r = RequiresNotNull<TextReader>(port);
      int c = r.Read();
      if (c == -1)
      {
        return EOF;
      }
      return (char)c;
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
      int c = r.Peek();
      if (c == -1)
      {
        return EOF;
      }
      return (char)c;
    }

    [Builtin("write-char")]
    public static object WriteChar(object ch)
    {
      return WriteChar(ch, CurrentOutputPort());
    }

    [Builtin("write-char")]
    public static object WriteChar(object ch, object port)
    {
      TextWriter w = RequiresNotNull<TextWriter>(port);
      char c = RequiresNotNull<char>(ch);
      w.Write(c);
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

    static readonly SymbolId quote = SymbolTable.StringToId("quote");
    static readonly SymbolId unquote_splicing = SymbolTable.StringToId("unquote-splicing");
    static readonly SymbolId quasiquote = SymbolTable.StringToId("quasiquote");
    static readonly SymbolId unquote = SymbolTable.StringToId("unquote");

    public static string DisplayFormat(object obj)
    {
      if (obj == null)
      {
        return "()";
      }
      if (obj == EOF)
      {
        return "<eof>";
      }
      if (obj == Builtins.Unspecified)
      {
        return "<unspecified>";
      }
      if (obj is BuiltinMethod)
      {
        return "builtin::" + ((BuiltinMethod)obj).Name;
      }
      if (obj is Closure)
      {
        return "closure::" + ((Closure)obj).Name;
      }
      if (obj is Macro)
      {
        return "macro::" + ((Macro)obj).Name;
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
#if NEW_NUMBERS
              if (t == typeof(Complex64) || t == typeof(Fraction))
              {
                return "Number";
              }
#endif
              if (typeof(ICallableWithCodeContext).IsAssignableFrom(t))
              {
                return "Procedure";
              }
              if (typeof(Macro).IsAssignableFrom(t) || typeof(Compiler.IGenerator).IsAssignableFrom(t))
              {
                return "Macro";
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

        if (s != null)
        {
          object scar = s.Car;
          if ((bool)IsSymbol(scar) && s.Cdr != null)
          {
            if ((bool)IsEqual(quote, scar))
            {
              return "'" + DisplayFormat(Cadr(s));
            }
            if ((bool)IsEqual(quasiquote, scar))
            {
              return "`" + DisplayFormat(Cadr(s));
            }
            if ((bool)IsEqual(unquote, scar))
            {
              return "," + DisplayFormat(Cadr(s));
            }
            if ((bool)IsEqual(unquote_splicing, scar))
            {
              return ",@" + DisplayFormat(Cadr(s));
            }
          }
        }

        while (s != null)
        {
          v.Add(DisplayFormat(s.Car));
          if (s.Cdr != null && !(s.Cdr is Cons))
          {
            v.Add(".");
            v.Add(DisplayFormat(s.Cdr));
            break;
          }
          s = s.Cdr as Cons;
        }
        return string.Format("({0})", string.Join(" ", v.ToArray()));
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

    public static string WriteFormat(object obj)
    {
      if (obj == null)
      {
        return "()";
      }

      if (obj is BuiltinMethod)
      {
        return ((BuiltinMethod)obj).Name;
      }
      if (obj is Closure)
      {
        return ((Closure)obj).Name;
      }
      if (obj is Macro)
      {
        return ((Macro)obj).Name;
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

        object scar = s.Car;
        if ((bool)IsSymbol(scar) && s.Cdr != null)
        {
          if ((bool)IsEqual(quote, scar))
          {
            return "'" + WriteFormat(Cadr(s));
          }
          if ((bool)IsEqual(quasiquote, scar))
          {
            return "`" + WriteFormat(Cadr(s));
          }
          if ((bool)IsEqual(unquote, scar))
          {
            return "," + WriteFormat(Cadr(s));
          }
          if ((bool)IsEqual(unquote_splicing, scar))
          {
            return ",@" + WriteFormat(Cadr(s));
          }
        }

        while (s != null)
        {
          v.Add(WriteFormat(s.Car));
          if (s.Cdr != null && !(s.Cdr is Cons))
          {
            v.Add(".");
            v.Add(WriteFormat(s.Cdr));
            break;
          }
          s = s.Cdr as Cons;
        }
        return string.Format("({0})", string.Join(" ", v.ToArray()));
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
        return SymbolTable.IdToString((SymbolId)obj);
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
    public static object IsInputPort(object obj)
    {
      return obj is TextReader; 
    }

    [Builtin("output-port?")]
    public static object IsOutputPort(object obj)
    {
      return obj is TextWriter;
    }

    [Builtin("call-with-input-file")]
    public static object CallWithInputFile(CodeContext cc, object filename, object fc1)
    {
      ICallableWithCodeContext f = RequiresNotNull<ICallableWithCodeContext>(fc1);
      string path = RequiresNotNull<string>(filename);

      using (TextReader r = File.OpenText(path))
      {
        return f.Call(cc, new object[] { r });
      }
    }

    [Builtin("call-with-output-file")]
    public static object CallWithOutputFile(CodeContext cc, object filename, object fc1)
    {
      ICallableWithCodeContext f = RequiresNotNull<ICallableWithCodeContext>(fc1);
      string path = RequiresNotNull<string>(filename);

      using (TextWriter w = File.CreateText(path))
      {
        return f.Call(cc, new object[] { w });
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
