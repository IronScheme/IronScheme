#region License
/* Copyright (c) 2007-2015 Llewellyn Pritchard
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Runtime.Serialization.Formatters.Binary;
using Microsoft.Scripting;
using BigInteger = Oyster.Math.IntX;
using System.IO.Compression;

namespace IronScheme.Runtime
{
  public static class Helpers
  {
    public static object LongToBignum(long l)
    {
      return Builtins.ToIntegerIfPossible(new BigInteger(l));
    }

    public static Type GetTypeFast(string name)
    {
      return Compiler.ClrGenerator.GetTypeFast(name);
    }

    public static object UnwrapValue(object o)
    {
      var mv = o as MultipleValues;
      if (mv != null)
      {
        return mv.ToArray(1);
      }
      return o;
    }

    public static MultipleValues WrapValue(object o)
    {
      var mv = o as MultipleValues;
      if (mv != null)
      {
        return mv;
      }
      return new MultipleValues(o);
    }

    public static T FFIConvertTo<T>(object obj)
    {
      var objtype = obj == null ? typeof(IntPtr) : obj.GetType();

      if (objtype == typeof(T))
      {
        if (obj == null)
        {
          return (T)(object)IntPtr.Zero;
        }
        return (T)obj;
      }

      var tc = Type.GetTypeCode(typeof(T));

      switch (tc)
      {
        case TypeCode.Int32:
          return (T)(object)Convert.ToInt32(obj);
        case TypeCode.UInt32:
          return (T)(object)Convert.ToUInt32(obj);
        case TypeCode.Int64:
          return (T)(object)Convert.ToInt64(obj);
        case TypeCode.UInt64:
          return (T)(object)Convert.ToUInt64(obj);
        case TypeCode.Int16:
          return (T)(object)Convert.ToInt16(obj);
        case TypeCode.UInt16:
          return (T)(object)Convert.ToUInt16(obj);
        case TypeCode.Byte:
          return (T)(object)Convert.ToByte(obj);
        case TypeCode.SByte:
          return (T)(object)Convert.ToSByte(obj);
        case TypeCode.Single:
          return (T)(object)Convert.ToSingle(obj);
        case TypeCode.Double:
          return (T)(object)Convert.ToDouble(obj);
        default:
          if (typeof(T) == typeof(IntPtr))
          {
            return (T)(object) new IntPtr(Convert.ToInt32(obj));
          }
          return (T)Builtins.AssertionViolation("FFIConvertTo", "not a known ffi type", obj, objtype);
      }
    }

    public static T UnsafeConvert<T>(object o)
    {
      if (o is T)
      {
        return (T)o;
      }
      else
      {
        return default(T);
      }
    }

    static BigInteger GetValue(object v)
    {
      if (v is uint)
      {
        return (uint)v;
      }
      if (v is long)
      {
        return (long)v;
      }
      else
      {
        return (ulong)v;
      }
    }

    public static object FFIConvertFrom<T>(T value)
    {
      var tc = Type.GetTypeCode(typeof(T));

      switch (tc)
      {
        case TypeCode.Int32:
        case TypeCode.Double:
          return value;
        case TypeCode.UInt32:
        case TypeCode.Int64:
        case TypeCode.UInt64:
          return Builtins.ToIntegerIfPossible(GetValue(value));
        case TypeCode.Int16:
        case TypeCode.UInt16:
        case TypeCode.Byte:
        case TypeCode.SByte:
          return Convert.ToInt32(value);
        case TypeCode.Single:
          return Convert.ToDouble(value);
        case TypeCode.String:
          return Convert.ToString(value);

        default:
          if (typeof(T) == typeof(IntPtr) ||
            typeof(T) == typeof(UIntPtr))
          {
            return value;
          }
          return Builtins.AssertionViolation("FFIConvertTo", "not a known ffi type", value, typeof(T));
      }
    }

    public static T FFIDelegate<T>(IntPtr ptr) where T : class
    {
      if (ptr == IntPtr.Zero)
      {
        Builtins.AssertionViolation("FFIDelegate", "pointer cannot be null", ptr);
      }
      var del = Marshal.GetDelegateForFunctionPointer(ptr, typeof(T));
      return (T)(object)del;
    }

    public static IntPtr FFIFunctionPointer(Type sig, Delegate del, CodeContext context)
    {
      del = Delegate.CreateDelegate(sig, context, del.Method);
      return Marshal.GetFunctionPointerForDelegate(del);
    }

    public static object EnumToSymbol<T>(T value)
    {
      return SymbolTable.StringToObject(value.ToString());
    }

    internal static object SymbolToEnum(Type type, object symbol)
    {
      if (symbol is SymbolId)
      {
        string name = SymbolTable.IdToString(RequiresNotNull<SymbolId>(symbol));
        try
        {
          return Enum.Parse(type, name, true);
        }
        catch (Exception ex)
        {
          return Builtins.AssertionViolation("symbol-to-enum", ex.Message, symbol, type);
        }
      }
      else if (symbol is Cons)
      {
        int v = 0;
        foreach (object n in symbol as Cons)
        {
          v |= (int)SymbolToEnum(type, n);
        }

        return v;
      }
      else
      {
        return Builtins.AssertionViolation("symbol-to-enum", "Not a valid type", symbol, type);
      }
    }

    public static T SymbolToEnum<T>(object symbol)
    {
      return (T) SymbolToEnum(typeof (T), symbol);
    }

    static object ConvertFromSchemeObject<T>(object o)
    {
      try
      {
        switch (Type.GetTypeCode(o.GetType()))
        {
          case TypeCode.Boolean:
          case TypeCode.Char:
          case TypeCode.DateTime:
          case TypeCode.DBNull:
          case TypeCode.Object:
            return (T)o;
          default:
            switch (Type.GetTypeCode(typeof(T)))
            {
              case TypeCode.Boolean:
                return Builtins.IsTrue(o);
              case TypeCode.Byte:
                return Convert.ToByte(o);
              case TypeCode.Char:
                return Convert.ToChar(o);
              case TypeCode.DateTime:
                return Convert.ToDateTime(o);
              case TypeCode.Decimal:
                return Convert.ToDecimal(o);
              case TypeCode.Double:
                return Convert.ToDouble(o);
              case TypeCode.Int16:
                return Convert.ToInt16(o);
              case TypeCode.Int32:
                return Convert.ToInt32(o);
              case TypeCode.Int64:
                return Convert.ToInt64(o);
              case TypeCode.SByte:
                return Convert.ToSByte(o);
              case TypeCode.Single:
                return Convert.ToSingle(o);
              case TypeCode.String:
                return Convert.ToString(o);
              case TypeCode.UInt16:
                return Convert.ToUInt16(o);
              case TypeCode.UInt32:
                return Convert.ToUInt32(o);
              case TypeCode.UInt64:
                return Convert.ToUInt64(o);
              default:
                return (T)o;
            }
        }
      }
      catch
      {
        // nothing i can do now :\
        return null;
      }
    }

    public static T[] RequiresArray<T>(object obj)
    {
      if (obj is byte[] && typeof(T) == typeof(byte))
      {
        return obj as T[];
      }

      if (obj is T[])
      {
        return obj as T[];
      }


      object[] arr = obj as object[];

      if (arr == null)
      {
        Builtins.AssertionViolation(GetCaller(), "expected type: " + typeof(T).Name, obj);
      }

      if (typeof(T) == typeof(object))
      {
        return (T[])obj;
      }

      T[] ret = new T[arr.Length];

      for (int i = 0; i < arr.Length; i++)
      {
        ret[i] = Requires<T>(arr[i]);
      }

      return ret;
    }

    public static T Requires<T>(object obj)
    {
      if (obj != null && !(obj is T))
      {
        // no way pass a box op :(
        object o = ConvertFromSchemeObject<T>(obj);
        if (o != null)
        {
          return (T)o;
        }
        Builtins.AssertionViolation(GetCaller(), "expected type: " + typeof(T).Name, obj);
      }
      if (obj == null && typeof(T).IsValueType)
      {
        Builtins.AssertionViolation(GetCaller(), "value type cannot be null", typeof(T).Name);
      }
      return (T)obj;
    }

    static object GetCaller()
    {
      StackTrace st = new StackTrace(2);
      MethodBase m = st.GetFrame(0).GetMethod();
      foreach (BuiltinAttribute ba in m.GetCustomAttributes(typeof(BuiltinAttribute), false))
      {
        return SymbolTable.StringToObject(ba.Name ?? m.Name.ToLower());
      }
      return SymbolTable.StringToObject(m.Name.ToLower());
    }

    [DebuggerNonUserCode]
    [DebuggerStepThrough]
    public static T RequiresNotNull<T>(object obj)
    {
      if (obj == null)
      {
        return (T) Builtins.AssertionViolation(GetCaller(), "argument cannot be null");
      }

      if (!(obj is T))
      {
        // no way pass a box op :(
        object o = ConvertFromSchemeObject<T>(obj);
        if (o != null)
        {
          return (T)o;
        }
        if (typeof(T) == typeof(Callable))
        {
          return (T)Builtins.AssertionViolation(GetCaller(), "expected procedure", obj);
        }
        else
        {
          return (T)Builtins.AssertionViolation(GetCaller(), "expected type: " + typeof(T).Name, obj);
        }
      }

      return (T)obj;
    }

    public static object[] DeserializeAssemblyConstants(Type t)
    {
      var ass = t.Assembly;
      var names = ass.GetManifestResourceNames();
      for (int i = 0; i < names.Length; i++)
      {
        var name = names[i];
        if (name.StartsWith("SerializedConstants"))
        {
          if (name.EndsWith(".gz"))
          {
            using (var s = ass.GetManifestResourceStream("SerializedConstants.gz"))
            {
              var arr = psyntax.Serialization.SERIALIZER.Deserialize(new GZipStream(s, CompressionMode.Decompress));
              return arr as object[];
            }
          }
          else
          {
            using (var s = ass.GetManifestResourceStream("SerializedConstants"))
            {
              var arr = psyntax.Serialization.SERIALIZER.Deserialize(s);
              return arr as object[];
            }
          }
        }
      }
      throw new ArgumentException("type contains no constants");
    }

    static Delegate MakeTypedCallable(Type returntype, Type[] argtypes, Callable c)
    {
      int arity = argtypes.Length;
      var d = Delegate.CreateDelegate(CallTargets[arity], c, Compiler.Generator.GetCallable(argtypes.Length));
      var meth = typeof(Typed.Utils).GetMethod("MakeTyped", new Type[] { CallTargets[arity] });
      var targs = new Type[arity + 1];
      int i = 0;
      for (; i < arity; i++)
			{
			  targs[i] = argtypes[i];
			}
      targs[i] = returntype;
      var gm = meth.MakeGenericMethod(targs);

      var wrapper = gm.Invoke(null, new object[] { d });

      return wrapper as Delegate;
    }

    static Delegate MakeVoidTypedCallable(Type[] argtypes, Callable c)
    {
      int arity = argtypes.Length;
      var d = Delegate.CreateDelegate(CallTargets[arity], c, Compiler.Generator.GetCallable(argtypes.Length));
      var meth = typeof(Typed.Utils).GetMethod("MakeVoidTyped", new Type[] { CallTargets[arity] });

      var gm = meth.IsGenericMethodDefinition ? meth.MakeGenericMethod(argtypes) : meth;

      var wrapper = gm.Invoke(null, new object[] { d });

      return wrapper as Delegate;
    }

    readonly static Type[] CallTargets = 
    {
      typeof(CallTarget0),
      typeof(CallTarget1),
      typeof(CallTarget2),
      typeof(CallTarget3),
      typeof(CallTarget4),
      typeof(CallTarget5),
      typeof(CallTarget6),
      typeof(CallTarget7),
      typeof(CallTarget8),
    };


    public static T ConvertToDelegate<T>(object proc)
    {
      if (!(proc is Callable))
      {
        Builtins.AssertionViolation("ConvertToDelegate", "not a procedure", proc);
      }
      MethodInfo meth = typeof(T).GetMethod("Invoke");
      ParameterInfo[] pars = meth.GetParameters();
      if (meth.ReturnType == typeof(void))
      {
        Delegate d = MakeVoidTypedCallable(Array.ConvertAll(pars, x => x.ParameterType), proc as Callable);
        return (T)(object)Delegate.CreateDelegate(typeof(T), d.Target , d.Method);
      }
      else
      {
        Delegate d = MakeTypedCallable(meth.ReturnType, Array.ConvertAll(pars, x => x.ParameterType), proc as Callable);
        return (T)(object)Delegate.CreateDelegate(typeof(T), d.Target, d.Method);
      }
    }

    public static bool StartProcess(Process p)
    {
      if (p.Start())
      {
        if (p.StartInfo.RedirectStandardOutput)
        {
          p.BeginOutputReadLine();
        }
        if (p.StartInfo.RedirectStandardError)
        {
          p.BeginErrorReadLine();
        }
        return true;
      }
      return false;
    }

    public static Process MakeProcess(string filename, string args, bool showwindow, object exit, object output, object error)
    {
      Process p = new Process();
      p.StartInfo = new ProcessStartInfo(filename, args);
      p.StartInfo.CreateNoWindow = !showwindow;
      Callable exitp = exit as Callable;
      Callable outp = output as Callable;
      Callable errorp = error as Callable;

      if (exitp != null)
      {
        p.EnableRaisingEvents = true;
        p.Exited += delegate
        {
          exitp.Call(p.ExitCode);
        };
      }
      if (outp != null)
      {
        p.StartInfo.UseShellExecute = false;
        p.StartInfo.RedirectStandardOutput = true;
        p.OutputDataReceived += delegate(object sender, DataReceivedEventArgs e)
        {
          if (!string.IsNullOrEmpty(e.Data))
          {
            outp.Call(e.Data);
          }
        };
      }
      if (errorp != null)
      {
        p.StartInfo.UseShellExecute = false;
        p.StartInfo.RedirectStandardError = true;
        p.ErrorDataReceived += delegate(object sender, DataReceivedEventArgs e)
        {
          if (!string.IsNullOrEmpty(e.Data))
          {
            errorp.Call(e.Data);
          }
        };
      }

      return p;
    }
  }
}
