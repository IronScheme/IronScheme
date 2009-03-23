using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using System.IO;
using System.Runtime.Serialization.Formatters.Binary;
using Microsoft.Scripting;
using System.Diagnostics;
using Microsoft.Scripting.Math;
using System.Runtime.InteropServices;

namespace IronScheme.Runtime
{
  public static class Helpers
  {
    static Dictionary<Type, Dictionary<string, object>> cache = new Dictionary<Type, Dictionary<string, object>>();

    public static object GetConstant(Type t, string id)
    {
      Dictionary<string, object> innerc;

      if (!cache.TryGetValue(t, out innerc))
      {
        cache[t] = innerc = new Dictionary<string, object>();
      }

      object c;
      if (!innerc.TryGetValue(id, out c))
      {
        Assembly ass = t.Assembly;
        Stream s = ass.GetManifestResourceStream(id);
        c = innerc[id] = bf.Deserialize(s);
      }

      return c;
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
      return SymbolTable.StringToObject(value.ToString().ToLower());
    }

    public static T SymbolToEnum<T>(object symbol)
    {
      if (symbol is SymbolId)
      {
        string name = SymbolTable.IdToString(RequiresNotNull<SymbolId>(symbol));
        try
        {
          return (T)Enum.Parse(typeof(T), name, true);
        }
        catch (Exception ex)
        {
          Builtins.AssertionViolation("symbol-to-enum", ex.Message, symbol, typeof(T));
          return default(T);
        }
      }
      else if (symbol is Cons)
      {
        int v = 0;
        foreach (object n in symbol as Cons)
        {
          v |= (int)(object)SymbolToEnum<T>(n); 
        }

        return (T)(object)v;
      }
      else
      {
        Builtins.AssertionViolation("symbol-to-enum", "Not a valid type", symbol, typeof(T));
        return default(T);
      }
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

    public static T RequiresNotNull<T>(object obj)
    {
      if (obj == null)
      {
        return (T) Builtins.AssertionViolation(GetCaller(), "argument cannot be null");
      }

      if (obj != null && !(obj is T))
      {
        // no way pass a box op :(
        object o = ConvertFromSchemeObject<T>(obj);
        if (o != null)
        {
          return (T)o;
        }
        return (T) Builtins.AssertionViolation(GetCaller(), "expected type: " + typeof(T).Name, obj);
      }

      return (T)obj;
    }

    static Helpers()
    {
      bf.AssemblyFormat = System.Runtime.Serialization.Formatters.FormatterAssemblyStyle.Simple;
      bf.FilterLevel = System.Runtime.Serialization.Formatters.TypeFilterLevel.Low;
      bf.TypeFormat = System.Runtime.Serialization.Formatters.FormatterTypeStyle.TypesWhenNeeded;
    }

    internal static BinaryFormatter bf = new BinaryFormatter();

    delegate void VoidCallable0();
    delegate void VoidCallable1(object t0);
    delegate void VoidCallable2(object t0, object t1);
    delegate void VoidCallable3(object t0, object t1, object t2);
    delegate void VoidCallable4(object t0, object t1, object t2, object t3);
    delegate void VoidCallable5(object t0, object t1, object t2, object t3, object t4);
    delegate void VoidCallableX(params object[] args);

    static Delegate MakeVoidCallable(ICallable c, int arity)
    {
      switch (arity)
      {
        case 0:
          return (VoidCallable0)delegate { c.Call(); };
        case 1:
          return (VoidCallable1)delegate(object o0) { c.Call(o0); };
        case 2:
          return (VoidCallable2)delegate(object o0, object o1) { c.Call(o0, o1); };
        case 3:
          return (VoidCallable3)delegate(object o0, object o1, object o2) { c.Call(o0, o1, o2); };
        case 4:
          return (VoidCallable4)delegate(object o0, object o1, object o2, object o3) { c.Call(o0, o1, o2, o3); };
        case 5:
          return (VoidCallable5)delegate(object o0, object o1, object o2, object o3, object o4) { c.Call(o0, o1, o2, o3, o4); };
        default:
          return (VoidCallableX)delegate(object[] args) { c.Call(args); };
      }
    }

    public static T ConvertToDelegate<T>(object proc)
    {
      if (proc is bool && !Builtins.IsTrue(proc))
      {
        return default(T);
      }
      MethodInfo meth = typeof(T).GetMethod("Invoke");
      ParameterInfo[] pars = meth.GetParameters();
      if (meth.ReturnType == typeof(void))
      {
        Delegate d = MakeVoidCallable(proc as ICallable, pars.Length);
        return (T)(object)Delegate.CreateDelegate(typeof(T), d.Target , d.Method);
      }
      else
      {
        return (T)(object)Delegate.CreateDelegate(typeof(T), proc, Compiler.Generator.GetCallable(pars.Length));
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
      ICallable exitp = exit as ICallable;
      ICallable outp = output as ICallable;
      ICallable errorp = error as ICallable;

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
