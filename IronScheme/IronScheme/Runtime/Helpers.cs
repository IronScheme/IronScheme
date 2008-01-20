using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using System.IO;
using System.Runtime.Serialization.Formatters.Binary;

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

    public static object EnumToSymbol<T>(T value)
    {
      return Builtins.StringToSymbol(value.ToString().ToLower());
    }

    public static T SymbolToEnum<T>(object symbol)
    {
      string name = Builtins.SymbolToString(symbol) as string;
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
  }
}
