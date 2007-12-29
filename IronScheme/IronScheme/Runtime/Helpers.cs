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

    static Helpers()
    {
      bf.AssemblyFormat = System.Runtime.Serialization.Formatters.FormatterAssemblyStyle.Simple;
      bf.FilterLevel = System.Runtime.Serialization.Formatters.TypeFilterLevel.Low;
      bf.TypeFormat = System.Runtime.Serialization.Formatters.FormatterTypeStyle.TypesWhenNeeded;
    }

    internal static BinaryFormatter bf = new BinaryFormatter();
  }
}
