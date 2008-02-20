using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting;
using System.Runtime.Serialization.Formatters.Binary;
using System.IO;

namespace IronScheme.Runtime.psyntax
{
  public class Serialization : Builtins
  {
    [Builtin("load-serialized-library")]
    public static object LoadLibrary(object filename, object sk)
    {
      string fn = RequiresNotNull<string>(filename);
      string faslfn = Path.ChangeExtension(fn, ".fasl");
      if (File.Exists(faslfn))
      {
        if (!File.Exists(fn) || File.GetLastWriteTime(faslfn) >= File.GetLastWriteTime(fn))
        {
          using (Stream input = File.OpenRead(faslfn))
          {
            int i = 0;
            object result = SERIALIZER.Deserialize(input);
            Cons pivot = result as Cons;
            while (i < 8)
            {
              pivot = (Cons)pivot.cdr;
              i++;
            }
            /*pivot.car = */EvalCore(Context, pivot.car);
            pivot = (Cons)pivot.cdr;
            /*pivot.car = */EvalCore(Context, pivot.car);

            return Apply(sk, result);
          }
        }
      }
      return FALSE;
    }

    static readonly BinaryFormatter SERIALIZER = new BinaryFormatter();

    [Builtin("serialize-library")]
    public static object SaveLibrary(object filename, object contents)
    {
      string fn = RequiresNotNull<string>(filename);
      // lets go cheap for now, just serialize, no compile
      using (Stream output = File.OpenWrite(Path.ChangeExtension(fn, ".fasl")))
      {
        SERIALIZER.Serialize(output, contents);
      }
      return Unspecified;
    }
  }
}
