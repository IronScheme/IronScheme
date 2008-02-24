using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting;
using System.Runtime.Serialization.Formatters.Binary;
using System.IO;
using System.Runtime.Serialization.Formatters;
using System.Runtime.Serialization;
using System.Reflection;

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

            object visit = pivot.car;

            CallTarget0 visitproc = delegate
            {
              return EvalCore(Context, visit);
            };

            pivot.car = Closure.Make(Context, visitproc);

            pivot = (Cons)pivot.cdr;

            object invoke = pivot.car;

            CallTarget0 invokeproc = delegate
            {
              return EvalCore(Context, invoke);
            };

            pivot.car = Closure.Make(Context, invokeproc);

            return Apply(sk, result);
          }
        }
      }
      return FALSE;
    }

    static Serialization()
    {
      SERIALIZER.AssemblyFormat = FormatterAssemblyStyle.Simple;
      AppDomain.CurrentDomain.AssemblyResolve += new ResolveEventHandler(CurrentDomain_AssemblyResolve);
    }

    static Assembly CurrentDomain_AssemblyResolve(object sender, ResolveEventArgs args)
    {
      foreach (Assembly ass in AppDomain.CurrentDomain.GetAssemblies())
      {
        if (ass.FullName == args.Name)
        {
          return ass;
        }
      }
      return null;
    }

    internal static readonly BinaryFormatter SERIALIZER = new BinaryFormatter();

    [Builtin("serialize-library")]
    public static object SaveLibrary(object filename, object contents)
    {
      Console.WriteLine("serializing {0}", filename);
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
