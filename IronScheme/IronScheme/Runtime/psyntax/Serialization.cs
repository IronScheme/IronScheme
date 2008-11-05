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
          try
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

              // this is to insure runtime constants can be read, long story... see psyntax/internal.ss
              ICallable e2c = SymbolValue(SymbolTable.StringToId("expanded2core")) as ICallable;

              object visit = e2c.Call(pivot.car);

              CallTarget0 visitproc = delegate
              {
                return EvalCore(Context, visit);
              };

              pivot.car = Closure.Make(Context, visitproc);

              pivot = (Cons)pivot.cdr;

              object invoke = e2c.Call(pivot.car);

              CallTarget0 invokeproc = delegate
              {
                return EvalCore(Context, invoke);
              };

              pivot.car = Closure.Make(Context, invokeproc);
#if CPS
              return Apply(Closure.IdentityForCPS, sk, result);
#else
              return Apply(sk, result);
#endif
            }
          }
          catch (Exception ex)
          {
            Console.Error.WriteLine("WARNING: precompiled library ({1}) could not load. Reason: {0}", ex.Message, Path.GetFileName(fn));
            return FALSE;
          }
        }
        Console.Error.WriteLine("WARNING: precompiled library ({0}) is out-of-date. Run (compile-system-libraries) to update.", Path.GetFileName(fn));
      }
      return FALSE;
    }

    static Serialization()
    {
      SERIALIZER.AssemblyFormat = FormatterAssemblyStyle.Simple;
      SERIALIZER.Binder = new TypeCorrector();
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

    sealed class TypeCorrector : SerializationBinder
    {
      public override Type BindToType(string assemblyName, string typeName)
      {
        try
        {
          Assembly a = Assembly.Load(assemblyName);
          return a.GetType(typeName);
        }
        catch (Exception)
        {
          foreach (Type t in R6RS.Records.typedescriptors.Keys)
          {
            if (t.Name == typeName)
            {
              return t;
            }
          }
          return AssertionViolation(FALSE, "could not find type to deserialize", assemblyName, typeName) as Type;
        }

      }
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
