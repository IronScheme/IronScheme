using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting;
using System.Runtime.Serialization.Formatters.Binary;
using System.IO;
using System.Runtime.Serialization.Formatters;
using System.Runtime.Serialization;
using System.Reflection;
using Microsoft.Scripting.Generation;

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
              ICallable e2c = SymbolValue(SymbolTable.StringToObject("expanded2core")) as ICallable;

#if CPS
              return FALSE;
              //object visit = OptimizedBuiltins.Call(e2c, pivot.car);

              //CallTarget0 visitproc = delegate
              //{
              //  return EvalCore(Context, visit);
              //};

              //pivot.car = Closure.Make(Context, OptimizedBuiltins.MakeCPS(visitproc));

              //pivot = (Cons)pivot.cdr;

              //object invoke = OptimizedBuiltins.Call(e2c, pivot.car);

              //CallTarget0 invokeproc = delegate
              //{
              //  return EvalCore(Context, invoke);
              //};

              //pivot.car = Closure.Make(Context, OptimizedBuiltins.MakeCPS(invokeproc));

              //return OptimizedBuiltins.Apply(Closure.IdentityForCPS, sk, result);
#else
              object visit = e2c.Call(pivot.car);

              pivot.car = CompileCore(cc, visit);

              pivot = (Cons)pivot.cdr;

              object invoke = e2c.Call(pivot.car);

              pivot.car = CompileCore(cc, invoke); 
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
      SERIALIZER.SurrogateSelector = new Selector();
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

    sealed class Selector : SurrogateSelector
    {
      public override ISerializationSurrogate GetSurrogate(Type type, StreamingContext context, out ISurrogateSelector selector)
      {
        if (type == typeof(SymbolId))
        {
          selector = this;
          return surrogate;
        }
        if (type == typeof(bool))
        {
          selector = this;
          return surrogate2;
        }
        return base.GetSurrogate(type, context, out selector);
      }

      static ISerializationSurrogate surrogate = new SymbolSurrogate();
      static ISerializationSurrogate surrogate2 = new BooleanSurrogate();
    }

    sealed class SymbolSurrogate : ISerializationSurrogate
    {
      public void GetObjectData(object obj, SerializationInfo info, StreamingContext context)
      {
        SymbolId s = (SymbolId)obj;
        info.AddValue("symbolName", SymbolTable.IdToString(s));
      }

      public object SetObjectData(object obj, SerializationInfo info, StreamingContext context, ISurrogateSelector selector)
      {
        string value = info.GetString("symbolName");
        int id = SymbolTable.StringToId(value).Id;
        return SymbolTable.GetSymbol(id);
      }
    }

    sealed class BooleanSurrogate : ISerializationSurrogate
    {
      public void GetObjectData(object obj, SerializationInfo info, StreamingContext context)
      {
        bool s = (bool)obj;
        info.AddValue("value", s);
      }

      public object SetObjectData(object obj, SerializationInfo info, StreamingContext context, ISurrogateSelector selector)
      {
        bool value = info.GetBoolean("value");
        return GetBool(value);
      }
    }


    sealed class TypeCorrector : SerializationBinder
    {
      public override Type BindToType(string assemblyName, string typeName)
      {
        try
        {
          Assembly a = Assembly.Load(assemblyName);
          return a.GetType(typeName, true);
        }
        catch (Exception)
        {
          typeName = typeName.Substring(0, typeName.Length - 36);
          foreach (var kvp in R6RS.Records.typedescriptors)
          {
            var t = kvp.Key;
            var v = kvp.Value;
            if (typeName == v.name) // watch for exceptions?
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
#if !CPS
      Console.WriteLine("serializing {0}", filename);
      string fn = RequiresNotNull<string>(filename);

      //object[] args = ListToVector(contents) as object[];

      //ReceiveContent(
      //  args[0],
      //  args[1],
      //  args[2],
      //  args[3],
      //  args[4],
      //  args[5],
      //  args[6],
      //  args[7],
      //  args[8],
      //  args[9],
      //  args[10]);

      // lets go cheap for now, just serialize, no compile
      using (Stream output = File.OpenWrite(Path.ChangeExtension(fn, ".fasl")))
      {
        SERIALIZER.Serialize(output, contents);
      }
#endif
      return Unspecified;





    }

    static void ReceiveContent(object id, object name, object ver, object imp, object vis, object inv, object exp_subst, object exp_env, object
          visit_proc, object invoke_proc, object visible)
    {
      ScriptCode visit, invoke;
      ScriptModule lib;

      AssemblyGenAttributes aga = ScriptDomainManager.Options.AssemblyGenAttributes;

      ScriptDomainManager.Options.AssemblyGenAttributes &= ~AssemblyGenAttributes.GenerateDebugAssemblies;
      ScriptDomainManager.Options.AssemblyGenAttributes &= ~AssemblyGenAttributes.EmitDebugInfo;
      ScriptDomainManager.Options.AssemblyGenAttributes &= ~AssemblyGenAttributes.DisableOptimizations;
      
      ScriptDomainManager.Options.AssemblyGenAttributes |= AssemblyGenAttributes.SaveAndReloadAssemblies;

      visit = Context.LanguageContext.CompileSourceCode(IronSchemeLanguageContext.CompileExpr(new Cons(visit_proc)));
      visit.SourceUnit.Id = "visit";
      invoke = Context.LanguageContext.CompileSourceCode(IronSchemeLanguageContext.CompileExpr(new Cons(invoke_proc)));
      invoke.SourceUnit.Id = string.Format("{0}", name);

      lib = ScriptDomainManager.CurrentManager.CreateModule(string.Format("{0}", name), invoke, visit);

      ScriptDomainManager.Options.AssemblyGenAttributes = aga;
    }
  }
}
