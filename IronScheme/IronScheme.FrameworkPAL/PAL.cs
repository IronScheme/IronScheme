using System;
using System.Diagnostics.SymbolStore;
#if NETCOREAPP2_1
using System.Linq;
#endif
using System.Reflection;
using System.Reflection.Emit;

namespace IronScheme.FrameworkPAL
{
  public static class PAL
  {
    public static bool ExcludeParamtypes(MethodInfo mi)
    {
#if NETCOREAPP2_1
      if (mi.GetParameters()
        .Any(x => x.ParameterType.Namespace == "System" && x.ParameterType.Name.Contains("Span")))
      {
        return true;
      }

      return false;
#else
      return false;
#endif
    }

    public static bool IsTransient(ModuleBuilder mb)
    {
#if !NETCOREAPP2_1
      return mb.IsTransient();
#else
      return true;
#endif
    }

    public static ISymbolWriter GetSymbolWriter(ModuleBuilder mb)
    {
#if NETCOREAPP2_1
      return null;
#else
      return mb.GetSymWriter();
#endif
    }

    public static void SetLocalSymInfo(LocalBuilder lb, string name)
    {
#if !NETCOREAPP2_1
      lb.SetLocalSymInfo(name);
#endif
    }

    public static void MarkSequencePoint(ILGenerator ilg, ISymbolDocumentWriter document, int startLine,
      int startColumn, int endLine, int endColumn)
    {
#if !NETCOREAPP2_1
      ilg.MarkSequencePoint(document, startLine, startColumn, endLine, endColumn);
#endif
    }

    public static ISymbolDocumentWriter CreateSymbolDocumentWriter(ModuleBuilder mb, string fn, Guid lang, Guid vendor,
      Guid doctype)
    {
#if !NETCOREAPP2_1
      return mb.DefineDocument(
        fn,
        lang,
        vendor,
        doctype);
#endif
      return null;
    }

    public static void Save(AssemblyBuilder ass, string filename, ImageFileMachine machineKind)
    {
#if !NETCOREAPP2_1
      ass.Save(filename, PortableExecutableKinds.ILOnly, machineKind);
#else
      var gen = new Lokad.ILPack.AssemblyGenerator();
      gen.GenerateAssembly(ass, filename);
#endif
    }
  }
}
