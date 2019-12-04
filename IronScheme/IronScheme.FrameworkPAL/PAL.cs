using System;
using System.Diagnostics.SymbolStore;
using System.IO;
#if NETCOREAPP2_1
using System.Linq;
#endif
using System.Reflection;
using System.Reflection.Emit;
using System.Threading;
using IronScheme.Runtime;

namespace IronScheme.FrameworkPAL
{
  public class PALImpl : IPAL
  {
    public bool ExcludeParamtypes(MethodInfo mi)
    {
      // todo: check if this is a 'problem' on .NET 4.8+? too
#if NETCOREAPP2_1
      if (mi.GetParameters()
        .Any(x => x.ParameterType.Namespace == "System" && x.ParameterType.IsGenericType && x.ParameterType.Name.Contains("Span")))
      {
        return true;
      }

      return false;
#else
      return false;
#endif
    }

    public bool IsTransient(ModuleBuilder mb)
    {
#if !NETCOREAPP2_1
      return mb.IsTransient();
#else
      return true;
#endif
    }

    public ISymbolWriter GetSymbolWriter(ModuleBuilder mb)
    {
#if NETCOREAPP2_1
      return null;
#else
      return mb.GetSymWriter();
#endif
    }

    public void SetLocalSymInfo(LocalBuilder lb, string name)
    {
#if !NETCOREAPP2_1
      lb.SetLocalSymInfo(name);
#endif
    }

    public void MarkSequencePoint(ILGenerator ilg, ISymbolDocumentWriter document, int startLine, int startColumn, int endLine, int endColumn)
    {
#if !NETCOREAPP2_1
      ilg.MarkSequencePoint(document, startLine, startColumn, endLine, endColumn);
#endif
    }

    public ISymbolDocumentWriter CreateSymbolDocumentWriter(ModuleBuilder mb, string fn, Guid lang, Guid vendor, Guid doctype)
    {
#if !NETCOREAPP2_1
      return mb.DefineDocument(
        fn,
        lang,
        vendor,
        doctype);
#else
      return null;
#endif
    }

    public void Save(AssemblyBuilder ass, string filename, ImageFileMachine machineKind)
    {
#if !NETCOREAPP2_1
      ass.Save(filename, PortableExecutableKinds.ILOnly, machineKind);
#elif LOKAD
      var gen = new Lokad.ILPack.AssemblyGenerator();
      gen.GenerateAssembly(ass, filename);
#endif
    }

    public void DefineAssembly(bool run, string outDir, AssemblyName asmname, string actualModuleName, string outFileName, bool emitDebugInfo, ref AssemblyBuilder ab, ref ModuleBuilder mb)
    {
#pragma warning disable 0618
      if (run)
      {
#if NETCOREAPP2_1
        ab = AssemblyBuilder.DefineDynamicAssembly(asmname, AssemblyBuilderAccess.RunAndCollect);
        mb = ab.DefineDynamicModule(actualModuleName);
#else
        var domain = AppDomain.CurrentDomain;
        ab = domain.DefineDynamicAssembly(asmname, AssemblyBuilderAccess.Run);
        mb = ab.DefineDynamicModule(actualModuleName, emitDebugInfo);
#endif
      }
      else
      {
#if !NETCOREAPP2_1
        var domain = AppDomain.CurrentDomain;
        ab = domain.DefineDynamicAssembly(asmname, AssemblyBuilderAccess.Save, outDir, null);
        mb = ab.DefineDynamicModule(actualModuleName, outFileName, emitDebugInfo);
#else
        ab = AssemblyBuilder.DefineDynamicAssembly(asmname, AssemblyBuilderAccess.RunAndCollect);
        mb = ab.DefineDynamicModule(actualModuleName);
#endif
      }

#if !NETCOREAPP2_1
      ab.DefineVersionInfoResource();
#endif

#pragma warning restore 0618
    }

    public void Initialize()
    {
#if NETCOREAPP2_1
      //TODO: check if this can be removed, was possibly just a hacking artefact
      Thread.AllocateNamedDataSlot("foo");
      Thread.FreeNamedDataSlot("foo");
#endif
    }

    public void SerializeConstants(MemoryStream s, ModuleBuilder mb, bool compress)
    {
#if !NETCOREAPP2_1
      if (compress)
      {
        var cms = new MemoryStream();
        var cs = new System.IO.Compression.GZipStream(cms, System.IO.Compression.CompressionMode.Compress, true);
        var content = s.ToArray();
        cs.Write(content, 0, content.Length);
        cs.Close();

        mb.DefineManifestResource("SerializedConstants.gz", cms, ResourceAttributes.Private);
      }
      else
      {
        mb.DefineManifestResource("SerializedConstants", s, ResourceAttributes.Private);
      }
#endif
    }
  }
}

