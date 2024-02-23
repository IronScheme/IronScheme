using System;
using System.Diagnostics.SymbolStore;
using System.IO;
#if NETCOREAPP2_1_OR_GREATER
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
    public bool IsTransient(ModuleBuilder mb)
    {
#if NET9_0_OR_GREATER
      return true;
#elif !NETCOREAPP2_1_OR_GREATER
      return mb.IsTransient();
#else
      return true;
#endif
    }

    public ISymbolWriter GetSymbolWriter(ModuleBuilder mb)
    {
#if NET9_0_OR_GREATER
      return null;
#elif NETCOREAPP2_1_OR_GREATER
      return null;
#else
      return mb.GetSymWriter();
#endif
    }

    public void SetLocalSymInfo(LocalBuilder lb, string name)
    {
#if NET9_0_OR_GREATER
#elif !NETCOREAPP2_1_OR_GREATER
      lb.SetLocalSymInfo(name);
#endif
    }

    public void MarkSequencePoint(ILGenerator ilg, ISymbolDocumentWriter document, int startLine, int startColumn, int endLine, int endColumn)
    {
#if NET9_0_OR_GREATER
#elif !NETCOREAPP2_1_OR_GREATER
      ilg.MarkSequencePoint(document, startLine, startColumn, endLine, endColumn);
#endif
    }

    public ISymbolDocumentWriter CreateSymbolDocumentWriter(ModuleBuilder mb, string fn, Guid lang, Guid vendor, Guid doctype)
    {
#if NET9_0_OR_GREATER
      return null;
#elif !NETCOREAPP2_1_OR_GREATER
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
#if NET9_0_OR_GREATER
      ass.Save(filename);
#elif !NETCOREAPP2_1_OR_GREATER
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
#if NETCOREAPP2_1_OR_GREATER
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
#if NET9_0_OR_GREATER
        ab = AssemblyBuilder.DefinePersistedAssembly(asmname, typeof(object).Assembly);
        mb = ab.DefineDynamicModule(actualModuleName);
#elif !NETCOREAPP2_1_OR_GREATER
        var domain = AppDomain.CurrentDomain;
        ab = domain.DefineDynamicAssembly(asmname, AssemblyBuilderAccess.Save, outDir, null);
        mb = ab.DefineDynamicModule(actualModuleName, outFileName, emitDebugInfo);
#else
        ab = AssemblyBuilder.DefineDynamicAssembly(asmname, AssemblyBuilderAccess.RunAndCollect);
        mb = ab.DefineDynamicModule(actualModuleName);
#endif
      }

#if !NETCOREAPP2_1_OR_GREATER
      ab.DefineVersionInfoResource();
#endif

#pragma warning restore 0618
    }

    public void Initialize()
    {
#if NETCOREAPP2_1_OR_GREATER
      //TODO: check if this can be removed, was possibly just a hacking artefact
      Thread.AllocateNamedDataSlot("foo");
      Thread.FreeNamedDataSlot("foo");
#endif
    }

    public void SerializeConstants(MemoryStream s, ModuleBuilder mb, bool compress)
    {
#if NET9_0_OR_GREATER
      
#elif !NETCOREAPP2_1_OR_GREATER
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

