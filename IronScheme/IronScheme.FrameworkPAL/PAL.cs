using System;
using System.Diagnostics.SymbolStore;
using System.IO;
#if NETCOREAPP2_1_OR_GREATER
using System.Threading;
#endif
using System.Reflection;
using System.Reflection.Emit;

#if NET9_0_OR_GREATER
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;
#endif

namespace IronScheme.FrameworkPAL
{
  public class PALImpl : IPAL
  {
    // this is just for some optimizations, probably only called on .NET Framework
    public bool IsTransient(ModuleBuilder mb)
    {
#if !NETCOREAPP2_1_OR_GREATER
      return mb.IsTransient();
#else
      return true;
#endif
    }

    public object GetSymbolWriter(ModuleBuilder mb)
    {
#if NET9_0_OR_GREATER
      return DummySymbolWriter;
#elif NETCOREAPP2_1_OR_GREATER
      return null;
#else
      return mb.GetSymWriter();
#endif
    }

    public void SetLocalSymInfo(LocalBuilder lb, string name)
    {
#if NET9_0_OR_GREATER || !NETCOREAPP2_1_OR_GREATER
      lb.SetLocalSymInfo(name);
#endif
    }

    public void MarkSequencePoint(ILGenerator ilg, ISymbolDocumentWriter document, int startLine, int startColumn, int endLine, int endColumn)
    {
#if NET9_0_OR_GREATER || !NETCOREAPP2_1_OR_GREATER
      ilg.MarkSequencePoint(document, startLine, startColumn, endLine, endColumn);
#endif
    }

    public ISymbolDocumentWriter CreateSymbolDocumentWriter(ModuleBuilder mb, string fn, Guid lang, Guid vendor, Guid doctype)
    {
#if NET9_0_OR_GREATER || !NETCOREAPP2_1_OR_GREATER
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
      if (filename == "ironscheme.boot.dll")
      {
        filename = Path.Combine("build", filename);
      }

      SaveNET9((PersistedAssemblyBuilder)ass, Path.GetFileNameWithoutExtension(filename), DummySymbolWriter != null);

#elif !NETCOREAPP2_1_OR_GREATER
      ass.Save(filename, PortableExecutableKinds.ILOnly, machineKind);
#elif LOKAD
      var gen = new Lokad.ILPack.AssemblyGenerator();
      gen.GenerateAssembly(ass, filename);
#else
      throw new NotSupportedException("Compiling is only supported on .NET Framework and .NET 9 or higher");
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
        if (emitDebugInfo)
        {
          DummySymbolWriter = new object();
        }
        //var mscorlib = Array.Find(AppDomain.CurrentDomain.GetAssemblies(), a => a.GetName().Name == "mscorlib");
        var pab = new PersistedAssemblyBuilder(asmname, typeof(object).Assembly);
        ab = pab;
        mb = ab.DefineDynamicModule(actualModuleName);
#elif !NETCOREAPP2_1_OR_GREATER
        var domain = AppDomain.CurrentDomain;
        ab = domain.DefineDynamicAssembly(asmname, AssemblyBuilderAccess.Save, outDir, null);
        mb = ab.DefineDynamicModule(actualModuleName, outFileName, emitDebugInfo);
#else
        throw new NotSupportedException("Compiling is only supported on .NET Framework and .NET 9 or higher");
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
      mb.DefineInitializedData("SerializedConstants", s.ToArray(), FieldAttributes.Private);
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

#if NET9_0_OR_GREATER
    private static void SaveNET9(PersistedAssemblyBuilder ab, string assemblyFileName, bool emitDebugInfo)
    {
      try
      {
        MetadataBuilder metadataBuilder = ab.GenerateMetadata(out BlobBuilder ilStream, out BlobBuilder fieldData, out MetadataBuilder pdbBuilder);

        DebugDirectoryBuilder debugDirectoryBuilder = null;

        if (emitDebugInfo)
        {
          BlobBuilder portablePdbBlob = new BlobBuilder();
          PortablePdbBuilder portablePdbBuilder = new PortablePdbBuilder(pdbBuilder, metadataBuilder.GetRowCounts(), entryPoint: default);
          BlobContentId pdbContentId = portablePdbBuilder.Serialize(portablePdbBlob);
          using FileStream pdbFileStream = new FileStream($"{assemblyFileName}.pdb", FileMode.Create, FileAccess.Write);
          portablePdbBlob.WriteContentTo(pdbFileStream);

          debugDirectoryBuilder = new DebugDirectoryBuilder();
          debugDirectoryBuilder.AddCodeViewEntry($"{assemblyFileName}.pdb", pdbContentId, portablePdbBuilder.FormatVersion);
        }

        ManagedPEBuilder peBuilder = new ManagedPEBuilder(
                        header: new PEHeaderBuilder(imageCharacteristics: Characteristics.ExecutableImage | Characteristics.Dll),
                        metadataRootBuilder: new MetadataRootBuilder(metadataBuilder),
                        ilStream: ilStream,
                        mappedFieldData: fieldData,
                        debugDirectoryBuilder: debugDirectoryBuilder);

        BlobBuilder peBlob = new BlobBuilder();
        peBuilder.Serialize(peBlob);
        using var dllFileStream = new FileStream($"{assemblyFileName}.dll", FileMode.Create, FileAccess.Write);
        peBlob.WriteContentTo(dllFileStream);
      }
      finally
      {
        DummySymbolWriter = null;
      }
    }

    static object DummySymbolWriter = null;
#endif
  }
}

