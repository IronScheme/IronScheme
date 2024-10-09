using System;
using System.Diagnostics.SymbolStore;
using System.IO;
#if NETCOREAPP2_1_OR_GREATER
using System.Threading;
#endif
using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.Serialization;

#if NET9_0_OR_GREATER
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;
#endif

using Microsoft.Scripting;
using System.Runtime.Serialization.Formatters;
using System.Runtime.Serialization.Formatters.Binary;

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
#if NET9_0_OR_GREATER
      ilg.MarkSequencePoint(document, startLine, startColumn, endLine, endColumn);
#elif !NETCOREAPP2_1_OR_GREATER
      ilg.MarkSequencePoint(document, startLine, startColumn, endLine, endColumn);
#endif
    }

    public ISymbolDocumentWriter CreateSymbolDocumentWriter(ModuleBuilder mb, string fn, Guid lang, Guid vendor, Guid doctype)
    {
#if NET9_0_OR_GREATER || !NETCOREAPP2_1_OR_GREATER
      var docwriter = mb.DefineDocument(
        fn,
        lang,
        vendor,
        doctype);
      return docwriter;
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

      SaveNET9((PersistedAssemblyBuilder)ass, filename, DummySymbolWriter != null);

#elif !NETCOREAPP2_1_OR_GREATER
      ass.Save(filename, PortableExecutableKinds.ILOnly, machineKind);
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
      if (compress)
      {
        var cms = new MemoryStream();
        var cs = new System.IO.Compression.GZipStream(cms, System.IO.Compression.CompressionMode.Compress, true);
        var content = s.ToArray();
        cs.Write(content, 0, content.Length);
        cs.Close();

        mb.DefineInitializedData("SerializedConstants.gz", cms.ToArray(), FieldAttributes.Private);
      }
      else
      {
        mb.DefineInitializedData("SerializedConstants", s.ToArray(), FieldAttributes.Private);
      }
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
#else
      throw new NotSupportedException("Compiling is only supported on .NET Framework and .NET 9 or higher");
#endif
    }

    public ISerializer GetSerializer(RecordBinderCallback recordBinder)
    {
      return new Serializer(recordBinder);
    }

#pragma warning disable SYSLIB0011 // Type or member is obsolete
    class Serializer : ISerializer
    {
      readonly BinaryFormatter bf;

      public Serializer(RecordBinderCallback recordBinder)
      {
        bf = new BinaryFormatter();
        bf.AssemblyFormat = FormatterAssemblyStyle.Simple;
        bf.Binder = new TypeCorrector(recordBinder);
        bf.SurrogateSelector = new Selector();
      }

      public object Deserialize(Stream serializationStream)
      {
        return bf.Deserialize(serializationStream);
      }

      public void Serialize(Stream serializationStream, object graph)
      {
        bf.Serialize(serializationStream, graph); 
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

        static readonly ISerializationSurrogate surrogate = new SymbolSurrogate();
        static readonly ISerializationSurrogate surrogate2 = new BooleanSurrogate();

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
          return value ? RuntimeHelpers.True : RuntimeHelpers.False;
        }
      }

      static Assembly FindAssembly(string assname)
      {
        foreach (Assembly ass in AppDomain.CurrentDomain.GetAssemblies())
        {
          if (ass.FullName == assname)
          {
            return ass;
          }
        }

        // this might accidentally load the debug file..., and screw up stuff, dunno why...
        try
        {
          return Assembly.Load(assname);
        }
        // deal with public key shit, starting to regret this...
        catch (FileLoadException)
        {
          return Assembly.Load(assname.Replace("PublicKeyToken=null", "PublicKeyToken=78f2e9d9541a0dee"));
        }
      }

      sealed class TypeCorrector : SerializationBinder
      {
        readonly RecordBinderCallback recordBinder;

        public TypeCorrector(RecordBinderCallback recordBinder)
        {
          this.recordBinder = recordBinder;
        }

        public override Type BindToType(string assemblyName, string typeName)
        {
          Assembly a = FindAssembly(assemblyName);
          Type tt = a.GetType(typeName, false);

          if (tt == null)
          {
            //fall back for dynamic records
            return recordBinder(assemblyName, typeName);
          }
          else
          {
            return tt;
          }
        }
      }
    }

#pragma warning restore SYSLIB0011 // Type or member is obsolete

#if NET9_0_OR_GREATER
    private static void SaveNET9(PersistedAssemblyBuilder ab, string path, bool emitDebugInfo)
    {
      var assemblyFileName = Path.GetFileNameWithoutExtension(path);
      var assPath = path;
      var pdbPath = Path.ChangeExtension(assPath, "pdb");
      try
      {
        MetadataBuilder metadataBuilder = ab.GenerateMetadata(out BlobBuilder ilStream, out BlobBuilder fieldData, out MetadataBuilder pdbBuilder);

        DebugDirectoryBuilder debugDirectoryBuilder = null;

        if (emitDebugInfo)
        {
          BlobBuilder portablePdbBlob = new BlobBuilder();
          PortablePdbBuilder portablePdbBuilder = new PortablePdbBuilder(pdbBuilder, metadataBuilder.GetRowCounts(), entryPoint: default);
          BlobContentId pdbContentId = portablePdbBuilder.Serialize(portablePdbBlob);
          using FileStream pdbFileStream = new FileStream(pdbPath, FileMode.Create, FileAccess.Write);
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
        using var dllFileStream = new FileStream(assPath, FileMode.Create, FileAccess.Write);
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

