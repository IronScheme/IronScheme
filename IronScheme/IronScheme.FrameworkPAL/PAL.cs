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
using System.Collections.Generic;
using System.Diagnostics;

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

#if NET9_0_OR_GREATER
    Dictionary<ModuleBuilder, HashSet<ISymbolDocumentWriter>> modmap = new();
    Dictionary<ISymbolDocumentWriter, HashSet<ILGenerator>> ilmap = new();
    Dictionary<ILGenerator, List<int>> iloffsets = new();
#endif

    public void MarkSequencePoint(ILGenerator ilg, ISymbolDocumentWriter document, int startLine, int startColumn, int endLine, int endColumn)
    {
#if NET9_0_OR_GREATER
      if (!ilmap.TryGetValue(document, out var ilgs))
      {
        ilmap[document] = ilgs = new ();
      }

      ilgs.Add(ilg);

      if (ilg.ILOffset == 0 && startLine == 16707566)
      {
        return;
      }

      if (ilg.ILOffset == 0 && endColumn - startColumn == 1)
      {
        Debugger.Break();
      }

      if (!iloffsets.TryGetValue(ilg, out var offsets))
      {
        iloffsets[ilg] = offsets = new ();
      }

      if (offsets.Contains(ilg.ILOffset))
      {
        // skipping
        if (ilg.ILOffset == 0)
        {
          Debugger.Break();
        }
        return;
      }

      offsets.Add(ilg.ILOffset);

      //Console.Error.WriteLine("{0},{1},{2},{3},{4},{5}, {6}", ilg.ILOffset, startLine, startColumn, endLine, endColumn, filemap[document], ilg.GetHashCode());
      
      ilg.MarkSequencePoint(document, startLine, startColumn, endLine, endColumn);
#elif !NETCOREAPP2_1_OR_GREATER
      ilg.MarkSequencePoint(document, startLine, startColumn, endLine, endColumn);
#endif
    }

    public ISymbolDocumentWriter CreateSymbolDocumentWriter(ModuleBuilder mb, string fn, Guid lang, Guid vendor, Guid doctype)
    {

#if NET9_0_OR_GREATER || !NETCOREAPP2_1_OR_GREATER
      string properfn = null;
      if (fn.StartsWith("build") || fn.StartsWith("psyntax"))
      {
        // look for source up dir
        var path = Path.GetFullPath(fn);
        var rooti = path.LastIndexOf("IronScheme.Console");
        if (rooti > -1)
        {
          var rootp = Path.Combine(Path.Combine(path.Substring(0, rooti), "IronScheme.Console"), fn);
          if (File.Exists(rootp))
          {
            properfn = rootp;
          }
        }
      }

      if (properfn == null)
      {
        var fullPath = Path.GetFullPath(fn);
        properfn = fullPath;
      }

      var docwriter = mb.DefineDocument(
        properfn,
        lang,
        vendor,
        doctype);

#if NET9_0_OR_GREATER

      if (!modmap.TryGetValue(mb, out var writers))
      {
        modmap[mb] = writers = new ();
      }

      writers.Add(docwriter);
#endif

      return docwriter;
#else
      return null;
#endif
    }

    public void Save(AssemblyBuilder ass, string filename, ImageFileMachine machineKind)
    {
#if NET9_0_OR_GREATER
      try
      {
        if (filename == "ironscheme.boot.dll")
        {
          filename = Path.Combine("build", filename);
        }

        SaveNET9((PersistedAssemblyBuilder)ass, filename, DummySymbolWriter != null);
      }
      finally
      {
        foreach (var (key, value) in modmap)
        {
          if (key == ass.ManifestModule)
          {
            //Console.Write("removign garbage from {0} ", key.ScopeName);
            foreach (var w in value)
            {
              
              foreach (var ilg in ilmap[w])
              {
                //Console.Write("I");
                iloffsets.Remove(ilg);
              }

              //Console.Write("W");
              ilmap.Remove(w);
            }

            //Console.WriteLine();
            modmap.Remove(key);
            break;
          }
        }
      }

#elif !NETCOREAPP2_1_OR_GREATER
      ass.Save(Path.GetFileName(filename), PortableExecutableKinds.ILOnly, machineKind);
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
          //debugDirectoryBuilder.AddEmbeddedPortablePdbEntry(portablePdbBlob, portablePdbBuilder.FormatVersion);
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

