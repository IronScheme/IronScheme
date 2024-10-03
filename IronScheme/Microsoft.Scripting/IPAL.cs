using System;
using System.Diagnostics.SymbolStore;
using System.IO;
using System.Reflection;
using System.Reflection.Emit;

namespace IronScheme.FrameworkPAL
{
    public interface ISerializer
    {
        object Deserialize(Stream serializationStream);
        void Serialize(Stream serializationStream, object graph);
    }

    public delegate Type RecordBinderCallback(string assName, string typeName);

    public interface IPAL
    {
        void Initialize();
        ISymbolDocumentWriter CreateSymbolDocumentWriter(ModuleBuilder mb, string fn, Guid lang, Guid vendor, Guid doctype);
        object GetSymbolWriter(ModuleBuilder mb);
        bool IsTransient(ModuleBuilder mb);
        void MarkSequencePoint(ILGenerator ilg, ISymbolDocumentWriter document, int startLine, int startColumn, int endLine, int endColumn);
        void Save(AssemblyBuilder ass, string filename, ImageFileMachine machineKind);
        void SetLocalSymInfo(LocalBuilder lb, string name);

        void DefineAssembly(bool run, string outDir, AssemblyName asmname, string actualModuleName, string outFileName, bool emitDebugInfo, ref AssemblyBuilder ab, ref ModuleBuilder mb);
        void SerializeConstants(MemoryStream s, ModuleBuilder mb, bool compress);
        ISerializer GetSerializer(RecordBinderCallback rb);
    }

    public static class PAL
    {
        static readonly bool IsCore = typeof(object).Assembly.FullName.StartsWith("System.Private.CoreLib");
        static readonly bool IsNet9 = IsCore && typeof(object).Assembly.GetName().Version.Major >= 9;

        static readonly IPAL pal = LoadPAL();

        private static IPAL LoadPAL()
        {
            const string fn = "IronScheme.FrameworkPAL.dll";

            var fullPath = Path.Combine(Path.GetDirectoryName(typeof(PAL).Assembly.Location), fn);

            if (File.Exists(fullPath))
            {
                var ass = Assembly.LoadFrom(fullPath);
                var type = ass.GetType("IronScheme.FrameworkPAL.PALImpl", true);
                return (IPAL)Activator.CreateInstance(type);
            }
            else
            {
                var resource = typeof(PAL).Assembly.GetManifestResourceStream($"{(IsCore ? (IsNet9 ? "net9-": "core-") : "")}{fn}");
                if (resource == null)
                {
                    throw new PlatformNotSupportedException();
                }
                var buffer = new byte[resource.Length];
                resource.Read(buffer, 0, buffer.Length);
                var ass = Assembly.Load(buffer);
                var type = ass.GetType("IronScheme.FrameworkPAL.PALImpl", true);
                return (IPAL)Activator.CreateInstance(type);
            }
        }

        public static void Initialize() => pal.Initialize();
        public static ISymbolDocumentWriter CreateSymbolDocumentWriter(ModuleBuilder mb, string fn, Guid lang, Guid vendor, Guid doctype) => pal.CreateSymbolDocumentWriter(mb, fn, lang, vendor, doctype);
        public static object GetSymbolWriter(ModuleBuilder mb) => pal.GetSymbolWriter(mb);
        public static bool IsTransient(ModuleBuilder mb) => pal.IsTransient(mb);
        public static void MarkSequencePoint(ILGenerator ilg, ISymbolDocumentWriter document, int startLine, int startColumn, int endLine, int endColumn) =>
            pal.MarkSequencePoint(ilg, document, startLine, startColumn, endLine, endColumn);
        public static void SerializeConstants(MemoryStream s, ModuleBuilder mb, bool compress) => pal.SerializeConstants(s, mb, compress);
        public static void Save(AssemblyBuilder ass, string filename, ImageFileMachine machineKind) => pal.Save(ass, filename, machineKind);
        public static void SetLocalSymInfo(LocalBuilder lb, string name) => pal.SetLocalSymInfo(lb, name);
        public static void DefineAssembly(bool run, string outDir, AssemblyName asmname, string actualModuleName, string outFileName, bool emitDebugInfo, ref AssemblyBuilder ab, ref ModuleBuilder mb) =>
            pal.DefineAssembly(run, outDir, asmname, actualModuleName, outFileName, emitDebugInfo, ref ab, ref mb);

        public static ISerializer GetSerializer(RecordBinderCallback recordBinder) => pal.GetSerializer(recordBinder);
    }
}