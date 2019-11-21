using System;
using System.Diagnostics.SymbolStore;
using System.IO;
using System.Reflection;
using System.Reflection.Emit;

namespace IronScheme.FrameworkPAL
{
    public interface IPAL
    {
        void Initialize();
        ISymbolDocumentWriter CreateSymbolDocumentWriter(ModuleBuilder mb, string fn, Guid lang, Guid vendor, Guid doctype);
        bool ExcludeParamtypes(MethodInfo mi);
        ISymbolWriter GetSymbolWriter(ModuleBuilder mb);
        bool IsTransient(ModuleBuilder mb);
        void MarkSequencePoint(ILGenerator ilg, ISymbolDocumentWriter document, int startLine, int startColumn, int endLine, int endColumn);
        void Save(AssemblyBuilder ass, string filename, ImageFileMachine machineKind);
        void SetLocalSymInfo(LocalBuilder lb, string name);

        void DefineAssembly(bool run, string outDir, AssemblyName asmname, string actualModuleName, string outFileName, bool emitDebugInfo, ref AssemblyBuilder ab, ref ModuleBuilder mb);
        void SerializeConstants(MemoryStream s, ModuleBuilder mb, bool compress);
    }

    public static class PAL
    {
        static readonly bool IsCore = typeof(object).Assembly.FullName.StartsWith("System.Private.CoreLib");

        static readonly IPAL pal = LoadPAL();

        private static IPAL LoadPAL()
        {
            const string fn = "IronScheme.FrameworkPAL.dll";

            if (File.Exists(fn))
            {
                var ass = Assembly.LoadFrom(fn);
                return (IPAL)Activator.CreateInstance(ass.GetType("IronScheme.FrameworkPAL.PALImpl"));
            }
            else
            {
                var resource = typeof(PAL).Assembly.GetManifestResourceStream($"{(IsCore ? "core-" : "")}{fn}");
                if (resource == null)
                {
                    throw new PlatformNotSupportedException();
                }
                var buffer = new byte[resource.Length];
                resource.Read(buffer, 0, buffer.Length);
                var ass = Assembly.Load(buffer);
                return (IPAL)Activator.CreateInstance(ass.GetType("IronScheme.FrameworkPAL.PALImpl"));
            }
        }
        public static void Initialize() => pal.Initialize();
        public static ISymbolDocumentWriter CreateSymbolDocumentWriter(ModuleBuilder mb, string fn, Guid lang, Guid vendor, Guid doctype) => pal.CreateSymbolDocumentWriter(mb, fn, lang, vendor, doctype);
        public static bool ExcludeParamtypes(MethodInfo mi) => pal.ExcludeParamtypes(mi);
        public static ISymbolWriter GetSymbolWriter(ModuleBuilder mb) => pal.GetSymbolWriter(mb);
        public static bool IsTransient(ModuleBuilder mb) => pal.IsTransient(mb);
        public static void MarkSequencePoint(ILGenerator ilg, ISymbolDocumentWriter document, int startLine, int startColumn, int endLine, int endColumn) =>
            pal.MarkSequencePoint(ilg, document, startLine, startColumn, endLine, endColumn);

        public static void SerializeConstants(MemoryStream s, ModuleBuilder mb, bool compress) => pal.SerializeConstants(s, mb, compress);
        public static void Save(AssemblyBuilder ass, string filename, ImageFileMachine machineKind) => pal.Save(ass, filename, machineKind);
        public static void SetLocalSymInfo(LocalBuilder lb, string name) => pal.SetLocalSymInfo(lb, name);

        public static void DefineAssembly(bool run, string outDir, AssemblyName asmname, string actualModuleName, string outFileName, bool emitDebugInfo, ref AssemblyBuilder ab, ref ModuleBuilder mb) =>
            pal.DefineAssembly(run, outDir, asmname, actualModuleName, outFileName, emitDebugInfo, ref ab, ref mb);
    }
}