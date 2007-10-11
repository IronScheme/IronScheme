/* ****************************************************************************
 *
 * Copyright (c) Microsoft Corporation. 
 *
 * This source code is subject to terms and conditions of the Microsoft Permissive License. A 
 * copy of the license can be found in the License.html file at the root of this distribution. If 
 * you cannot locate the  Microsoft Permissive License, please send an email to 
 * dlr@microsoft.com. By using this source code in any fashion, you are agreeing to be bound 
 * by the terms of the Microsoft Permissive License.
 *
 * You must not remove this notice, or any other, from this software.
 *
 *
 * ***************************************************************************/

using System;
using System.Resources;
using System.Diagnostics;
using System.Diagnostics.SymbolStore;
using System.Reflection;
using System.Reflection.Emit;
using System.IO;
using System.Security;
using System.Security.Policy;
using System.Security.Permissions;
using System.Threading;
using System.Globalization;
using System.Collections.Generic;

using Microsoft.Scripting.Hosting;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Generation {
    public class AssemblyGen {
        private readonly AssemblyBuilder _myAssembly;
        private ModuleBuilder _myModule;
        private AssemblyGenAttributes _genAttrs;
        private int _index;
        
        private ISymbolDocumentWriter _symbolWriter;
        private readonly string _outFileName;       // can be null iff !SaveAndReloadAssemblies
        private PortableExecutableKinds _peKind;
        private ImageFileMachine _machine;

#if !SILVERLIGHT
        private readonly string _outDir;            // null means the current directory
        private const string peverify_exe = "peverify.exe";
#endif

        public AssemblyGen(string moduleName,
            string outDir,
            string outFile,
            AssemblyGenAttributes generationAttributes)
            :
            this(moduleName, outDir, outFile, generationAttributes, 
            PortableExecutableKinds.ILOnly, ImageFileMachine.I386) { 
        }

        public AssemblyGen(string moduleName, 
            string outDir, 
            string outFile, 
            AssemblyGenAttributes generationAttributes,
            PortableExecutableKinds peKind, 
            ImageFileMachine machine) {

            Contract.Requires(!String.IsNullOrEmpty(moduleName), "moduleName", "Module name cannot be a null reference or an empty string.");
            Contract.Requires(outFile != null || !SaveAndReloadAssemblies, "outFile", "SaveAssemblies mode requires non-null output file name.");

            _genAttrs = generationAttributes;

            AssemblyName asmname = new AssemblyName();

            AppDomain domain = System.Threading.Thread.GetDomain();

            _machine = machine;
            _peKind = peKind;
            _outFileName = outFile;

#if SILVERLIGHT  // AssemblyBuilderAccess.RunAndSave, Environment.CurrentDirectory
            asmname.Name = moduleName;
            _myAssembly = domain.DefineDynamicAssembly(asmname, AssemblyBuilderAccess.Run);
            _myModule = _myAssembly.DefineDynamicModule(moduleName, EmitDebugInfo);
#else
            try {
                outDir = Path.GetFullPath(String.IsNullOrEmpty(outDir) ? Environment.CurrentDirectory : outDir);
            } catch (Exception e) {
                throw new ArgumentException("Invalid output directory", e);
            }

            if (SaveAndReloadAssemblies || VerifyAssemblies) {
                _outDir = outDir;
            }

            if (SaveAndReloadAssemblies) {
                asmname.Name = Path.GetFileNameWithoutExtension(_outFileName);
                _myAssembly = domain.DefineDynamicAssembly(asmname, AssemblyBuilderAccess.RunAndSave, outDir, null);
                _myModule = _myAssembly.DefineDynamicModule(_outFileName, _outFileName, EmitDebugInfo);
            } else {
                asmname.Name = moduleName;
                _myAssembly = domain.DefineDynamicAssembly(asmname, AssemblyBuilderAccess.Run);
                _myModule = _myAssembly.DefineDynamicModule(moduleName, EmitDebugInfo);
            }
            _myAssembly.DefineVersionInfoResource();
#endif
            if (EmitDebugInfo) SetDebuggableAttributes();
        }

        private void SetDebuggableAttributes() {
            if (GenerateDebugAssemblies) {
                Type[] argTypes = new Type[] { typeof(DebuggableAttribute.DebuggingModes) };
                Object[] argValues = new Object[] { CurrentDebuggableAttributes };

                _myAssembly.SetCustomAttribute(new CustomAttributeBuilder(
                   typeof(DebuggableAttribute).GetConstructor(argTypes), argValues)
                   );

                _myModule.SetCustomAttribute(new CustomAttributeBuilder(
                    typeof(DebuggableAttribute).GetConstructor(argTypes), argValues)
                    );
            }
        }

        private DebuggableAttribute.DebuggingModes CurrentDebuggableAttributes {
            get {
                return DebuggableAttribute.DebuggingModes.Default |
                       DebuggableAttribute.DebuggingModes.IgnoreSymbolStoreSequencePoints |
                       (DisableOptimizations ?
                            DebuggableAttribute.DebuggingModes.DisableOptimizations :
                            DebuggableAttribute.DebuggingModes.None);
            }
        }

        public void SetSourceUnit(SourceUnit sourceUnit) {
            if (EmitDebugInfo) {
                Debug.Assert(sourceUnit.IsVisibleToDebugger);
                _symbolWriter = _myModule.DefineDocument(
                    sourceUnit.Id,
                    sourceUnit.Engine.LanguageGuid,
                    sourceUnit.Engine.VendorGuid,
                    SymbolGuids.DocumentType_Text);
			}
		}

        public bool EmitDebugInfo {
            get {
                return (_genAttrs & AssemblyGenAttributes.EmitDebugInfo) == AssemblyGenAttributes.EmitDebugInfo;
            }
        }

        public bool ILDebug {
            get {
                return (_genAttrs & AssemblyGenAttributes.ILDebug) == AssemblyGenAttributes.ILDebug;
            }
        }

        public bool BeforeFieldInit {
            get {
                return (_genAttrs & AssemblyGenAttributes.BeforeFieldInit) == AssemblyGenAttributes.BeforeFieldInit;
            }
        }

        public bool DisableOptimizations {
            get {
                return (_genAttrs & AssemblyGenAttributes.DisableOptimizations) == AssemblyGenAttributes.DisableOptimizations;
            }
        }

#if !SILVERLIGHT // IResourceWriter
        public void AddResourceFile(string name, string file, ResourceAttributes attribute) {
            IResourceWriter rw = _myModule.DefineResource(Path.GetFileName(file), name, attribute);

            string ext = Path.GetExtension(file);
            if(String.Equals(ext, ".resources", StringComparison.InvariantCultureIgnoreCase)) {
                ResourceReader rr = new ResourceReader(file);
                using (rr) {
                    System.Collections.IDictionaryEnumerator de = rr.GetEnumerator();

                    while (de.MoveNext()) {
                        string key = de.Key as string;
                        rw.AddResource(key, de.Value);
                    }
                }
            } else {
                rw.AddResource(name, File.ReadAllBytes(file));
            }
        }
#endif

        public Assembly DumpAndLoad() {
#if SILVERLIGHT // AssemblyBuilder.Save
            return _myAssembly;
#else
            if (!SaveAndReloadAssemblies) {
                return _myAssembly;
            }

            string fullPath = Path.Combine(_outDir, _outFileName);

            try {
                Dump();
            } catch (IOException) {
                return _myAssembly;
            }

            return Assembly.LoadFile(fullPath);
#endif
        }

        public void Dump() {
            Dump(null);
        }

        public void Dump(string fileName) {
#if !SILVERLIGHT // AssemblyBuilder.Save
            _myAssembly.Save(fileName ?? _outFileName, _peKind, _machine);
            if (VerifyAssemblies) {
                PeVerifyThis();
            }
#endif
        }

        private static string FindPeverify() {
#if !SILVERLIGHT // Environment.GetEnvironmentVariable
            string path = System.Environment.GetEnvironmentVariable("PATH");
            string[] dirs = path.Split(';');
            foreach (string dir in dirs) {
                string file = Path.Combine(dir, peverify_exe);
                if (File.Exists(file)) {
                    return file;
                }
            }
#endif
            return null;
        }

#if !SILVERLIGHT // ProcessStartInfo
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1031:DoNotCatchGeneralExceptionTypes")]
        private void PeVerifyThis() {
            string peverifyPath = FindPeverify();
            if (peverifyPath == null) {
                return;
            }

            int exitCode = 0;
            string strOut = null;
            string verifyFile = null;

            try {
                string pythonPath = new FileInfo(Assembly.GetEntryAssembly().Location).DirectoryName;

                string assemblyFile = Path.Combine(_outDir, _outFileName).ToLower(CultureInfo.InvariantCulture);
                string assemblyName = Path.GetFileNameWithoutExtension(_outFileName);
                string assemblyExtension = Path.GetExtension(_outFileName);
                Random rnd = new System.Random();

                for (int i = 0; ; i++) {
                    string verifyName = string.Format(CultureInfo.InvariantCulture, "{0}_{1}_{2}{3}", assemblyName, i, rnd.Next(1, 100), assemblyExtension);
                    verifyName = Path.Combine(Path.GetTempPath(), verifyName);

                    try {
                        File.Copy(assemblyFile, verifyName);
                        verifyFile = verifyName;
                        break;
                    } catch (IOException) {
                    }
                }

                // copy any DLLs or EXEs created by the process during the run...
                CopyFilesCreatedSinceStart(Path.GetTempPath(), Environment.CurrentDirectory);
                CopyDirectory(Path.GetTempPath(), pythonPath);
                if (ScriptDomainManager.Options.BinariesDirectory != null && ScriptDomainManager.Options.BinariesDirectory != Path.GetTempPath()) {
                    CopyFilesCreatedSinceStart(Path.GetTempPath(), ScriptDomainManager.Options.BinariesDirectory);
                }
                
                // /IGNORE=80070002 ignores errors related to files we can't find, this happens when we generate assemblies
                // and then peverify the result.  Note if we can't resolve a token thats in an external file we still
                // generate an error.
                ProcessStartInfo psi = new ProcessStartInfo(peverifyPath, "/IGNORE=80070002 \"" + verifyFile + "\"");
                psi.UseShellExecute = false;
                psi.RedirectStandardOutput = true;
                Process proc = Process.Start(psi);
                Thread thread = new Thread(
                    new ThreadStart(
                        delegate {
                            using (StreamReader sr = proc.StandardOutput) {
                                strOut = sr.ReadToEnd();
                            }
                        }
                        ));

                thread.Start();
                proc.WaitForExit();
                thread.Join();
                exitCode = proc.ExitCode;
                proc.Close();
            } catch(Exception e) {
                strOut = "Unexpected exception: " + e.ToString();
                exitCode = 1;
            }

            if (exitCode != 0) {
                Console.WriteLine("Verification failed w/ exit code {0}: {1}", exitCode, strOut);
                throw new VerificationException(String.Format(CultureInfo.CurrentCulture, 
                    Resources.VerificationException,
                    _outFileName, 
                    verifyFile, 
                    strOut ?? ""));
            }

            if (verifyFile != null) {
                File.Delete(verifyFile);
            }
        }

        private void CopyFilesCreatedSinceStart(string pythonPath, string dir) {
            DateTime start = Process.GetCurrentProcess().StartTime;
            foreach (string filename in Directory.GetFiles(dir)) {
                FileInfo fi = new FileInfo(filename);
                if (fi.Name != _outFileName) {
                    if (fi.LastWriteTime - start >= TimeSpan.Zero) {
                        try {
                            File.Copy(filename, Path.Combine(pythonPath, fi.Name), true);
                        } catch {
                            Console.WriteLine("Error copying {0}", filename);
                        }
                    }
                }
            }
        }

        private void CopyDirectory(string to, string from) {
            foreach (string filename in Directory.GetFiles(from)) {
                FileInfo fi = new FileInfo(filename);
                string toFile = Path.Combine(to, fi.Name);
                FileInfo toInfo = new FileInfo(toFile);

                if (fi.Extension.ToLower() == ".dll" || fi.Extension.ToLower() == ".exe") {
                    if (!File.Exists(toFile) || toInfo.LastWriteTime < fi.LastWriteTime) {
                        try {
                            File.Copy(filename, toFile, true);
                        } catch { 
                            Console.WriteLine("Error copying {0}", filename); 
                        }
                    }
                }
            }
        }
#endif

        public TypeGen DefinePublicType(string name, Type parent) {
            TypeAttributes attrs = TypeAttributes.Public;
            if (BeforeFieldInit) attrs |= TypeAttributes.BeforeFieldInit;
            TypeBuilder tb = _myModule.DefineType(name.Replace('+', '_'), attrs);
            tb.SetParent(parent);
            return new TypeGen(this, tb);
        }

        // This overload is only available in CLR V2 SP1
        static readonly Type[] anonHostedDynamicMethodCtorSig = new Type[] { typeof(string), typeof(Type), typeof(Type[]) };
        static readonly ConstructorInfo anonHostedDynamicMethodCtor = typeof(DynamicMethod).GetConstructor(anonHostedDynamicMethodCtorSig);

        public CodeGen DefineMethod(string methodName, Type returnType, IList<Type> paramTypes, ConstantPool constantPool) {
            CodeGen cg;
            if (GenerateStaticMethods) {
                int index = Interlocked.Increment(ref _index);
                TypeGen tg = DefinePublicType("Type$" + methodName + "$" + index, typeof(object));
                cg = tg.DefineMethod("Handle" + index, returnType, paramTypes, null, constantPool);
                cg.DynamicMethod = true;
            } else {
                Type[] parameterTypes = CompilerHelpers.MakeParamTypeArray(paramTypes, constantPool);
                string dynamicMethodName = methodName + "##" + Interlocked.Increment(ref _index);
                DynamicMethod target;
#if SILVERLIGHT // Module-hosted DynamicMethod is not available in SILVERLIGHT
                target = new DynamicMethod(dynamicMethodName, returnType, parameterTypes);
#else
                if (anonHostedDynamicMethodCtor != null) {
                    object[] parameters = new object[] { dynamicMethodName, returnType, parameterTypes };
                    target = (DynamicMethod)anonHostedDynamicMethodCtor.Invoke(parameters);
                } else {
                    target = new DynamicMethod(dynamicMethodName, returnType, parameterTypes, _myModule);
                }
#endif
                cg = new CodeGen(null, this, target, target.GetILGenerator(), parameterTypes, constantPool);
            }
            return cg;
        }
#if !SILVERLIGHT
        public void SetEntryPoint(MethodInfo mi, PEFileKinds kind) {
            _myAssembly.SetEntryPoint(mi, kind);
        }
#endif

        public bool GenerateStaticMethods {
            get {
                return (_genAttrs & AssemblyGenAttributes.GenerateStaticMethods) != 0;
            }
            set {
                if (value) _genAttrs |= AssemblyGenAttributes.GenerateStaticMethods;
                else _genAttrs &= ~AssemblyGenAttributes.GenerateStaticMethods;
            }
        }

        public bool GenerateDebugAssemblies {
            get {
                return (_genAttrs & AssemblyGenAttributes.GenerateDebugAssemblies) != 0;
            }
            set {
                if (value) _genAttrs |= AssemblyGenAttributes.GenerateDebugAssemblies;
                else _genAttrs &= ~AssemblyGenAttributes.GenerateDebugAssemblies;
            }
        }

        public bool SaveAndReloadAssemblies {
            get {
                return (_genAttrs & AssemblyGenAttributes.SaveAndReloadAssemblies) != 0;
            }
            set {
                if (value) _genAttrs |= AssemblyGenAttributes.SaveAndReloadAssemblies;
                else _genAttrs &= ~AssemblyGenAttributes.SaveAndReloadAssemblies;
            }
        }

#if !SILVERLIGHT
        public bool VerifyAssemblies {
            get {
                return (_genAttrs & AssemblyGenAttributes.VerifyAssemblies) != 0;
            }
            set {
                if (value) _genAttrs |= AssemblyGenAttributes.VerifyAssemblies;
                else _genAttrs &= ~AssemblyGenAttributes.VerifyAssemblies;
            }
        }
#endif
        
        // TODO: SourceUnit should provide writers for each symbol document file used in the unit
        public ISymbolDocumentWriter SymbolWriter {
            get { return _symbolWriter; }
            set { _symbolWriter = value; }
        }

        public AssemblyBuilder AssemblyBuilder {
            get { return _myAssembly; }
        }

        internal ModuleBuilder ModuleBuilder {
            get { return _myModule; }
        }
    }

    internal static class SymbolGuids {
        internal static readonly Guid LanguageType_ILAssembly =
            new Guid(-1358664493, -12063, 0x11d2, 0x97, 0x7c, 0, 160, 0xc9, 180, 0xd5, 12);
        
        internal static readonly Guid DocumentType_Text =
            new Guid(0x5a869d0b, 0x6611, 0x11d3, 0xbd, 0x2a, 0, 0, 0xf8, 8, 0x49, 0xbd);

        internal static readonly Guid LanguageVendor_Microsoft =
            new Guid(-1723120188, -6423, 0x11d2, 0x90, 0x3f, 0, 0xc0, 0x4f, 0xa3, 2, 0xa1);
    }
}

