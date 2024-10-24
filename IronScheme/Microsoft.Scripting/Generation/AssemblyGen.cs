/* ****************************************************************************
 *
 * Copyright (c) Microsoft Corporation. 
 *
 * This source code is subject to terms and conditions of the Microsoft Public License. A 
 * copy of the license can be found in the License.html file at the root of this distribution. If 
 * you cannot locate the  Microsoft Public License, please send an email to 
 * dlr@microsoft.com. By using this source code in any fashion, you are agreeing to be bound 
 * by the terms of the Microsoft Public License.
 *
 * You must not remove this notice, or any other, from this software.
 *
 *
 * ***************************************************************************/

using System;
using System.Diagnostics;
using System.Reflection;
using System.Reflection.Emit;
using System.IO;
using System.Threading;
using System.Collections.Generic;
using Microsoft.Scripting.Hosting;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Generation
{
    public class AssemblyGen {
        private readonly AssemblyBuilder _myAssembly;
        private ModuleBuilder _myModule;
        private AssemblyGenAttributes _genAttrs;
        private int _index;
        
        private object _symbolWriter;
        private readonly string _outFileName;       // can be null iff !SaveAndReloadAssemblies
        private ImageFileMachine _machine;

        private readonly string _outDir;            // null means the current directory

        public static AssemblyGen CreateModuleAssembly(string suid)
        {
          return OptimizedModuleGenerator.CreateModuleAssembly(suid);
        }

        public AssemblyGen(string moduleName,
            string outDir,
            string outFile,
            AssemblyGenAttributes generationAttributes)
            :
            this(moduleName, outDir, outFile, generationAttributes, ImageFileMachine.I386) { 
        }

        public AssemblyGen(string moduleName, 
            string outDir, 
            string outFile, 
            AssemblyGenAttributes generationAttributes,
            ImageFileMachine machine) {

            Contract.Requires(!String.IsNullOrEmpty(moduleName), "moduleName", "Module name cannot be a null reference or an empty string.");
            Contract.Requires(outFile != null || !SaveAndReloadAssemblies, "outFile", "SaveAssemblies mode requires non-null output file name.");

            _genAttrs = generationAttributes;

            AssemblyName asmname = new AssemblyName();

            AppDomain domain = AppDomain.CurrentDomain; //System.Threading.Thread.GetDomain();

            _machine = machine;
            _outFileName = outFile;

            try {
                outDir = Path.GetFullPath(String.IsNullOrEmpty(outDir) ? Environment.CurrentDirectory : outDir);
            } catch (Exception e) {
                throw new ArgumentException("Invalid output directory", e);
            }

            if (SaveAndReloadAssemblies) {
                _outDir = outDir;
            }

            if (moduleName == "ironscheme.boot.new")
            {
              _outDir = outDir = Path.Combine(outDir, "build");
              _outFileName = "ironscheme.boot.dll";
            }

          // SymbolWriter fails on Mono for some reason

            if (SaveAndReloadAssemblies)
            {
                asmname.Name = moduleName == "ironscheme.boot.new" ? "ironscheme.boot" : moduleName;
                asmname.Version = new Version("1.0.0.0");

                var actualModuleName = moduleName == "ironscheme.boot.new" ? "ironscheme.boot.dll" : _outFileName;

                PAL.DefineAssembly(false, outDir, asmname, actualModuleName, _outFileName, EmitDebugInfo, ref _myAssembly, ref _myModule);
            }
            else {
                asmname.Name = moduleName;

                PAL.DefineAssembly(true, null, asmname, moduleName, null, EmitDebugInfo, ref _myAssembly, ref _myModule);
            }

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
                       //DebuggableAttribute.DebuggingModes.IgnoreSymbolStoreSequencePoints |
                       (DisableOptimizations ?
                            DebuggableAttribute.DebuggingModes.DisableOptimizations :
                            DebuggableAttribute.DebuggingModes.None);
            }
        }

        internal Guid LanguageGuid { get; set; }
        internal Guid VendorGuid { get; set; }

        public void SetSourceUnit(SourceUnit sourceUnit)
        {
          if (EmitDebugInfo)
          {
            LanguageGuid = sourceUnit.Engine.LanguageGuid;
            VendorGuid = sourceUnit.Engine.VendorGuid;
            CreateSymWriter();
          }
        }

        public bool EmitDebugInfo {
            get {
                return (_genAttrs & AssemblyGenAttributes.EmitDebugInfo) == AssemblyGenAttributes.EmitDebugInfo;
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

        public Assembly DumpAndLoad() {
            if (!SaveAndReloadAssemblies) {
                return _myAssembly;
            }

            string fullPath = Path.Combine(_outDir, _outFileName);

            try {
              Dump(fullPath);
            } catch (IOException) { // this looks weird
                return _myAssembly;
            }

            if (_outFileName == "ironscheme.boot.new.dll")
            {
              return _myAssembly;
            }

            if (File.Exists(fullPath))
            {
                // this is not really ideal, but it seems to work fine for now
                if (_outDir == Environment.CurrentDirectory)
                {
                    return Assembly.LoadFrom(fullPath);
                }
                else
                {
                    //Console.Error.WriteLine("LoadFile (AssemblyGen.cs:202) {0}", fullPath);
                    return Assembly.LoadFile(fullPath);
                }
            }
            return _myAssembly;
        }

        public void Dump(string fileName) {
            PAL.Save(_myAssembly, fileName, _machine);
        }


      public TypeGen DefineType(string name, Type parent, TypeAttributes attrs)
      {
        if (BeforeFieldInit) attrs |= TypeAttributes.BeforeFieldInit;
        TypeBuilder tb = _myModule.DefineType(name.Replace('+', '_'), attrs); //& is also illegal here
        tb.SetParent(parent);
        return new TypeGen(this, tb);
      }

        public TypeGen DefinePublicType(string name, Type parent) {
            return DefineType(name, parent, TypeAttributes.Public);
        }

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
                DynamicMethod target = new DynamicMethod(dynamicMethodName, returnType, parameterTypes, _myModule);
                cg = new CodeGen(null, this, target, target.GetILGenerator(), parameterTypes, constantPool);
            }
            return cg;
        }

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
        
        // TODO: SourceUnit should provide writers for each symbol document file used in the unit
        public bool HasSymbolWriter {
            get { return _symbolWriter != null; }
            //set { _symbolWriter = value; }
        }

        public AssemblyBuilder AssemblyBuilder {
            get { return _myAssembly; }
        }

        public ModuleBuilder ModuleBuilder {
            get { return _myModule; }
        }

        internal void CreateSymWriter()
        {
            _symbolWriter = PAL.GetSymbolWriter(_myModule);
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

