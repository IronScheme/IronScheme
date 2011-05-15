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
using System.Reflection.Emit;
using System.Collections.Generic;
using System.Reflection;
using System.Security;
using System.IO;
using System.Threading;
using System.Security.Permissions;

using Microsoft.Scripting;
using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Actions;
using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Utils;
using Microsoft.Scripting.Hosting;

namespace Microsoft.Scripting.Generation {
    /// <summary>
    /// Creates the code for an optimized module.  
    /// </summary>
    internal abstract class OptimizedModuleGenerator {
        private ScriptCode[] _scriptCodes;
        private string _moduleName;
        private Dictionary<LanguageContext, ScopeAllocator> _allocators = new Dictionary<LanguageContext, ScopeAllocator>();
        private List<CodeContext> _codeContexts = new List<CodeContext>();

        public string ModuleName {
            get { return _moduleName; }
        }

        protected OptimizedModuleGenerator(string moduleName, params ScriptCode[] scriptCodes) {
            Assert.NotNull(moduleName);
            Assert.NotNullItems(scriptCodes);

            _scriptCodes = scriptCodes;
            _moduleName = moduleName;
        }

        public static OptimizedModuleGenerator Create(string moduleName, params ScriptCode[] scriptCodes) {
            Contract.RequiresNotNull(moduleName, "moduleName");
            Contract.RequiresNotEmpty(scriptCodes, "scriptCodes");
            
            //if (scriptCodes.Length != 1) throw new NotSupportedException("Only one ScriptCode currently supported");

            // Silverlight: can't access SecurityPermission
            if (!ScriptDomainManager.Options.GenerateModulesAsSnippets) {
#if !SILVERLIGHT
                try {
                    // CreateStaticCodeGenerator requires ReflectionEmit (in CLR V2) or UnmanagedCode (in CLR V2 SP1) permission.
                    // If we are running in partial-trust, fall through to generated dynamic code.
                    // TODO: Symbol information requires unmanaged code permission.  Move the error
                    // handling lower and just don't dump PDBs.
                    new SecurityPermission(SecurityPermissionFlag.UnmanagedCode).Demand();

#endif
                    return new StaticFieldModuleGenerator(moduleName, scriptCodes);
#if !SILVERLIGHT
                } catch (SecurityException) {
                }
#endif
            }

            return new TupleModuleGenerator(moduleName, scriptCodes);
        }

        /// <summary>
        /// Creates the methods and optimized Scope's which get associated with each ScriptCode.
        /// </summary>
        public Scope GenerateScope() {
            List<CodeGen> cgs = GenerateScriptMethods();
            List<Scope> scopes = GenerateScriptScopes();

            Debug.Assert(cgs.Count == scopes.Count);
            Debug.Assert(scopes.Count == _scriptCodes.Length);
            Debug.Assert(_codeContexts.Count == _scriptCodes.Length);

            List<CallTargetWithContext0> targets = new List<CallTargetWithContext0>();
            foreach (CodeGen cg in cgs) {
                targets.Add((CallTargetWithContext0)cg.CreateDelegate(typeof(CallTargetWithContext0)));
            }

            // TODO: clean this up after clarifying dynamic site initialization logic
            for (int i = 0; i < _scriptCodes.Length; i++) {
                //Microsoft.Scripting.Actions.DynamicSiteHelpers.InitializeFields(_codeContexts[i], cgs[i].MethodInfo.DeclaringType);
            }

            // everything succeeded, commit the results
            for (int i = 0; i < _scriptCodes.Length; i++) {
                ScriptCode sc = _scriptCodes[i];

                sc.OptimizedTarget = targets[i];
                sc.OptimizedScope = scopes[i];
            }

            return scopes[0];
        }

        private List<Scope> GenerateScriptScopes() {
            List<Scope> scopes = new List<Scope>(_scriptCodes.Length);
            ScriptModule sm = ScriptDomainManager.CurrentManager.Host.DefaultModule as ScriptModule;
            for (int i = 0; i < _scriptCodes.Length; i++) {
                ScriptCode scriptCode = _scriptCodes[i];

                // Force creation of names used in other script codes into all optimized dictionaries
                ScopeAllocator allocator = _allocators[scriptCode.LanguageContext];
                IAttributesCollection iac = CreateLanguageDictionary(scriptCode.LanguageContext, allocator);
                Scope scope = new Scope(sm.Scope, iac);

                // module context is filled later:
                CodeContext codeContext = new CodeContext(scope, scriptCode.LanguageContext);

                IModuleDictionaryInitialization ici = iac as IModuleDictionaryInitialization;
                if (ici != null) {
                    ici.InitializeModuleDictionary(codeContext);
                }

                scopes.Add(scope);
                _codeContexts.Add(codeContext);
            }
            return scopes;
        }

        public void BindGeneratedCodeToModule(ScriptModule module) {
            Assert.NotNull(module);
            foreach (CodeContext codeContext in _codeContexts) {
                codeContext.ModuleContext = codeContext.LanguageContext.EnsureModuleContext(module);
            }
        }

        private List<CodeGen> GenerateScriptMethods() {
            List<CodeGen> cgs = new List<CodeGen>(_scriptCodes.Length);
            foreach (ScriptCode sc in _scriptCodes) {
                ScopeAllocator sa = CreateStorageAllocator(sc);
                CodeGen cg = CreateCodeGen(sc);
                cg.Allocator = sa;

                // every module can hand it's environment to anything embedded in it.
                cg.EnvironmentSlot = new EnvironmentSlot(new PropertySlot(
                    new PropertySlot(cg.ContextSlot, 
                        typeof(CodeContext).GetProperty("Scope")),
                    typeof(Scope).GetProperty("Dict"))
                );

                cg.Context = sc.CompilerContext;

                GlobalFieldAllocator gfa = sa.LocalAllocator as GlobalFieldAllocator;
                if (gfa != null)
                {
                  Dictionary<SymbolId, Slot> fields = gfa.SlotFactory.Fields;

                  Label ok = cg.DefineLabel();
                  cg.ContextSlot.EmitGet(cg);
                  //cg.EmitNull();
                  //cg.Emit(OpCodes.Ceq);
                  cg.Emit(OpCodes.Brtrue_S, ok);

                  // MyModuleDictType.ContextSlot = arg0
                  
                  cg.EmitNew(cg.TypeGen.DefaultConstructor);
                  cg.EmitArgGet(0);
                  cg.EmitCall(typeof(IModuleDictionaryInitialization), "InitializeModuleDictionary");
                  
                  cg.MarkLabel(ok);
                }

                sc.CodeBlock.EmitFunctionImplementation(cg);

                cg.Finish();

                cgs.Add(cg);
            }
            return cgs;
        }

        private ScopeAllocator CreateStorageAllocator(ScriptCode scriptCode) {
            ScopeAllocator allocator;
            if (!_allocators.TryGetValue(scriptCode.LanguageContext, out allocator)) {
                SlotFactory sf = CreateSlotFactory(scriptCode);
                ModuleGlobalFactory mgf = new ModuleGlobalFactory(sf);
                GlobalFieldAllocator gfa = new GlobalFieldAllocator(mgf);

                // Locals and globals are allocated from the same namespace for optimized modules
                ScopeAllocator global = new ScopeAllocator(null, gfa);
                allocator = new ScopeAllocator(global, gfa);

                _allocators[scriptCode.LanguageContext] = allocator;
            }

            return allocator;
        }

        #region Protected Members

        protected abstract CodeGen CreateCodeGen(ScriptCode scriptCode);
        protected abstract IAttributesCollection CreateLanguageDictionary(LanguageContext context, ScopeAllocator allocator);
        protected abstract SlotFactory CreateSlotFactory(ScriptCode scriptCode);

        #endregion
    }

    class TupleModuleGenerator : OptimizedModuleGenerator {
        private Dictionary<LanguageContext, TupleSlotFactory> _languages = new Dictionary<LanguageContext, TupleSlotFactory>();

        public TupleModuleGenerator(string moduleName, params ScriptCode[] scriptCodes)
            : base(moduleName, scriptCodes) {
        }

        #region Abstract overrides

        protected override SlotFactory CreateSlotFactory(ScriptCode scriptCode) {
            return _languages[scriptCode.LanguageContext] = new TupleSlotFactory(typeof(ModuleGlobalDictionary<>));
        }

        protected override IAttributesCollection CreateLanguageDictionary(LanguageContext context, ScopeAllocator allocator) {
            TupleSlotFactory tsf = _languages[context];
            object tuple = tsf.CreateTupleInstance();

            // TODO: Force all dictionaries to share same object data (for multi-module)

            IAttributesCollection res = (IAttributesCollection)Activator.CreateInstance(
                tsf.DictionaryType.MakeGenericType(tsf.TupleType),
                tuple,
                tsf.Names);

            return res;
        }

        protected override CodeGen CreateCodeGen(ScriptCode scriptCode) {
            return CompilerHelpers.CreateDynamicCodeGenerator(scriptCode.CompilerContext);
        }

        #endregion
    }

    class StaticFieldModuleGenerator : OptimizedModuleGenerator {
        //private static int _Counter;
        private Dictionary<LanguageContext, LanguageInfo> _languages = new Dictionary<LanguageContext, LanguageInfo>();

        private class LanguageInfo {
            [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1823:AvoidUnusedPrivateFields")] // TODO: fix
            public StaticFieldSlotFactory SlotFactory;
            public TypeGen TypeGen;

            public LanguageInfo(StaticFieldSlotFactory slotFactory, TypeGen tg) {
                TypeGen = tg;
                SlotFactory = slotFactory;
            }
        }

        public StaticFieldModuleGenerator(string moduleName, params ScriptCode[] scriptCodes)
            : base(moduleName, scriptCodes) {
        }

        #region Abstract overrides

        static AssemblyGen runtimemethods;

        protected override SlotFactory CreateSlotFactory(ScriptCode scriptCode) {
            AssemblyGen ag = null;

            if (scriptCode.SourceUnit.Kind == SourceCodeKind.Default && scriptCode.CodeBlock.Name != "ironscheme.boot.new")
            {
              if (ScriptDomainManager.Options.DebugMode)
              {
                if ((ScriptDomainManager.Options.AssemblyGenAttributes & AssemblyGenAttributes.SaveAndReloadAssemblies) != 0)
                {
                  ag = CreateModuleAssembly(scriptCode);
                }
                else
                {
                  ag = ScriptDomainManager.CurrentManager.Snippets.DebugAssembly;
                }
              }
              else
              {
                if ((ScriptDomainManager.Options.AssemblyGenAttributes & AssemblyGenAttributes.SaveAndReloadAssemblies) != 0)
                {
                  ag = CreateModuleAssembly(scriptCode);
                }
                else
                {
                  if (runtimemethods == null)
                  {
                    runtimemethods = ScriptDomainManager.CurrentManager.Snippets.Assembly;
                  }
                  ag = runtimemethods;
                }
              }
            }
            else
            {
              ag = CreateModuleAssembly(scriptCode);
            }

            ScriptDomainManager.CurrentManager.Snippets.CurrentAssembly = ag;

            TypeGen tg = GenerateModuleGlobalsType(ag, scriptCode);

            if (scriptCode.LibraryGlobals != null)
            {
              foreach (var kvp in scriptCode.LibraryGlobals)
              {
                var k = kvp.Key;
                var v = kvp.Value;

                var cg = v.Block.CreateGlobalMethodStub(tg);

                if (cg != null)
                {
                  CodeGen._codeBlockStubs[v.Block] = cg;
                  CodeGen._codeBlockLookup[k] = cg;
                }
              }
            }

            if (scriptCode.LibraryGlobalsX != null)
            {
              foreach (var kvp in scriptCode.LibraryGlobalsX)
              {
                var k = kvp.Key;
                var v = kvp.Value;

                var cg = v.Block.CreateGlobalMethodStub(tg);

                if (cg != null)
                {
                  CodeGen._codeBlockStubsX[v.Block] = cg;
                  CodeGen._codeBlockLookupX[k] = cg;
                }
              }
            }

            if (scriptCode.LibraryGlobalsN != null)
            {
              foreach (var kvp in scriptCode.LibraryGlobalsN)
              {
                var k = kvp.Key;
                var v = kvp.Value;

                var cgd = new List<CodeGenDescriptor>();

                foreach (var i in v)
                {
                  var cg = i.codeblock.Block.CreateGlobalMethodStub(tg);

                  if (cg != null)
                  {
                    CodeGen._codeBlockStubsN[i.codeblock.Block] = cg;

                    cgd.Add(new CodeGenDescriptor
                    {
                      arity = i.arity,
                      varargs = i.varargs,
                      cg = cg,
                    });
                  }
                }

                CodeGen._codeBlockLookupN[k] = cgd.ToArray();

              }
            }

            StaticFieldSlotFactory factory = new StaticFieldSlotFactory(tg);

            _languages[scriptCode.LanguageContext] = new LanguageInfo(factory, tg);

            return factory;
        }

        protected override IAttributesCollection CreateLanguageDictionary(LanguageContext context, ScopeAllocator allocator)
        {
          LanguageInfo li = _languages[context];

          // TODO: Force all dictionaries to share same object data (for multi-module)
          GlobalFieldAllocator gfa = allocator.LocalAllocator as GlobalFieldAllocator;
          if (gfa != null)
          {
            Dictionary<SymbolId, Slot> fields = gfa.SlotFactory.Fields;

            BuildDictionary(li, fields);

            //// emit methods without bodies
            //foreach (var kvp in CodeGen._codeBlockStubs)
            //{
            //}

            //foreach (var kvp in CodeGen._codeBlockStubsX)
            //{
            //}


            Type t = li.TypeGen.FinishType();
            var ass = li.TypeGen.AssemblyGen.DumpAndLoad();

            if (ModuleName == "ironscheme.boot.new")
            {
              return null;
            }

            try
            {
              return (IAttributesCollection)Activator.CreateInstance(ass.GetType(t.FullName));
            }
            catch (Exception ex)
            {
              Console.WriteLine(ex.Message);
              return null;
            }
          }
          else
          {
            throw new InvalidOperationException("invalid allocator");
          }

        }

        protected override CodeGen CreateCodeGen(ScriptCode scriptCode) {
            LanguageInfo li = _languages[scriptCode.LanguageContext];

            return li.TypeGen.DefineMethod(CompilerHelpers.PublicStatic,
                "Initialize",
                typeof(object),
                new Type[] { typeof(CodeContext) },
                null);
        }

        #endregion

        /// <summary>
        /// Creates a new assembly for generating a module, ensuring a unique filename like "filename.N.exe" for the generated assembly
        /// </summary>
        private AssemblyGen CreateModuleAssembly(ScriptCode scriptCode) {
            //scriptCode.CompilerContext.Options
            AssemblyGenAttributes genAttrs = ScriptDomainManager.Options.AssemblyGenAttributes;

            if (ScriptDomainManager.Options.DebugMode)
                genAttrs |= AssemblyGenAttributes.EmitDebugInfo;
            
            if (ScriptDomainManager.Options.DebugCodeGeneration)
                genAttrs |= AssemblyGenAttributes.DisableOptimizations;

            string outDir, fileName;
            GetCompiledSourceUnitAssemblyLocation(scriptCode.CompilerContext.SourceUnit, out outDir, out fileName);


            AssemblyGen ag;
            string ext = ".dll";

            if (scriptCode.CodeBlock.Name == "ironscheme.boot.new")
            {
              fileName = "ironscheme.boot.new";
              ext = ".dll";
            }


            // Try to create a file called "filename.<cnt>.exe", ensuring that the filename does not clash with an existing file
            int cnt = 0;
            for (; ; ) {
                try {
                    ag = new AssemblyGen(fileName, outDir, fileName + ext, genAttrs);
                    break;
                } catch (IOException) {
                    // If a file already exits with the same name, try the next name in the sequence.
                    ext = "_" + cnt.ToString() + ext;
                }
            }

            ag.SetSourceUnit(scriptCode.CompilerContext.SourceUnit);

            return ag;
        }

        private static void GetCompiledSourceUnitAssemblyLocation(SourceUnit sourceUnit, out string outDir, out string fileName) {
            outDir = ScriptDomainManager.Options.BinariesDirectory;

            if (String.IsNullOrEmpty(sourceUnit.Id)) {
                fileName = Guid.NewGuid().ToString();
                return;
            }

            string path = IOUtils.ToValidPath(sourceUnit.Id);

            if (outDir == null) {
                try {
                    outDir = Path.GetDirectoryName(path);
                } catch (PathTooLongException) {
                    outDir = null;
                }
            }

            fileName = Path.GetFileNameWithoutExtension(path);
            Debug.Assert(!String.IsNullOrEmpty(fileName));
        }

        private TypeGen GenerateModuleGlobalsType(AssemblyGen ag, ScriptCode sc)
        {
          var n = sc.CodeBlock.Name;
          switch (n)
          {
            case "visit-code":
            case "invoke-code":
            case "guard-code":
              TypeGen tg = ag.DefinePublicType(sc.CodeBlock.Name, typeof(CustomSymbolDictionary));
              tg.AddCodeContextField();
              tg.DefaultConstructor = tg.TypeBuilder.DefineDefaultConstructor(MethodAttributes.Public);
              return tg;
            default:
              return GenerateModuleGlobalsType(ag);
          }
        }

        private TypeGen GenerateModuleGlobalsType(AssemblyGen ag) {
            TypeGen tg = ag.DefinePublicType(ModuleName == "ironscheme.boot.new" ? "#" : ModuleName, typeof(CustomSymbolDictionary));
            tg.AddCodeContextField();
            tg.DefaultConstructor = tg.TypeBuilder.DefineDefaultConstructor(MethodAttributes.Public);

            return tg;
        }

        private void BuildDictionary(LanguageInfo li, Dictionary<SymbolId, Slot> fields) {
            MakeGetMethod(li, fields);
            MakeSetMethod(li, fields);
            MakeRawKeysMethod(li, fields);
            MakeInitialization(li, fields);
        }

        private static void MakeInitialization(LanguageInfo li, Dictionary<SymbolId, Slot> fields) {
            li.TypeGen.TypeBuilder.AddInterfaceImplementation(typeof(IModuleDictionaryInitialization));
            CodeGen cg = li.TypeGen.DefineExplicitInterfaceImplementation(typeof(IModuleDictionaryInitialization).GetMethod("InitializeModuleDictionary"));

            Label ok = cg.DefineLabel();
            cg.ContextSlot.EmitGet(cg);
            //cg.EmitNull();
            //cg.Emit(OpCodes.Ceq);
            cg.Emit(OpCodes.Brfalse_S, ok);
            cg.EmitReturn();
            //cg.EmitNew(typeof(InvalidOperationException), ArrayUtils.EmptyTypes);
            //cg.Emit(OpCodes.Throw);
            cg.MarkLabel(ok);

            // MyModuleDictType.ContextSlot = arg0
            cg.EmitArgGet(0);
            cg.ContextSlot.EmitSet(cg);

            foreach (KeyValuePair<SymbolId, Slot> kv in fields) {
                Slot slot = kv.Value;
                ModuleGlobalSlot builtin = slot as ModuleGlobalSlot;

                Debug.Assert(builtin != null);

                cg.EmitCodeContext();
                cg.EmitSymbolId(kv.Key);
                //cg.EmitUnbox(typeof(SymbolId));
                builtin.EmitWrapperAddr(cg);
                cg.EmitCall(typeof(RuntimeHelpers), "InitializeModuleFieldBoxed");
            }

            cg.EmitReturn();
            cg.Finish();
        }

        //
        // This generates a method like the following:
        //
        //  TryGetExtraValue(int name, object out value) {
        //      if (name1 == name) {
        //          value = type.name1Slot.RawValue;
        //          return value != Uninitialized.Instance;
        //      }
        //      if (name2 == name) {
        //          value = type.name2Slot.RawValue;
        //          return value != Uninitialized.Instance;
        //      }
        //      ...
        //      return false
        //  }

        private void MakeGetMethod(LanguageInfo li, Dictionary<SymbolId, Slot> fields) {
            CodeGen cg = li.TypeGen.DefineMethodOverride(typeof(CustomSymbolDictionary).GetMethod("TryGetExtraValue", BindingFlags.NonPublic | BindingFlags.Instance));
            //foreach (KeyValuePair<SymbolId, Slot> kv in fields) {
            //    SymbolId name = kv.Key;
            //    Slot slot = kv.Value;

            //    cg.EmitSymbolId(name);
            //    cg.EmitArgGet(0);
            //    cg.EmitCall(typeof(SymbolId), "op_Equality");

            //    Label next = cg.DefineLabel();
            //    cg.Emit(OpCodes.Brfalse_S, next);

            //    cg.EmitArgGet(1);

            //    ModuleGlobalSlot builtin = slot as ModuleGlobalSlot;
            //    Debug.Assert(builtin != null);
            //    builtin.EmitGetRaw(cg);
            //    cg.Emit(OpCodes.Stind_Ref);

            //    builtin.EmitGetRaw(cg);
            //    cg.EmitUninitialized();
            //    cg.Emit(OpCodes.Ceq);
            //    cg.Emit(OpCodes.Not);
            //    cg.EmitReturn();
            //    cg.MarkLabel(next);
            //}
            cg.EmitInt(0);
            cg.EmitReturn();
            cg.Finish();
        }

        // This generates a method like the following:
        //
        //  TrySetExtraValue(object name, object value) {
        //      if (name1 == name) {
        //          type.name1Slot = value;
        //          return 1;
        //      }
        //      if (name2 == name) {
        //          type.name2Slot = value;
        //          return 1;
        //      }
        //      ...
        //      return 0
        //  }

        private void MakeSetMethod(LanguageInfo li, Dictionary<SymbolId, Slot> fields) {
            CodeGen cg = li.TypeGen.DefineMethodOverride(typeof(CustomSymbolDictionary).GetMethod("TrySetExtraValue", BindingFlags.NonPublic | BindingFlags.Instance));
            Slot valueSlot = cg.GetArgumentSlot(1);
            //foreach (KeyValuePair<SymbolId, Slot> kv in fields) {
            //    SymbolId name = kv.Key;
            //    Slot slot = kv.Value;

            //    cg.EmitSymbolId(name);
            //    cg.EmitArgGet(0);
            //    cg.EmitCall(typeof(SymbolId), "op_Equality");

            //    Label next = cg.DefineLabel();
            //    cg.Emit(OpCodes.Brfalse_S, next);

            //    slot.EmitSet(cg, valueSlot);
            //    cg.EmitInt(1);
            //    cg.EmitReturn();
            //    cg.MarkLabel(next);
            //}
            cg.EmitInt(0);
            cg.EmitReturn();
            cg.Finish();
        }

        private CodeGen MakeRawKeysMethod(LanguageInfo li, Dictionary<SymbolId, Slot> fields) {
            Slot rawKeysCache = li.TypeGen.AddStaticField(typeof(SymbolId[]), "ExtraKeysCache");
            CodeGen init = li.TypeGen.TypeInitializer;

            init.EmitInt(0);
            init.Emit(OpCodes.Newarr, typeof(SymbolId));

            //int current = 0;
            //foreach (KeyValuePair<SymbolId, Slot> kv in fields) {
            //  Debug.Assert(current < fields.Count);
            //  init.Emit(OpCodes.Dup);
            //  init.EmitInt(current++);
            //  init.Emit(OpCodes.Ldelema, typeof(SymbolId));
            //  init.EmitSymbolIdId(kv.Key);
            //  init.Emit(OpCodes.Call, typeof(SymbolId).GetConstructor(new Type[] { typeof(int) }));
            //}

            rawKeysCache.EmitSet(init);

            CodeGen cg = li.TypeGen.DefineMethodOverride(typeof(CustomSymbolDictionary).GetMethod("GetExtraKeys", BindingFlags.Public | BindingFlags.Instance));
            rawKeysCache.EmitGet(cg);
            cg.EmitReturn();
            cg.Finish();

            return cg;
        }
    }

}
