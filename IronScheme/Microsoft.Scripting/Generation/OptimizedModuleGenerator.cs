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
using System.IO;
using Microsoft.Scripting.Utils;
using Microsoft.Scripting.Hosting;
using Microsoft.Scripting.Generation.Allocators;
using Microsoft.Scripting.Generation.Factories;
using Microsoft.Scripting.Generation.Slots;

namespace Microsoft.Scripting.Generation
{
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

            return new StaticFieldModuleGenerator(moduleName, scriptCodes);
        }

        protected static void GetCompiledSourceUnitAssemblyLocation(string suid, out string outDir, out string fileName)
        {
          outDir = ScriptDomainManager.Options.BinariesDirectory;

          if (String.IsNullOrEmpty(suid))
          {
            fileName = Guid.NewGuid().ToString();
            return;
          }

          string path = IOUtils.ToValidPath(suid);

          if (outDir == null)
          {
            try
            {
              outDir = Path.GetDirectoryName(path);
            }
            catch (PathTooLongException)
            {
              outDir = null;
            }
          }

          fileName = Path.GetFileNameWithoutExtension(path);
          Debug.Assert(!String.IsNullOrEmpty(fileName));
        }

        public static AssemblyGen CreateModuleAssembly(string suid)
        {
          AssemblyGenAttributes genAttrs = ScriptDomainManager.Options.AssemblyGenAttributes;

          if (ScriptDomainManager.Options.DebugMode)
            genAttrs |= AssemblyGenAttributes.EmitDebugInfo;

          if (ScriptDomainManager.Options.DebugCodeGeneration)
            genAttrs |= AssemblyGenAttributes.DisableOptimizations;

          string outDir, fileName;
          GetCompiledSourceUnitAssemblyLocation(suid, out outDir, out fileName);

          AssemblyGen ag;
          string ext = ".dll";

          // Try to create a file called "filename.<cnt>.exe", ensuring that the filename does not clash with an existing file
          int cnt = 0;
          for (; ; )
          {
            try
            {
              ag = new AssemblyGen(fileName, outDir, fileName + ext, genAttrs);
              if (ScriptDomainManager.Options.DebugMode)
              {
                ag.CreateSymWriter();
              }
              break;
            }
            catch (FileNotFoundException)
            {
              throw;
            }
            catch (IOException)
            {
              // If a file already exits with the same name, try the next name in the sequence.
              ext = "_" + cnt.ToString() + ext;
            }
          }

          return ag;
        }

        /// <summary>
        /// Creates the methods and optimized Scope's which get associated with each ScriptCode.
        /// </summary>
        public Scope GenerateScope() {
            List<CodeGen> cgs = GenerateScriptMethods();
            List<Scope> scopes = GenerateScriptScopes();

            if (ModuleName == "ironscheme.boot.new")
            {
                return scopes[0];
            }

            Debug.Assert(cgs.Count == scopes.Count);
            Debug.Assert(scopes.Count == _scriptCodes.Length);
            Debug.Assert(_codeContexts.Count == _scriptCodes.Length);

            List<CallTargetWithContext0> targets = new List<CallTargetWithContext0>();
            foreach (CodeGen cg in cgs) {
                targets.Add((CallTargetWithContext0)cg.CreateDelegate(typeof(CallTargetWithContext0)));
            }

            // everything succeeded, commit the results
            for (int i = 0; i < _scriptCodes.Length; i++) {
                ScriptCode sc = _scriptCodes[i];
                var dict = scopes[i].Dict;
                if (dict is SymbolDictionary)
                {
                  sc.OptimizedTarget = targets[i];
                }
                else
                {
                  sc.OptimizedTarget = (CallTargetWithContext0) Delegate.CreateDelegate(typeof(CallTargetWithContext0), dict.GetType().GetMethod("Initialize"));// targets[i];
                }
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
                  cg.Emit(OpCodes.Brtrue_S, ok);

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
                var sf = CreateSlotFactory(scriptCode) as StaticFieldSlotFactory;
                var mgf = new ModuleGlobalFactory(sf);
                var sf2 = new StaticFieldSlotFactory(sf.TypeGen);
                GlobalFieldAllocator gfa = new GlobalFieldAllocator(mgf);
                var gfa2 = new GlobalFieldAllocator(sf2);

                // Locals and globals are allocated from the same namespace for optimized modules
                ScopeAllocator global = new ScopeAllocator(null, gfa);
                allocator = new ScopeAllocator(global, gfa2);

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

    class StaticFieldModuleGenerator : OptimizedModuleGenerator {
        //private static int _Counter;
        private Dictionary<LanguageContext, LanguageInfo> _languages = new Dictionary<LanguageContext, LanguageInfo>();

        private class LanguageInfo {
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

        protected override SlotFactory CreateSlotFactory(ScriptCode scriptCode) {
            AssemblyGen ag = null;

            if (scriptCode.SourceUnit.Kind == SourceCodeKind.Default && scriptCode.CodeBlock.Name != "ironscheme.boot.new")
            {
              if (ScriptDomainManager.Options.DebugMode)
              {
                if ((ScriptDomainManager.Options.AssemblyGenAttributes & AssemblyGenAttributes.SaveAndReloadAssemblies) != 0)
                {
                  ag = ScriptDomainManager.CurrentManager.Snippets.CurrentAssembly;
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
                  ag = ScriptDomainManager.CurrentManager.Snippets.CurrentAssembly;
                }
                else
                {
                  ag = ScriptDomainManager.CurrentManager.Snippets.Assembly;
                }
              }
            }
            else
            {
              ag = ScriptDomainManager.CurrentManager.Snippets.CurrentAssembly; 
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
          GlobalFieldAllocator gfa = allocator.GlobalAllocator as GlobalFieldAllocator;
          if (gfa != null)
          {
            Dictionary<SymbolId, Slot> fields = gfa.SlotFactory.Fields;

            BuildDictionary(li, fields);

            Type t = li.TypeGen.FinishType();
            var ass = li.TypeGen.AssemblyGen.DumpAndLoad();

            if (ModuleName == "ironscheme.boot.new")
            {
              return null;
            }

            try
            {
              var rt = ass.GetType(t.FullName);
              if (rt == null)
              {
                Console.WriteLine("Type: '{0}' not found in '{1}' (mono bug)", t.FullName, ass.FullName);
                return null;
              }

              li.TypeGen.BakedType = rt;
              return (IAttributesCollection)Activator.CreateInstance(rt);
            }
            catch (Exception ex)
            {
              Console.WriteLine("Failed to create language dictionary: {0}", ex.Message);
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

        private TypeGen GenerateModuleGlobalsType(AssemblyGen ag, ScriptCode sc)
        {
          var n = sc.CodeBlock.Name;
          switch (n)
          {
            case "visit-code":
            case "invoke-code":
            case "guard-code":
              TypeGen tg = ag.DefinePublicType("syntax-" + n, typeof(CustomSymbolDictionary));
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
            MakeInitialization(li, fields);
        }

        private static void MakeInitialization(LanguageInfo li, Dictionary<SymbolId, Slot> fields) {
            li.TypeGen.TypeBuilder.AddInterfaceImplementation(typeof(IModuleDictionaryInitialization));
            CodeGen cg = li.TypeGen.DefineExplicitInterfaceImplementation(typeof(IModuleDictionaryInitialization).GetMethod("InitializeModuleDictionary"));

            Label ok = cg.DefineLabel();
            cg.ContextSlot.EmitGet(cg);
            cg.Emit(OpCodes.Brfalse_S, ok);
            cg.EmitReturn();
            cg.MarkLabel(ok);

            cg.EmitArgGet(0);
            cg.ContextSlot.EmitSet(cg);

            foreach (KeyValuePair<SymbolId, Slot> kv in fields) {
                Slot slot = kv.Value;
                ModuleGlobalSlot builtin = slot as ModuleGlobalSlot;

                if (builtin != null)
                {
                  cg.EmitCodeContext();
                  cg.EmitSymbolId(kv.Key);
                  builtin.EmitWrapperAddr(cg);
                  cg.EmitCall(typeof(RuntimeHelpers), nameof(RuntimeHelpers.InitializeModuleFieldBoxed));
                }

                StaticFieldSlot sfs = slot as StaticFieldSlot;
                if (sfs != null)
                {
                  cg.EmitCodeContext();
                  cg.EmitSymbolId(kv.Key);
                  sfs.EmitGetAddr(cg);
                  cg.EmitCall(typeof(RuntimeHelpers).GetMethod(nameof(RuntimeHelpers.InitializeFieldBoxed)).MakeGenericMethod( sfs.Type ));
                }
            }

            cg.EmitReturn();
            cg.Finish();
        }
    }

}
