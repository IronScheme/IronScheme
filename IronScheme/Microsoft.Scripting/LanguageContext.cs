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
using System.Text;
using System.Threading;
using System.Globalization;

using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Hosting;
using Microsoft.Scripting.Utils;
using System.IO;

namespace Microsoft.Scripting
{
    /// <summary>
    /// Provides language specific facilities which are typicalled called by the runtime.
    /// </summary>
    public abstract class LanguageContext
    {
        private static ModuleGlobalCache _noCache;

        public virtual ActionBinder Binder {
            get { return Engine.DefaultBinder; }
        }

        public virtual ScriptEngine Engine {
            get {
                return null;
            }
        }

        protected LanguageContext() {
        }

        public static LanguageContext FromEngine(IScriptEngine engine) {
            ScriptEngine localEngine = engine as ScriptEngine;
            return localEngine.LanguageContext;
        }

        #region Module Context
        
        public ModuleContext EnsureModuleContext(ScriptModule module) {
            Contract.RequiresNotNull(module, "module");
            ModuleContext context = module.GetModuleContext();
            
            if (context == null) {
                context = CreateModuleContext(module);
                if (context == null) {
                    throw new InvalidImplementationException("CreateModuleContext must return a module context.");
                }
                return module.SetModuleContext(context);
            }

            return context;
        }

        /// <summary>
        /// Notification sent when a ScriptCode is about to be executed within given ModuleContext.
        /// </summary>
        /// <param name="newContext"></param>
        public virtual void ModuleContextEntering(ModuleContext newContext) {
            // nop
        }

        /// <summary>
        /// Factory for ModuleContext creation. 
        /// It is guaranteed that this method is called once per each ScriptModule the langauge code is executed within.
        /// </summary>
        /// <param name="module">The module the context will be associated with.</param>
        /// <returns>Non-<c>null</c> module context instance.</returns>
        public virtual ModuleContext CreateModuleContext(ScriptModule module) {
            return new ModuleContext(module);
        }

        #endregion

        #region Source Code Parsing & Compilation

        /// <summary>
        /// Parses the source code within a specified compiler context. 
        /// The source unit to parse is held on by the context.
        /// </summary>
        /// <param name="context">Compiler context.</param>
        /// <returns><b>null</b> on failure.</returns>
        /// <remarks>Could also set the code properties and line/file mappings on the source unit.</remarks>
        public abstract CodeBlock ParseSourceCode(CompilerContext context);

        /// <summary>
        /// Updates code properties of the specified source unit. 
        /// The default implementation invokes code parsing. 
        /// </summary>
        public virtual void UpdateSourceCodeProperties(CompilerContext context) {
            Contract.RequiresNotNull(context, "context");

            CodeBlock block = ParseSourceCode(context);

            if (!context.SourceUnit.CodeProperties.HasValue) {
                context.SourceUnit.CodeProperties = (block != null) ? SourceCodeProperties.None : SourceCodeProperties.IsInvalid;
            }
        }
        
        public ScriptCode CompileSourceCode(SourceUnit sourceUnit) {
            return CompileSourceCode(sourceUnit, null);
        }

      public ScriptCode CompileSourceCode(CodeBlock block)
      {
        CompilerContext context = new CompilerContext(SourceUnit.CreateSnippet(Engine, string.Empty));
        AnalyzeBlock(block);
        return new ScriptCode(block, Engine.GetLanguageContext(), context);
      }

      public ScriptCode CompileSourceCode(CodeBlock block, string sourcefile)
      {
        CompilerContext context = new CompilerContext(SourceUnit.CreateFileUnit(Engine, sourcefile));
        AnalyzeBlock(block);
        return new ScriptCode(block, Engine.GetLanguageContext(), context);
      }

        public ScriptCode CompileSourceCode(SourceUnit sourceUnit, ErrorSink errorSink) {
            Contract.RequiresNotNull(sourceUnit, "sourceUnit");

            if (errorSink == null) errorSink = Engine.GetCompilerErrorSink();

            CompilerContext context = new CompilerContext(sourceUnit, errorSink);

            CodeBlock block = ParseSourceCode(context);

            if (block == null) {
                throw new SyntaxErrorException("invalid syntax|" + sourceUnit.GetCode().Trim());
            }

            AnalyzeBlock(block);

            // TODO: ParseSourceCode can update CompilerContext.Options
            return new ScriptCode(block, Engine.GetLanguageContext(), context);
        }

        public static void AnalyzeBlock(CodeBlock block)
        {
          ClosureBinder.Bind(block);
            FlowChecker.Check(block);
        }

        public virtual StreamReader GetSourceReader(Stream stream, Encoding defaultEncoding) {
            return new StreamReader(stream, defaultEncoding);
        }

        #endregion

        /// <summary>
        /// Looks up the name in the provided Scope using the current language's semantics.
        /// </summary>
        public virtual bool TryLookupName(CodeContext context, SymbolId name, out object value) {
            if (context.Scope.TryLookupName(this, name, out value)) {
                return true;
            }
            
            return TryLookupGlobal(context, name, out value);
        }

        /// <summary>
        /// Looks up the name in the provided scope using the current language's semantics.
        /// 
        /// If the name cannot be found throws the language appropriate exception or returns
        /// the language's appropriate default value.
        /// </summary>
        public virtual object LookupName(CodeContext context, SymbolId name) {
            object value;
            if (!TryLookupName(context, name, out value) || value == Uninitialized.Instance) {
                throw MissingName(name);
            }

            return value;
        }

        /// <summary>
        /// Attempts to set the name in the provided scope using the current language's semantics.
        /// </summary>
        public virtual void SetName(CodeContext context, SymbolId name, object value) {
            context.Scope.SetName(name, value);
        }

        /// <summary>
        /// Attemps to lookup a global variable using the language's semantics called from
        /// the provided Scope.  The default implementation will attempt to lookup the variable
        /// at the host level.
        /// </summary>
        public virtual bool TryLookupGlobal(CodeContext context, SymbolId name, out object value) {
            return ScriptDomainManager.CurrentManager.Host.TryGetVariable(Engine, name, out value);
        }

        /// <summary>
        /// Called when a lookup has failed and an exception should be thrown.  Enables the 
        /// language context to throw the appropriate exception for their language when
        /// name lookup fails.
        /// </summary>
        protected internal virtual Exception MissingName(SymbolId name) {
            return new MissingMemberException(String.Format(CultureInfo.CurrentCulture, Resources.NameNotDefined, SymbolTable.IdToString(name)));
        }

        /// <summary>
        /// Returns a ModuleGlobalCache for the given name.  
        /// 
        /// This cache enables fast access to global values when a SymbolId is not defined after searching the Scope's.  Usually
        /// a language implements lookup of the global value via TryLookupGlobal.  When GetModuleCache returns a ModuleGlobalCache
        /// a cached value can be used instead of calling TryLookupGlobal avoiding a possibly more expensive lookup from the 
        /// LanguageContext.  The ModuleGlobalCache can be held onto and have its value updated when the cache is invalidated.
        /// 
        /// By default this returns a cache which indicates no caching should occur and the LanguageContext will be 
        /// consulted when a module value is not available. If a LanguageContext only caches some values it can return 
        /// the value from the base method when the value should not be cached.
        /// </summary>
        protected internal virtual ModuleGlobalCache GetModuleCache(SymbolId name) {
            if (_noCache == null) {
                Interlocked.CompareExchange<ModuleGlobalCache>(ref _noCache, new ModuleGlobalCache(ModuleGlobalCache.NotCaching), null);
            }

            return _noCache;
        }

        public virtual bool IsTrue(object obj) {
            return false;
        }

        /// <summary>
        /// Gets the value or throws an exception when the provided MethodCandidate cannot be called.
        /// </summary>
        /// <returns></returns>
        public virtual object GetNotImplemented(params MethodCandidate []candidates) {
            throw new MissingMemberException("the specified operator is not implemented");
        }
    }
}
