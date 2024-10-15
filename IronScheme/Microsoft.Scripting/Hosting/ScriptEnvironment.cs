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
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using Microsoft.Scripting;
using System.IO;
using System.Diagnostics;
using Microsoft.Scripting.Generation;
using System.Text;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Hosting {

    public interface IScriptEnvironment {
        IScriptHost Host { get; }
        // language providers (TODO: register):
        string[] GetRegisteredFileExtensions();
        string[] GetRegisteredLanguageIdentifiers();
        ILanguageProvider GetLanguageProvider(string languageId);
        ILanguageProvider GetLanguageProvider(Type languageProviderType);
        ILanguageProvider GetLanguageProviderByFileExtension(string extension);
        
        // modules:
        IScriptModule CreateModule(string name, params ICompiledCode[] compiledCodes);
        IScriptModule CreateModule(string name, ScriptModuleKind kind, IAttributesCollection dictionary, params ICompiledCode[] compiledCodes);

        IScriptModule CompileModule(string name, params SourceUnit[] sourceUnits);
        IScriptModule CompileModule(string name, ScriptModuleKind kind, ErrorSink errorSink, IAttributesCollection dictionary, params SourceUnit[] sourceUnits);
        

        //void PublishModule(IScriptModule module);
        //void PublishModule(IScriptModule module, string publicName);
        //IDictionary<string, IScriptModule> GetPublishedModules();
        
        Delegate GetDelegate(object callableObject, Type delegateType);
        
        // TODO:
        // Delegate CreateDelegate(IObjectHandle remoteCallableObject, Type delegateType);

        // TODO: remove
        ScriptDomainOptions GlobalOptions { get; set; }
    }

    public sealed class ScriptEnvironment : IScriptEnvironment {

        private readonly ScriptDomainManager _manager;
        
        public IScriptHost Host {
            get { return _manager.Host; }
        }

        // TODO: remove
        public ScriptDomainOptions GlobalOptions {
            get { return _manager.GlobalOptions; }
            set { _manager.GlobalOptions = value; }
        }

        internal ScriptEnvironment(ScriptDomainManager manager) {
            Debug.Assert(manager != null);
            _manager = manager;
        }

        public static IScriptEnvironment Create() {
            ScriptDomainManager manager;
            if (!ScriptDomainManager.TryCreateLocal(out manager))
                throw new InvalidOperationException("Environment already created in the current AppDomain");

            return manager.Environment;
        }

        public static IScriptEnvironment GetEnvironment() {
            return ScriptDomainManager.CurrentManager.Environment;
        }

        public string[] GetRegisteredFileExtensions() {
            return _manager.GetRegisteredFileExtensions();
        }

        public string[] GetRegisteredLanguageIdentifiers() {
            return _manager.GetRegisteredLanguageIdentifiers();
        }

        /// <exception cref="ArgumentNullException"><paramref name="type"/></exception>
        /// <exception cref="ArgumentException"><paramref name="type"/></exception>
        /// <exception cref="MissingTypeException"><paramref name="languageId"/></exception>
        /// <exception cref="InvalidImplementationException">The language provider's implementation failed to instantiate.</exception>
        public ILanguageProvider GetLanguageProvider(string languageId) {
            return _manager.GetLanguageProvider(languageId);
        }

        public ILanguageProvider GetLanguageProvider(Type languageProviderType) {
            return _manager.GetLanguageProvider(languageProviderType);
        }

        public ILanguageProvider GetLanguageProviderByFileExtension(string extension) {
            return _manager.GetLanguageProviderByFileExtension(extension);
        }

        #region Compilation, Module Creation

        public IScriptModule CreateModule(string name, params ICompiledCode[] compiledCodes) {
            return CreateModule(name, ScriptModuleKind.Default, null, compiledCodes);
        }

        /// <summary>
        /// Creates a module.
        /// <c>dictionary</c> can be <c>null</c>
        /// </summary>
        /// <returns></returns>
        public IScriptModule CreateModule(string name, ScriptModuleKind kind, IAttributesCollection dictionary, params ICompiledCode[] compiledCodes) {
            Contract.RequiresNotNullItems(compiledCodes, "compiledCodes");

            ScriptCode[] script_codes = new ScriptCode[compiledCodes.Length];
            for (int i = 0; i < compiledCodes.Length; i++) {
                script_codes[i] = ScriptCode.FromCompiledCode(compiledCodes[i] as CompiledCode);
                if (script_codes[i] == null) {
                    throw new ArgumentException(Resources.RemoteCodeModuleComposition, String.Format("{0}[{1}]", "compiledCodes", i));
                }
            }

            return _manager.CreateModule(name, kind, new Scope(dictionary), script_codes);
        }

        public IScriptModule CompileModule(string name, params SourceUnit[] sourceUnits) {
            return CompileModule(name, ScriptModuleKind.Default, null, null, sourceUnits);
        }

        /// <summary>
        /// Compiles a list of source units into a single module.
        /// <c>options</c> can be <c>null</c>
        /// <c>errroSink</c> can be <c>null</c>
        /// <c>dictionary</c> can be <c>null</c>
        /// </summary>
        public IScriptModule CompileModule(string name, ScriptModuleKind kind, ErrorSink errorSink, 
            IAttributesCollection dictionary, params SourceUnit[] sourceUnits) {

            return _manager.CompileModule(name, kind, new Scope(dictionary), errorSink, sourceUnits);
        }

        #endregion

        #region Variables // TODO: remove

        public IAttributesCollection Variables { 
            get { return _manager.Variables; } 
            set { _manager.Variables = value; } 
        }

        public void SetVariables(IAttributesCollection dictionary) {
            Contract.RequiresNotNull(dictionary, "dictionary");
            _manager.Variables = dictionary;
        }

        public object GetVariable(CodeContext context, SymbolId name) {
            return _manager.GetVariable(context, name);
        }

        public void SetVariable(CodeContext context, SymbolId name, object value) {
            _manager.SetVariable(context, name, value);
        }

        #endregion

        #region Object Operations

        public Delegate GetDelegate(object callableObject, Type delegateType) {
            return RuntimeHelpers.GetDelegate(callableObject, delegateType);            
        }

        #endregion
    }
}
