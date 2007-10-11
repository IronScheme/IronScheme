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
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using Microsoft.Scripting;
using System.IO;
using System.Diagnostics;
using Microsoft.Scripting.Generation;
using System.Text;
using System.Runtime.Remoting;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Hosting {

    public interface IScriptEnvironment : IRemotable {
        IScriptHost Host { get; }
        
        // convenience API:
#if !SILVERLIGHT
        void RedirectIO(TextReader input, TextWriter output, TextWriter errorOutput);
#endif

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
        IScriptModule CompileModule(string name, ScriptModuleKind kind, CompilerOptions options, ErrorSink errorSink, IAttributesCollection dictionary, params SourceUnit[] sourceUnits);
        

        void PublishModule(IScriptModule module);
        void PublishModule(IScriptModule module, string publicName);
        IDictionary<string, IScriptModule> GetPublishedModules();
        
        // TODO: remove exceptionHandler parameter - just a SL hack
        Delegate GetDelegate(object callableObject, Type delegateType, Action<Exception> exceptionHandler);
        
        // TODO:
        // Delegate CreateDelegate(IObjectHandle remoteCallableObject, Type delegateType);

        // TODO: remove
        ScriptDomainOptions GlobalOptions { get; set; }
    }

    public sealed class ScriptEnvironment : IScriptEnvironment, ILocalObject {

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

        public static IScriptEnvironment Create(ScriptEnvironmentSetup setup) {
            ScriptDomainManager manager;
            if (!ScriptDomainManager.TryCreateLocal(setup, out manager))
                throw new InvalidOperationException("Environment already created in the current AppDomain");

            return manager.Environment;
        }

        public static IScriptEnvironment GetEnvironment() {
            return ScriptDomainManager.CurrentManager.Environment;
        }

#if !SILVERLIGHT
        RemoteWrapper ILocalObject.Wrap() {
            return new RemoteScriptEnvironment(_manager);
        }
        
        public static IScriptEnvironment Create() {
            return Create(null);
        }

        public static IScriptEnvironment Create(ScriptEnvironmentSetup setup, AppDomain domain) {
            Contract.RequiresNotNull(domain, "domain");

            if (domain == AppDomain.CurrentDomain) {
                return Create(setup);
            }

            RemoteScriptEnvironment rse;
            if (!RemoteScriptEnvironment.TryCreate(domain, setup, out rse))
                throw new InvalidOperationException("Environment already created in the specified AppDomain");

            return rse;
        }

        public static IScriptEnvironment GetEnvironment(AppDomain domain) {
            Contract.RequiresNotNull(domain, "domain");

            if (domain == AppDomain.CurrentDomain) {
                return GetEnvironment();
            }

            // TODO:
            throw new NotImplementedException("TODO");
        }
#endif
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
        /// </summary>
        /// <c>dictionary</c> can be <c>null</c>
        /// <returns></returns>
        public IScriptModule CreateModule(string name, ScriptModuleKind kind, IAttributesCollection dictionary, params ICompiledCode[] compiledCodes) {
            Contract.RequiresNotNullItems(compiledCodes, "compiledCodes");

            ScriptCode[] script_codes = new ScriptCode[compiledCodes.Length];
            for (int i = 0; i < compiledCodes.Length; i++) {
                script_codes[i] = ScriptCode.FromCompiledCode(RemoteWrapper.TryGetLocal<CompiledCode>(compiledCodes[i]));
                if (script_codes[i] == null) {
                    throw new ArgumentException(Resources.RemoteCodeModuleComposition, String.Format("{0}[{1}]", "compiledCodes", i));
                }
            }

            return _manager.CreateModule(name, kind, new Scope(dictionary), script_codes);
        }

        public IScriptModule CompileModule(string name, params SourceUnit[] sourceUnits) {
            return CompileModule(name, ScriptModuleKind.Default, null, null, null, sourceUnits);
        }

        /// <summary>
        /// Compiles a list of source units into a single module.
        /// <c>options</c> can be <c>null</c>
        /// <c>errroSink</c> can be <c>null</c>
        /// <c>dictionary</c> can be <c>null</c>
        /// </summary>
        public IScriptModule CompileModule(string name, ScriptModuleKind kind, CompilerOptions options, ErrorSink errorSink, 
            IAttributesCollection dictionary, params SourceUnit[] sourceUnits) {

            return _manager.CompileModule(name, kind, new Scope(dictionary), options, errorSink, sourceUnits);
        }

        public void PublishModule(IScriptModule module) {
            _manager.PublishModule(RemoteWrapper.GetLocalArgument<ScriptModule>(module, "module"));
        }

        public void PublishModule(IScriptModule module, string publicName) {
            _manager.PublishModule(RemoteWrapper.GetLocalArgument<ScriptModule>(module, "module"), publicName);
        }

        public IDictionary<string, IScriptModule> GetPublishedModules() {
            IDictionary<string, ScriptModule> local_modules = _manager.GetPublishedModules();

            IDictionary<string, IScriptModule> result = new Dictionary<string, IScriptModule>(local_modules.Count);
            foreach (KeyValuePair<string, ScriptModule> local_module in local_modules) {
                result.Add(local_module.Key, local_module.Value);
            }

            return result;
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

        // TODO: remove exceptionHandler param (Silverlight hack):
        public Delegate GetDelegate(object callableObject, Type delegateType, Action<Exception> exceptionHandler) {
            return DynamicHelpers.GetDelegate(callableObject, delegateType, exceptionHandler);            
        }

        #endregion

        #region Convenience API (not available for Silverlight to make the assembly smaller)

#if !SILVERLIGHT

        public void RedirectIO(TextReader input, TextWriter output, TextWriter errorOutput) {
            if (input != null) Console.SetIn(input);
            if (output != null) Console.SetOut(output);
            if (errorOutput != null) Console.SetError(errorOutput);
        }
#endif
        
        #endregion
    }
}
