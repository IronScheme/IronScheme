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
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using System.Diagnostics;
using System.Collections;
using System.IO;
using System.Threading;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Hosting {

    public interface IScriptHost {
        // virtual file-system ops:
        string NormalizePath(string path);  // throws ArgumentException
        string[] GetSourceFileNames(string mask, string searchPattern);
        
        // source units:
        SourceUnit TryGetSourceFileUnit(IScriptEngine engine, string path, Encoding encoding);
        SourceUnit ResolveSourceFileUnit(string name);

        // notifications:
        void EngineCreated(IScriptEngine engine);
        void ModuleCreated(IScriptModule module);

        // environment variables:
        bool TrySetVariable(IScriptEngine engine, SymbolId name, object value);
        bool TryGetVariable(IScriptEngine engine, SymbolId name, out object value);
        
        /// <summary>
        /// Default module is provided by the host.
        /// For some hosting scenarios, the default module is not necessary so the host needn't to implement this method. 
        /// The default module should be created lazily as the environment is not prepared for module creation at the time the 
        /// host tis created (the host is created prior module creation so that it could be notified about the creation).
        /// </summary>
        IScriptModule DefaultModule { get; } // throws InvalidOperationException if no default module is available

        /// <summary>
        /// Provides the host with a mechanism for catching exceptions thrown by
        /// user code in event handlers.
        /// TODO: this is a workaround for Silverlight, this will be removed at
        /// some point in the future.
        /// </summary>
        Action<Exception> EventExceptionHandler { get; }
    }

    public class ScriptHost : IScriptHost {

        /// <summary>
        /// The environment the host is attached to.
        /// </summary>
        private IScriptEnvironment _environment;
        private ScriptModule _defaultModule;

        /// <summary>
        /// Default module for convenience. Lazily init'd.
        /// </summary>
        public virtual IScriptModule DefaultModule {
            get {
                if (_defaultModule == null) {
                    CreateDefaultModule(ref _defaultModule);
                 }

                return _defaultModule;
            }
        }
        
        #region Construction

        public ScriptHost(IScriptEnvironment environment) {
            Contract.RequiresNotNull(environment, "environment");
            _environment = environment;
            _defaultModule = null;
        }

        internal ScriptHost() {
            _environment = null;
        }

        #endregion

        static internal void CreateDefaultModule(ref ScriptModule defaultModule) {
           // create a module and throw it away if there is already one:
            ScriptModule module = ScriptDomainManager.CurrentManager.CreateModule("<default>", null, ScriptCode.EmptyArray);
            Interlocked.CompareExchange<ScriptModule>(ref defaultModule, module, null);
        }

        public virtual Action<Exception> EventExceptionHandler {
            get { return null; }
        }

        #region Virtual File System

        /// <summary>
        /// Normalizes a specified path.
        /// </summary>
        /// <param name="path">Path to normalize.</param>
        /// <returns>Normalized path.</returns>
        /// <exception cref="ArgumentException"><paramref name="path"/> is not a valid path.</exception>
        /// <remarks>
        /// Normalization should be idempotent, i.e. NormalizePath(NormalizePath(path)) == NormalizePath(path) for any valid path.
        /// </remarks>
        public virtual string NormalizePath(string path) {
            Contract.RequiresNotNull(path, "path");
            return (path.Length > 0) ? ScriptDomainManager.CurrentManager.PAL.GetFullPath(path) : "";
        }

        public virtual string[] GetSourceFileNames(string mask, string searchPattern) {
            return ScriptDomainManager.CurrentManager.PAL.GetFiles(mask, searchPattern);
        }

        #endregion

        #region Source File Units Resolving and Creation

        public const string PathEnvironmentVariableName = "DLRPATH";
        
        /// <summary>
        /// Gets the default path used for searching for source units.
        /// </summary>
        internal protected virtual IList<string> SourceUnitResolutionPath {
            get {
#if SILVERLIGHT
                return new string[] { "." };
#else
                return (System.Environment.GetEnvironmentVariable(PathEnvironmentVariableName) ?? ".").Split(Path.PathSeparator);
#endif
            }
        }

        public virtual SourceUnit TryGetSourceFileUnit(IScriptEngine engine, string path, Encoding encoding) {
            Contract.RequiresNotNull(engine, "engine");
            Contract.RequiresNotNull(path, "path");
            
            if (ScriptDomainManager.CurrentManager.PAL.FileExists(path)) {
                return SourceUnit.CreateFileUnit(engine, path, encoding);
            }

            return null;
        }

        /// <summary>
        /// Loads the module of the given name using the host provided semantics.
        /// 
        /// The default semantics are to search the host path for a file of the specified
        /// name.
        /// </summary>
        /// <param name="name"></param>
        /// <returns>A valid SourceUnit or null no module could be found.</returns>
        /// <exception cref="System.InvalidOperationException">An ambigious module match has occured</exception>
        public virtual SourceUnit ResolveSourceFileUnit(string name) {
            Contract.RequiresNotNull(name, "name");

            SourceUnit result = null;

            foreach (string directory in SourceUnitResolutionPath) {

                string finalPath = null;

                foreach (string extension in _environment.GetRegisteredFileExtensions()) {
                    string fullPath = Path.Combine(directory, name + extension);

                    if (ScriptDomainManager.CurrentManager.PAL.FileExists(fullPath)) {
                        if (result != null) {
                            throw new InvalidOperationException(String.Format(Resources.AmbigiousModule, fullPath, finalPath));
                        }

                        LanguageProvider provider;
                        if (!ScriptDomainManager.CurrentManager.TryGetLanguageProviderByFileExtension(extension, out provider)) {
                            // provider may have been unregistered, let's pick another one: 
                            continue;    
                        }

                        result = SourceUnit.CreateFileUnit(provider.GetEngine(), NormalizePath(fullPath), Encoding.Default);
                        finalPath = fullPath;
                    }
                }
            }

            return result;
        }

        #endregion

        #region Notifications

        public virtual void EngineCreated(IScriptEngine engine) {
            // nop
        }

        public virtual void ModuleCreated(IScriptModule module) {
            // nop
        }

        #endregion

        #region Variables

        public virtual bool TrySetVariable(IScriptEngine engine, SymbolId name, object value) {
            return false;
        }

        public virtual bool TryGetVariable(IScriptEngine engine, SymbolId name, out object value) {
            value = null;
            return false;
        }

        #endregion
    }

}
