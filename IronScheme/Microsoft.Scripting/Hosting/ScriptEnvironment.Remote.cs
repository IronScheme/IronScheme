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
#if FULL

using System;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using System.IO;
using System.Diagnostics;
using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Hosting {

    internal sealed class RemoteScriptEnvironment : RemoteWrapper, IScriptEnvironment {
        private readonly ScriptDomainManager _manager;

#region Construction

        internal RemoteScriptEnvironment(ScriptDomainManager manager) {
            Debug.Assert(manager != null);
            _manager = manager;
        }

        public override object InitializeLifetimeService() {
            return null;
        }

        /// <summary>
        /// Creates a new <see cref="RemoteScriptEnvironment"/> in a specified AppDomain unless it already exists.
        /// Returns either <c>false</c> and a remote reference to the newly created environment initialized according 
        /// to the provided setup information or <c>true</c> and the existing one ignoring the specified setup information.
        /// </summary>
        internal static bool TryCreate(AppDomain domain, ScriptEnvironmentSetup setup, out RemoteScriptEnvironment environment) {
            Contract.RequiresNotNull(domain, "domain");

            // TODO: should be retrieved later in local.trycreate
            if (setup == null) setup = new ScriptEnvironmentSetup(true);

            // prepare remote stub for the host:
            setup.RemoteHost = new RemoteScriptHost();

            bool new_created = RemoteDelegate.Invoke(domain, setup, out environment);

            if (new_created) {
                // create host local to the caller (i.e. remote to the environment):
                setup.RemoteHost.SetLocalHost(setup.CreateHostLocally(environment));
            }

            return new_created;
        }

#region RemoteDelegate

        private sealed class RemoteDelegate : MarshalByRefObject {

            public readonly RemoteScriptEnvironment Environment;
            public readonly bool NewCreated;

            public RemoteDelegate(ScriptEnvironmentSetup setup) {
                ScriptDomainManager local_environment;
                NewCreated = ScriptDomainManager.TryCreateLocal(setup, out local_environment);
                Environment = new RemoteScriptEnvironment(local_environment);
            }

            public static bool Invoke(AppDomain domain, ScriptEnvironmentSetup setup, out RemoteScriptEnvironment environment) {
                RemoteDelegate rd = (RemoteDelegate)domain.CreateInstanceAndUnwrap(typeof(RemoteDelegate).Assembly.FullName,
                    typeof(RemoteDelegate).FullName, false, BindingFlags.Default, null, new object[] { setup }, null, null, null);
                
                environment = rd.Environment;
                return rd.NewCreated;
            }
        }

        #endregion

        #endregion

        public override ILocalObject LocalObject {
            get { return _manager.Environment; }
        }
        
        public IScriptHost Host { 
            get {
                // we allow a host to be created locally but the hosting app may live in remote app-domain:
                return RemoteWrapper.WrapRemotable<IScriptHost>(_manager.Host);
            }
        }

        // TODO: remove
        public ScriptDomainOptions GlobalOptions {
            get { return _manager.GlobalOptions; }
            set { _manager.GlobalOptions = value; }
        }

        public string[] GetRegisteredFileExtensions() {
            return _manager.GetRegisteredFileExtensions();
        }

        public string[] GetRegisteredLanguageIdentifiers() {
            return _manager.GetRegisteredLanguageIdentifiers();
        }

        public ILanguageProvider GetLanguageProvider(string languageId) {
            return new RemoteLanguageProvider(_manager.GetLanguageProvider(languageId));
        }

        public ILanguageProvider GetLanguageProvider(Type type) {
            return new RemoteLanguageProvider(_manager.GetLanguageProvider(type));
        }

        public ILanguageProvider GetLanguageProviderByFileExtension(string extension) {
            return new RemoteLanguageProvider(_manager.GetLanguageProviderByFileExtension(extension));
        }
        
        public void RedirectIO(TextReader input, TextWriter output, TextWriter errorOutput) {
            _manager.Environment.RedirectIO(input, output, errorOutput);
        }

        public IScriptModule CreateModule(string name, params ICompiledCode[] compiledCodes) {
            return RemoteWrapper.WrapRemotable<IScriptModule>(_manager.Environment.CreateModule(name, compiledCodes));
        }

        public IScriptModule CreateModule(string name, ScriptModuleKind kind, IAttributesCollection dictionary, params ICompiledCode[] compiledCodes) {
            return RemoteWrapper.WrapRemotable<IScriptModule>(_manager.Environment.CreateModule(name, kind, dictionary, compiledCodes));
        }

        public IScriptModule CompileModule(string name, params SourceUnit[] sourceUnits) {
            return RemoteWrapper.WrapRemotable<IScriptModule>(_manager.Environment.CompileModule(name, sourceUnits));
        }

        public IScriptModule CompileModule(string name, ScriptModuleKind kind, CompilerOptions options, ErrorSink errorSink, IAttributesCollection dictionary, params SourceUnit[] sourceUnits) {
            return RemoteWrapper.WrapRemotable<IScriptModule>(_manager.Environment.CompileModule(name, kind, options, errorSink, dictionary, sourceUnits));
        }

        public void PublishModule(IScriptModule module) {
            _manager.Environment.PublishModule(module);
        }

        public void PublishModule(IScriptModule module, string publicName) {
            _manager.Environment.PublishModule(module, publicName);
        }

        public IDictionary<string, IScriptModule> GetPublishedModules() {
            IDictionary<string, IScriptModule> result = _manager.Environment.GetPublishedModules();

            string[] keys = new string[result.Count];
            result.Keys.CopyTo(keys, 0);

            foreach (string key in keys) {
                result[key] = RemoteWrapper.WrapRemotable<IScriptModule>(result[key]);
            }

            return result;
        }

        public Delegate GetDelegate(object callableObject, Type delegateType) {
            // TODO:
            throw new NotImplementedException();
        }
    }
}

#endif