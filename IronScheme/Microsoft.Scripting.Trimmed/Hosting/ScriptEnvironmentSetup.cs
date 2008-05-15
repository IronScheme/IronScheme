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
using System.Configuration;
using System.Diagnostics;
using System.Reflection;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Hosting {

    [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1815:OverrideEqualsAndOperatorEqualsOnValueTypes")] // TODO: fix
    [Serializable]
    public struct LanguageProviderSetup {
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2105:ArrayFieldsShouldNotBeReadOnly")] // TODO: fix
        public readonly string[] Names;
        public readonly string Assembly;
        public readonly string Type;

        public LanguageProviderSetup(string type, string assembly, params string[] names) {
            this.Names = names;
            this.Assembly = assembly;
            this.Type = type;
        }
    }

    [Serializable]
    public class ScriptEnvironmentSetup {

        private LanguageProviderSetup[] _languageProviders;
        private Type _hostType;
        private Type _palType;

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1819:PropertiesShouldNotReturnArrays")] // TODO: fix
        public LanguageProviderSetup[] LanguageProviders {
            get { 
                return _languageProviders; 
            }
            set {
                Contract.RequiresNotNull(value, "value");
                _languageProviders = value; 
            }
        }

        public Type PALType {
            get { 
                return _palType; 
            }
            set {
                Contract.RequiresNotNull(value, "value");
                if (!value.IsSubclassOf(typeof(PlatformAdaptationLayer))) throw new ArgumentException("Invalid type", "value");
                _palType = value; 
            }
        }

        public Type HostType {
            get { 
                return _hostType; 
            }
            set {
                Contract.RequiresNotNull(value, "value");
                if (!value.IsSubclassOf(typeof(ScriptHost))) throw new ArgumentException("Invalid type", "value");
                _hostType = value;
            }
        }

        private static string AppDomainDataKey {
            get {
                return typeof(ScriptEnvironmentSetup).FullName;
            }
        }

#if !SILVERLIGHT
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2235:MarkAllNonSerializableFields")] // MBRO
        private RemoteScriptHost _remoteHost;

        private Type _localHostType;
        
        internal RemoteScriptHost RemoteHost {
            get {
                return _remoteHost;
            }
            set {
                _remoteHost = value;
            }
        }

        public Type LocalHostType {
            get {
                return _localHostType;
            }
            set {
                Contract.RequiresNotNull(value, "value");
                if (!value.IsSubclassOf(typeof(LocalScriptHost))) throw new ArgumentException("Invalid type", "value");
                _localHostType = value;
            }
        }

        public void AssociateWithAppDomain(AppDomain domain) {
            Contract.RequiresNotNull(domain, "domain");
            domain.SetData(AppDomainDataKey, this);
        }

        public static ScriptEnvironmentSetup GetAppDomainAssociated(AppDomain domain) {
            Contract.RequiresNotNull(domain, "domain");
            return domain.GetData(AppDomainDataKey) as ScriptEnvironmentSetup;
        }

        /// <exception cref="InvalidImplementationException">The local host type has no constructor compatible with specified args.</exception>
        private LocalScriptHost CreateLocalHost(RemoteScriptHost remoteHost) {
            LocalScriptHost result = ReflectionUtils.CreateInstance<LocalScriptHost>(_localHostType);
            result.SetRemoteHost(remoteHost);
            return result;

        }
#endif

        public ScriptEnvironmentSetup() 
            : this(false) {
        }

        public ScriptEnvironmentSetup(bool addWellKnownLanguages) {
            if (addWellKnownLanguages) {
                _languageProviders = new LanguageProviderSetup[] {
#if SIGNED
                    new LanguageProviderSetup("IronPython.Hosting.PythonLanguageProvider", "IronPython, Version=2.0.0.500, Culture=neutral, PublicKeyToken=31bf3856ad364e35", ".py", "py", "python", "ironpython"),
                    new LanguageProviderSetup("Microsoft.JScript.Compiler.Hosting.LanguageProvider", "Microsoft.JScript.Compiler, Version=1.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35", ".jsx", ".js", "managedjscript", "js", "jscript"),
                    new LanguageProviderSetup("Ruby.Hosting.RubyLanguageProvider", "IronRuby, Version=1.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35", ".rb", "rb", "ruby", "ironruby"),
                    new LanguageProviderSetup("Microsoft.VisualBasic.Scripting.Hosting.VisualBasicLanguageProvider", "Microsoft.VisualBasic.Scripting, Version=1.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35", ".vbx", "vbx"),
                    new LanguageProviderSetup("ToyScript.ToyLanguageProvider", "ToyScript, Version=1.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35", ".ts", "ts", "toyscript", "toyscript"),
#else
                    new LanguageProviderSetup("IronPython.Hosting.PythonLanguageProvider", "IronPython", ".py", "py", "python", "ironpython"),
                    new LanguageProviderSetup("Microsoft.JScript.Compiler.Hosting.LanguageProvider", "Microsoft.JScript.Compiler", ".jsx", ".js", "managedjscript", "js", "jscript"),
                    new LanguageProviderSetup("Microsoft.VisualBasic.Scripting.Hosting.VisualBasicLanguageProvider", "Microsoft.VisualBasic.Scripting", ".vbx", "vbx"),
                    new LanguageProviderSetup("Ruby.Hosting.RubyLanguageProvider", "IronRuby", ".rb", "rb", "ruby", "ironruby"),
                    new LanguageProviderSetup("ToyScript.ToyLanguageProvider", "ToyScript", ".ts", "ts", "toyscript", "toyscript"),
#endif
                };
            } else {
                 _languageProviders = new LanguageProviderSetup[0];
            }

            _palType = typeof(PlatformAdaptationLayer);
            _hostType = typeof(ScriptHost);

#if !SILVERLIGHT
            _localHostType = typeof(LocalScriptHost);
#endif
        }

        internal void RegisterProviders(ScriptDomainManager manager) {
            Debug.Assert(manager != null);

            foreach (LanguageProviderSetup provider in _languageProviders) {
                manager.RegisterLanguageProvider(provider.Assembly, provider.Type, provider.Names);
            }
        }

        /// <exception cref="InvalidImplementationException">The PAL type has no compatible constructor.</exception>
        internal PlatformAdaptationLayer CreatePAL() {
            return ReflectionUtils.CreateInstance<PlatformAdaptationLayer>(_palType);
        }

        internal IScriptHost CreateScriptHost(IScriptEnvironment environment) {
            Debug.Assert(environment != null);
#if !SILVERLIGHT
            // a remote host was created by call to ScriptEnvironment.CreateRemote
            if (_remoteHost != null) return CreateLocalHost(_remoteHost);
#endif
            // do not create forwarding host (single app-domain scenario):
            return CreateHostLocally(environment);
        }

        /// <exception cref="InvalidImplementationException">The host type failed to instantiate.</exception>
        internal ScriptHost CreateHostLocally(IScriptEnvironment environment) {
            Debug.Assert(environment != null);
            return ReflectionUtils.CreateInstance<ScriptHost>(_hostType, environment);
        }
    }
}
