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
#if !SILVERLIGHT

using System;
using System.Collections.Generic;
using System.Text;
using System.Diagnostics;
using System.Threading;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Hosting {
    
    /// <summary>
    /// Forwards to associated host wrapping objects if it resides in remote domain.
    /// Override those methods that shouldn't forward to remote host.
    /// </summary>
    public class LocalScriptHost : IScriptHost, ILocalObject {
        
        // implementors shouldn't have access to the host; they should call the base implementation for forwarding
        private RemoteScriptHost _remoteHost;
        private ScriptModule _defaultModule;

        public LocalScriptHost() {
        }

        // the host is not set in ctor in order to prevent manipulation with it while constructing the base class:
        internal void SetRemoteHost(RemoteScriptHost remoteHost) {
            Debug.Assert(remoteHost != null);
            _remoteHost = remoteHost;
        }

        public virtual string NormalizePath(string path) {
            Debug.Assert(_remoteHost != null);
            return _remoteHost.NormalizePath(path);
        }

        public virtual string[] GetSourceFileNames(string mask, string searchPattern) {
            Debug.Assert(_remoteHost != null);
            return _remoteHost.GetSourceFileNames(mask, searchPattern);
        }

        public virtual SourceUnit TryGetSourceFileUnit(IScriptEngine engine, string path, Encoding encoding) {
            Debug.Assert(_remoteHost != null);
            return _remoteHost.TryGetSourceFileUnit(RemoteWrapper.WrapRemotable<IScriptEngine>(engine), path, encoding);
        }

        public virtual SourceUnit ResolveSourceFileUnit(string name) {
            Debug.Assert(_remoteHost != null);
            return _remoteHost.ResolveSourceFileUnit(name);
        }

        /// <summary>
        /// TODO: Called under a lock. Should work with the engine via the argument only.
        /// </summary>
        public virtual void EngineCreated(IScriptEngine engine) {
            Contract.RequiresNotNull(engine, "engine");
            Debug.Assert(_remoteHost != null);
            _remoteHost.EngineCreated(RemoteWrapper.WrapRemotable<IScriptEngine>(engine));
        }

        public virtual void ModuleCreated(IScriptModule module) {
            Contract.RequiresNotNull(module, "module");
            Debug.Assert(_remoteHost != null);
            _remoteHost.ModuleCreated(RemoteWrapper.WrapRemotable<IScriptModule>(module));
        }

        // throws SerializationException 
        public virtual bool TrySetVariable(IScriptEngine engine, SymbolId name, object value) {
            Debug.Assert(_remoteHost != null);
            return _remoteHost.TrySetVariable(RemoteWrapper.WrapRemotable<IScriptEngine>(engine), name, value);
        }

        // throws SerializationException 
        public virtual bool TryGetVariable(IScriptEngine engine, SymbolId name, out object value) {
            Debug.Assert(_remoteHost != null);
            return _remoteHost.TryGetVariable(RemoteWrapper.WrapRemotable<IScriptEngine>(engine), name, out value);
        }

        public virtual IScriptModule DefaultModule {
            get {
                if (_defaultModule == null) {
                    ScriptHost.CreateDefaultModule(ref _defaultModule);
                }
                return _defaultModule;
            }
        }

        public Action<Exception> EventExceptionHandler {
            get { return null; }
        }

        #region ILocalObject Members

#if !SILVERLIGHT
        RemoteWrapper ILocalObject.Wrap() {
            return new RemoteScriptHost(this);
        }
#endif

        #endregion
    }
}

#endif