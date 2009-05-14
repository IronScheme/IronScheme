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
using System.Collections.Generic;
using System.Text;
using System.Diagnostics;
using Microsoft.Scripting.Hosting;
using System.Runtime.Remoting;

namespace Microsoft.Scripting {

    internal sealed class RemoteScriptModule : RemoteWrapper, IScriptModule {
        private readonly ScriptModule _module;

        public override ILocalObject LocalObject {
            get { return _module; }
        }

        public string ModuleName {
            get { return _module.ModuleName; }
        }

        public string FileName {
            get { return _module.FileName; }
            set { _module.FileName = value; }
        }

#region Construction

        internal RemoteScriptModule(ScriptModule module) {
            Debug.Assert(module != null);
            _module = module;
        }
        
        public override object InitializeLifetimeService() {
            return null;
        }

        #endregion

        public void Execute() {
            _module.Execute();
        }

        public void Reload() {
            _module.Reload();
        }

        public CompilerOptions GetCompilerOptions(IScriptEngine engine) {
            return _module.GetCompilerOptions(engine);
        }

        // throws SerializationException 
        public bool TryLookupVariable(string name, out object value) {
            return _module.TryLookupVariable(name, out value);
        }

        // throws SerializationException 
        public bool TryGetVariable(string name, out object value) {
            return _module.TryGetVariable(name, out value);
        }

        // throws SerializationException 
        public object LookupVariable(string name) {
            return _module.LookupVariable(name);
        }

        // throws SerializationException 
        public void SetVariable(string name, object value) {
            _module.SetVariable(name, value);
        }
        
        public IObjectHandle LookupVariableAndWrap(string name) {
            return _module.LookupVariableAndWrap(name);
        }

        public bool VariableExists(string name) {
            return _module.VariableExists(name);
        }

        public bool RemoveVariable(string name) {
            return _module.RemoveVariable(name);
        }

        public void ClearVariables() {
            _module.ClearVariables();
        }
    }
}

#endif