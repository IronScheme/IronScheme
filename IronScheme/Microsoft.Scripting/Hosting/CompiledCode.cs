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
using System.Diagnostics;
using System.Runtime.Remoting;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Hosting {

    public interface ICompiledCode : IRemotable {
        IScriptModule MakeModule(string name);
        
        void Execute();
        void Execute(IScriptModule module);

        object Evaluate();
        object Evaluate(IScriptModule module);
     
#if !SILVERLIGHT
        IObjectHandle EvaluateAndWrap();
        IObjectHandle EvaluateAndWrap(IScriptModule module);
#endif
    }

    /// <summary>
    /// Hosting API counterpart for <see cref="ScriptCode"/>.
    /// </summary>
    public sealed class CompiledCode : ICompiledCode, ILocalObject {
        private readonly ScriptCode _code;

        // should be called only from ScriptCode.FromCompiledCode:
        internal ScriptCode ScriptCode { get { return _code; } }

        internal CompiledCode(ScriptCode code) {
            Debug.Assert(code != null);
            _code = code;
        }

        public IScriptModule MakeModule(string name) {
            Contract.RequiresNotNull(name, "name");
            return ScriptDomainManager.CurrentManager.CreateModule(name, _code);
        }

        /// <summary>
        /// Execute code within default module context.
        /// </summary>
        public void Execute() {
            Evaluate(null);
        }

        /// <summary>
        /// Execute code within a given module context. 
        /// The module must be local with respect to the compiled code object.
        /// </summary>
        public void Execute(IScriptModule module) {
            Evaluate(module);
        }

        /// <summary>
        /// Execute code within default module context and returns the result.
        /// </summary>
        public object Evaluate() {
            return Evaluate(null);
        }

        /// <summary>
        /// Execute code within a given module context and returns the result.
        /// The module must be local with respect to the compiled code object.
        /// </summary>
        public object Evaluate(IScriptModule module) {
            ScriptModule localModule;

            if (module == null) {
                localModule = RemoteWrapper.TryGetLocal<ScriptModule>(ScriptDomainManager.CurrentManager.Host.DefaultModule);
            } else {
                localModule = RemoteWrapper.GetLocalArgument<ScriptModule>(module, "module");
            }

            return _code.Run(localModule);
        }

#if !SILVERLIGHT

        public IObjectHandle EvaluateAndWrap() {
            return new ObjectHandle(Evaluate());
        }

        public IObjectHandle EvaluateAndWrap(IScriptModule module) {
            return new ObjectHandle(Evaluate(module));
        }

#endif

        #region ILocalObject Members

#if !SILVERLIGHT
        RemoteWrapper ILocalObject.Wrap() {
            return new RemoteCompiledCode(this);
        }
#endif

        #endregion

    }
}
