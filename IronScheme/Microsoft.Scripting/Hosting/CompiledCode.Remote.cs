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
using System.Runtime.Remoting;

namespace Microsoft.Scripting.Hosting {
    internal sealed class RemoteCompiledCode : RemoteWrapper, ICompiledCode {
        private readonly CompiledCode _compiledCode;

        public RemoteCompiledCode(CompiledCode compiledCode) {
            _compiledCode = compiledCode;
        }

        public override ILocalObject LocalObject {
            get { return _compiledCode; }
        }

#region ICompiledCode Members

        public IScriptModule MakeModule(string name) {
            return RemoteWrapper.WrapRemotable<IScriptModule>(_compiledCode.MakeModule(name));
        }

        public void Execute() {
            _compiledCode.Execute();
        }

        public void Execute(IScriptModule module) {
            _compiledCode.Execute(module);
        }

        // throws SerializationException 
        public object Evaluate() {
            return _compiledCode.Evaluate(); ;
        }

        // throws SerializationException
        public object Evaluate(IScriptModule module) {
            return _compiledCode.Evaluate(module);
        }

        public IObjectHandle EvaluateAndWrap() {
            return _compiledCode.EvaluateAndWrap();
        }

        public IObjectHandle EvaluateAndWrap(IScriptModule module) {
            return _compiledCode.EvaluateAndWrap(module);
        }

        #endregion
    }
}

#endif