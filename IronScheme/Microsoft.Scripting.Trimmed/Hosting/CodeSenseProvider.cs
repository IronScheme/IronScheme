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
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Hosting {

    public interface ICodeSenseProvider : IRemotable, ILanguageService {
        string GetFunctionSignature(string name);
        string[] GetMemberNames(string name);
        string GetFunctionDoc(string name);
    }

    public abstract class CodeSenseProvider : ICodeSenseProvider, ILocalObject {
        private readonly IScriptEngine _engine;
        private readonly IScriptModule _module;

        protected IScriptEngine Engine { get { return _engine; } }
        protected IScriptModule Module { get { return _module; } }

        public CodeSenseProvider(IScriptEngine engine, IScriptModule module) {
            Contract.RequiresNotNull(engine, "engine");
            _engine = engine;
            _module = module;
        }

        #region ILocalObject Members

#if !SILVERLIGHT
        RemoteWrapper ILocalObject.Wrap() {
            return new RemoteCodeSenseProvider(this);
        }
#endif

        #endregion

        #region ICodeSenseProvider Members

        public abstract string GetFunctionSignature(string name);
        public abstract string[] GetMemberNames(string name);
        public abstract string GetFunctionDoc(string name);

        #endregion
    }
}
