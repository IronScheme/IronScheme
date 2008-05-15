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

namespace Microsoft.Scripting.Hosting {

    internal sealed class RemoteCodeSenseProvider : RemoteWrapper, ICodeSenseProvider {

        private CodeSenseProvider _provider;

        public override ILocalObject LocalObject {
            get { return _provider; }
        }

        internal RemoteCodeSenseProvider(CodeSenseProvider provider) {
            Debug.Assert(provider != null);
            _provider = provider;
        }

        #region ICodeSenseProvider Members

        public string GetFunctionSignature(string name) {
            return _provider.GetFunctionSignature(name);
        }

        public string[] GetMemberNames(string name) {
            return _provider.GetMemberNames(name);
        }

        public string GetFunctionDoc(string name) {
            return _provider.GetFunctionDoc(name);
        }

        #endregion
    }

}

#endif