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
using System.Collections.Generic;
using System.Text;

namespace Microsoft.Scripting {
    public sealed class KwCallInfo {
        private object[] _args;
        private string[] _names;
        public KwCallInfo(object[] args, string[] names) {
            _args = args;
            _names = names;
        }

        public object[] Arguments {
            get {
                return _args;
            }
        }

        public string[] Names {
            get {
                return _names;
            }
        }
    }
}
