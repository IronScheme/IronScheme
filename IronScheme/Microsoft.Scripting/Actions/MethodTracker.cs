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
using System.Reflection;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Actions {
    public class MethodTracker : MemberTracker {
        private readonly MethodInfo _method;

        public MethodTracker(MethodInfo method) {
            Contract.RequiresNotNull(method, "method");
            _method = method;
        }

        public override Type DeclaringType {
            get { return _method.DeclaringType; }
        }

        public override TrackerTypes MemberType {
            get { return TrackerTypes.Method; }
        }

        public override string Name {
            get { return _method.Name; }
        }

        public MethodInfo Method {
            get {
                return _method;
            }
        }

        public bool IsPublic {
            get {
                return _method.IsPublic;
            }
        }

        public override string ToString() {
            return _method.ToString();
        }
    }
}
