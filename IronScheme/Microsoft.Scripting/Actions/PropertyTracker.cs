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

namespace Microsoft.Scripting.Actions {
    public abstract class PropertyTracker : MemberTracker {
        public override TrackerTypes MemberType {
            get { return TrackerTypes.Property; }
        }

        public abstract MethodInfo GetGetMethod();
        public abstract MethodInfo GetSetMethod();
        public abstract MethodInfo GetGetMethod(bool privateMembers);
        public abstract MethodInfo GetSetMethod(bool privateMembers);

        public virtual MethodInfo GetDeleteMethod() {
            return null;
        }

        public virtual MethodInfo GetDeleteMethod(bool privateMembers) {
            return null;
        }

        public abstract ParameterInfo[] GetIndexParameters();

        public virtual bool IsExtension {
            get {
                return false;
            }
        }
    }
}
