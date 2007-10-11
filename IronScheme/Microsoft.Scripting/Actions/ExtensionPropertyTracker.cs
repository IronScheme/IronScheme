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
    public class ExtensionPropertyTracker : PropertyTracker {
        private string _name;
        private Type _declaringType;
        private MethodInfo _getter, _setter, _deleter;

        public ExtensionPropertyTracker(string name, MethodInfo getter, MethodInfo setter, MethodInfo deleter, Type declaringType) {
            _name = name; 
            _getter = getter; 
            _setter = setter;
            _deleter = deleter;
            _declaringType = declaringType;
        }

        public override string Name {
            get { return _name; }
        }

        public override Type DeclaringType {
            get { return _declaringType; }
        }

        public override MethodInfo GetGetMethod() {
            if (_getter != null && _getter.IsPrivate) return null;

            return _getter;
        }

        public override MethodInfo GetSetMethod() {
            if (_setter != null && _setter.IsPrivate) return null;

            return _setter;
        }

        public override MethodInfo GetGetMethod(bool privateMembers) {
            if (privateMembers) return _getter;

            return GetGetMethod();
        }

        public override MethodInfo GetSetMethod(bool privateMembers) {
            if (privateMembers) return _setter;

            return GetSetMethod();
        }

        public override MethodInfo GetDeleteMethod() {
            if (_deleter != null && _deleter.IsPrivate) return null;

            return _deleter;
        }

        public override MethodInfo GetDeleteMethod(bool privateMembers) {
            if (privateMembers) return _deleter;

            return GetDeleteMethod();
        }

        public override bool IsExtension {
            get {
                return true;
            }
        }

        public override ParameterInfo[] GetIndexParameters() {
            return new ParameterInfo[0];
        }
    }
}
