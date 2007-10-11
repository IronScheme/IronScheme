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
using System.Reflection;
using System.Collections.Generic;
using System.Text;

namespace Microsoft.Scripting.Types {

    public class ExtensionPropertyInfo {
        private MethodInfo _getter, _setter, _deleter;
        private Type _declaringType;

        public ExtensionPropertyInfo(DynamicType declaringDynamicType, MethodInfo mi)
            : this(declaringDynamicType.UnderlyingSystemType, mi) {
        }

        public ExtensionPropertyInfo(Type logicalDeclaringType, MethodInfo mi) {
            _declaringType = logicalDeclaringType;

            string propname = mi.Name.Substring(3);

            _deleter = mi.DeclaringType.GetMethod("Delete" + propname);

            if (String.Compare(mi.Name, 0, "Get", 0, 3) == 0) {
                _getter = mi;
                _setter = mi.DeclaringType.GetMethod("Set" + propname);
            } else {
                _getter = mi.DeclaringType.GetMethod("Get" + propname);
                _setter = mi;
            }

            if (_setter != null && GetEffectiveParameterCount(_setter) != 2) { System.Diagnostics.Debug.Assert(false, _setter.Name); throw new InvalidOperationException("setter must take 2 parameters"); }
            if (_getter != null && GetEffectiveParameterCount(_getter) != 1) throw new InvalidOperationException("getter must take 2 parameters");
            if (_deleter != null && GetEffectiveParameterCount(_deleter) != 1) throw new InvalidOperationException("deleter must take 2 parameters");
        }

        private int GetEffectiveParameterCount(MethodInfo mi) {
            ParameterInfo[] pis = mi.GetParameters();
            if (pis.Length > 0 && pis[0].ParameterType == typeof(CodeContext)) {
                return pis.Length - 1;
            }
            return pis.Length;
        }

        public MethodInfo Getter {
            get { return _getter; }
        }

        public MethodInfo Setter {
            get { return _setter; }
        }

        public MethodInfo Deleter {
            get { return _deleter; }
        }

        public Type DeclaringType {
            get { return _declaringType; }
        }

        public string Name {
            get {
                // remove Get or Set from name
                if (_getter != null) return _getter.Name.Substring(3);
                return _setter.Name.Substring(3);
            }
        }

        public static string GetName(MethodInfo mi)
        {
            return mi.Name.Substring(3);
        }
    }
}
