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
using System.Diagnostics;

namespace Microsoft.Scripting.Types {
    /// <summary>
    /// Represents a ReflectedProperty created for an extension method.  Logically the property is an
    /// instance property but the method implementing it is static.
    /// </summary>
    public class ReflectedExtensionProperty : ReflectedProperty {
        private MethodInfo _deleter;
        private ExtensionPropertyInfo _extInfo;

        public ReflectedExtensionProperty(ExtensionPropertyInfo info, NameType nt)
            : this(info, info.Getter, info.Setter, nt) {
        }

        public ReflectedExtensionProperty(ExtensionPropertyInfo info, MethodInfo getter, MethodInfo setter, NameType nt)
            : base(null, getter, setter, nt) {
            Debug.Assert(Getter == null || Getter.IsStatic);
            Debug.Assert(Setter == null || Setter.IsStatic);

            _extInfo = info;
            _deleter = info.Deleter;
        }


        public override bool TryGetValue(CodeContext context, object instance, DynamicMixin owner, out object value) {
            if (Getter == null || instance == null) {
                value = null;
                return false;
            }

            return base.TryGetValue(context, instance, owner, out value);
        }

        public override bool TrySetValue(CodeContext context, object instance, DynamicMixin owner, object value) {
            if (Setter == null || instance == null) return false;

            return CallSetter(context, instance, Utils.ArrayUtils.EmptyObjects, value);
        }

        public override bool TryDeleteValue(CodeContext context, object instance, DynamicMixin owner) {
            if (_deleter == null || instance == null) {
                return base.TryDeleteValue(context, instance, owner);
            }

            MethodBinder.MakeBinder(context.LanguageContext.Binder, Name, new MethodInfo[] { _deleter }, BinderType.Normal).CallInstanceReflected(context, instance);
            return true;
        }

        public override Type DeclaringType {
            get {
                return _extInfo.DeclaringType;
            }
        }

        public ExtensionPropertyInfo ExtInfo {
            get {
                return _extInfo;
            }
        }

        public override string Name {
            get {
                return base.Name ?? _extInfo.Name;
            }
        }

    }
}
