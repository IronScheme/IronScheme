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
using System.Text;
using System.Reflection;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;

using Microsoft.Scripting;

namespace Microsoft.Scripting.Types {
    public class ReflectedProperty : ReflectedGetterSetter {
        public ReflectedProperty(PropertyInfo info, MethodInfo getter, MethodInfo setter, NameType nt)
            : base(info, getter, setter, nt) {
        }

        /// <summary>
        /// Convenience function for users to call directly
        /// </summary>
        public object GetValue(CodeContext context, object instance) {
            object value;
            if (TryGetValue(context, instance, DynamicHelpers.GetDynamicType(instance), out value)) {
                return value;
            }
            throw new InvalidOperationException("cannot get property");
        }

        /// <summary>
        /// Convenience function for users to call directly
        /// </summary>
        public void SetValue(CodeContext context, object instance, object value) {
            if (!TrySetValue(context, instance, DynamicHelpers.GetDynamicType(instance), value)) {
                throw new InvalidOperationException("cannot set property");
            }            
        }

        public override bool TrySetValue(CodeContext context, object instance, DynamicMixin owner, object value) {
            if (Setter == null) return false;

            if (instance == null) {
                if (Setter.IsStatic && DeclaringType != ((DynamicType)owner).UnderlyingSystemType)
                    return false;
            } else if (instance != null) {
                if (Setter.IsStatic)
                    return false;
            }

            return CallSetter(context, instance, Utils.ArrayUtils.EmptyObjects, value);
        }

        public override bool TryGetValue(CodeContext context, object instance, DynamicMixin owner, out object value) {
            PerfTrack.NoteEvent(PerfTrack.Categories.Properties, this);

            value = CallGetter(context, instance, Utils.ArrayUtils.EmptyObjects);
            return true;
        }

        public sealed override bool IsVisible(CodeContext context, DynamicMixin owner) {
            if (context.ModuleContext.ShowCls)
                return true;

            return NameType == NameType.PythonProperty;
        }

        public override bool IsSetDescriptor(CodeContext context, DynamicMixin owner) {
            return Setter != null;
        }
    }
}
