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
    public class ReflectedField : DynamicTypeSlot, IContextAwareMember {
        private NameType _nameType;
        public readonly FieldInfo info;

        public ReflectedField(FieldInfo info, NameType nameType) {
            this._nameType = nameType;
            this.info = info;
        }

        public ReflectedField(FieldInfo info)
            : this(info, NameType.PythonField) {
        }

        /// <summary>
        /// Convenience function for users to call directly
        /// </summary>
        public object GetValue(CodeContext context, object instance) {
            object value;
            if (TryGetValue(context, instance, DynamicHelpers.GetDynamicType(instance), out value)) {
                return value;
            }
            throw new InvalidOperationException("cannot get field");
        }

        /// <summary>
        /// Convenience function for users to call directly
        /// </summary>
        public void SetValue(CodeContext context, object instance, object value) {
            if (!TrySetValue(context, instance, DynamicHelpers.GetDynamicType(instance), value)) {
                throw new InvalidOperationException("cannot set field");
            }            
        }

        public override bool TryGetValue(CodeContext context, object instance, DynamicMixin owner, out object value) {
            PerfTrack.NoteEvent(PerfTrack.Categories.Fields, this);
            if (instance == null) {
                if (info.IsStatic) {
                    value = info.GetValue(null);
                } else {
                    value = this;
                }
            } else {
                value = info.GetValue(context.LanguageContext.Binder.Convert(instance, info.DeclaringType));
            }

            return true;
        }

        private bool ShouldSetOrDelete(object instance, DynamicMixin type) {
            DynamicType dt = type as DynamicType;
            
            // statics must be assigned through their type, not a derived type.  Non-statics can
            // be assigned through their instances.
            return (dt != null && info.DeclaringType == dt.UnderlyingSystemType) || !info.IsStatic || info.IsLiteral;
        }

        public override bool TrySetValue(CodeContext context, object instance, DynamicMixin owner, object value) {
            if (ShouldSetOrDelete(instance, owner)) {
                DoSet(context, instance, value);
                return true;
            }

            return false;
        }

        public override bool IsSetDescriptor(CodeContext context, DynamicMixin owner) {
            return (info.Attributes & FieldAttributes.InitOnly) != 0;
        }

        public override bool TryDeleteValue(CodeContext context, object instance, DynamicMixin owner) {
            if (ShouldSetOrDelete(instance, owner)) {
                return base.TryDeleteValue(context, instance, owner);
            }
            return false;
        }

        private void DoSet(CodeContext context, object instance, object val) {
            PerfTrack.NoteEvent(PerfTrack.Categories.Fields, this);
            if (instance != null && instance.GetType().IsValueType)
                throw new ArgumentException(String.Format("Attempt to update field '{0}' on value type '{1}'; value type fields cannot be directly modified", info.Name, info.DeclaringType.Name));
            if (info.IsInitOnly || info.IsLiteral)
                throw new MissingFieldException(String.Format("Cannot set field {1} on type {0}", info.DeclaringType.Name, SymbolTable.StringToId(info.Name)));

            info.SetValue(instance, context.LanguageContext.Binder.Convert(val, info.FieldType));
        }

        #region IContextAwareMember Members

        public override bool IsVisible(CodeContext context, DynamicMixin owner) {
            return _nameType == NameType.PythonField || context.ModuleContext.ShowCls;
        }

        #endregion
    }
}
