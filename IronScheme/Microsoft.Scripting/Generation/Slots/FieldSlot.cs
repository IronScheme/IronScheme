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
using System.Reflection.Emit;
using System.Diagnostics;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Generation {
    /// <summary>
    /// FieldSlot is an access of an attribute of an object 
    /// </summary>
    public class FieldSlot : Slot {
        private readonly Slot _instance;
        private readonly FieldInfo _field;

        public FieldSlot(Slot instance, FieldInfo field) {
            Contract.RequiresNotNull(instance, "instance");
            Contract.RequiresNotNull(field, "field");

            this._instance = instance;
            this._field = field;
        }
        public override void EmitGet(CodeGen cg) {
            Contract.RequiresNotNull(cg, "cg");

            _instance.EmitGet(cg);
            cg.Emit(OpCodes.Ldfld, _field);
        }
        public override void EmitGetAddr(CodeGen cg) {
            Contract.RequiresNotNull(cg, "cg");

            _instance.EmitGet(cg);
            cg.EmitFieldAddress(_field);
        }

        public override void EmitSet(CodeGen cg, Slot val) {
            Contract.RequiresNotNull(cg, "cg");
            Contract.RequiresNotNull(val, "val");

            _instance.EmitGet(cg);
            val.EmitGet(cg);
            cg.Emit(OpCodes.Stfld, _field);
        }

        public override void EmitSet(CodeGen cg) {
            Contract.RequiresNotNull(cg, "cg");

            Slot val = cg.GetLocalTmp(_field.FieldType);
            val.EmitSet(cg);
            EmitSet(cg, val);
            cg.FreeLocalTmp(val);
        }

        public override Type Type {
            get {
                return _field.FieldType;
            }
        }

        /// <summary>
        /// Gets the slot that is used for the instance of the field
        /// </summary>
        public Slot Instance {
            get { return _instance; }
        }

        /// <summary>
        /// Gets the FieldInfo for which this slot loads its value
        /// </summary>
        public FieldInfo Field {
            get { return _field; }
        }

        public override string ToString() {
            return String.Format("FieldSlot From: ({0}) On {1} Field {2}", _instance, _field.DeclaringType, _field.Name);
        }
    }
}
