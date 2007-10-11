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
using System.Reflection.Emit;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Generation {
    /// <summary>
    /// Slot that indexes into a param array.  Currently the param array is assumed to always
    /// be of type object.
    /// </summary>
    public class ParamArraySlot : Slot {
        private Slot _param;
        private int _index;

        public ParamArraySlot(Slot paramArray, int paramIndex) {
            _param = paramArray;
            _index = paramIndex;
        }

        public override void EmitGet(CodeGen cg) {
            Contract.RequiresNotNull(cg, "cg");

            _param.EmitGet(cg);
            cg.EmitInt(_index);
            cg.Emit(OpCodes.Ldelem_Ref);
        }

        public override void EmitGetAddr(CodeGen cg) {
            throw new NotImplementedException(Resources.NotImplemented);
        }

        public override Type Type {
            get { return typeof(object); }
        }

        public override string ToString() {
            return String.Format("ParamArraySlot From: ({0}) Index: {1}", _param, _index);
        }
    }
}
