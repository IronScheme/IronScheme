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
    /// Argument access
    /// </summary>
    public class ArgSlot : Slot {
        private Type _argType;
        private int _index;
        private CodeGen _codeGen;

        public ArgSlot(int index, Type type, CodeGen codeGen) {
            this._index = index;
            this._argType = type;
            this._codeGen = codeGen;
        }

        public void SetName(string name) {
            _codeGen.DefineParameter(_index, ParameterAttributes.None, name);
        }


        public override void EmitGet(CodeGen cg) {
            Contract.RequiresNotNull(cg, "cg");
            Debug.Assert(cg == this._codeGen);
            cg.EmitTrueArgGet(_index);
        }

        public override void EmitGetAddr(CodeGen cg) {
            Contract.RequiresNotNull(cg, "cg");
            Debug.Assert(cg == this._codeGen);
            cg.EmitArgAddr(_index);
        }

        public override void EmitSet(CodeGen cg) {
            Contract.RequiresNotNull(cg, "cg");
            Debug.Assert(cg == this._codeGen);
            cg.Emit(OpCodes.Starg, _index);
        }

        public override Type Type {
            get {
                return _argType;
            }
        }

        public override string ToString() {
            return String.Format("ArgSlot Index: {0} Type: {1}", _index, _argType.FullName);
        }
    }
}
