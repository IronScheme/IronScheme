/* ****************************************************************************
 *
 * Copyright (c) Microsoft Corporation. 
 *
 * This source code is subject to terms and conditions of the Microsoft Public License. A 
 * copy of the license can be found in the License.html file at the root of this distribution. If 
 * you cannot locate the  Microsoft Public License, please send an email to 
 * dlr@microsoft.com. By using this source code in any fashion, you are agreeing to be bound 
 * by the terms of the Microsoft Public License.
 *
 * You must not remove this notice, or any other, from this software.
 *
 *
 * ***************************************************************************/

using System;

using System.Reflection;
using System.Reflection.Emit;
using System.Collections.Generic;
using System.Diagnostics;

namespace Microsoft.Scripting.Generation {
    public class ModuleGlobalSlot : Slot {
        private Slot _wrapper;

        public ModuleGlobalSlot(Slot builtinWrapper) {
            Debug.Assert(builtinWrapper.Type == typeof(ModuleGlobalWrapper));
            if (builtinWrapper.Type != typeof(ModuleGlobalWrapper)) throw new ArgumentException("builtinWrapper " + builtinWrapper.GetType().FullName);

            _wrapper = builtinWrapper;
        }

        public override void EmitGet(CodeGen cg) {
            _wrapper.EmitGet(cg);
            cg.EmitPropertyGet(typeof(ModuleGlobalWrapper), "CurrentValue");
        }

        public override void EmitSet(CodeGen cg)
        {
          _wrapper.EmitGet(cg);
          cg.EmitCall(typeof(ModuleGlobalWrapper), "SetValue");
        }

        public override void EmitSet(CodeGen cg, Slot val) {
            _wrapper.EmitGet(cg);
            val.EmitGet(cg);
            cg.EmitPropertySet(typeof(ModuleGlobalWrapper), "CurrentValue");
        }

        public void EmitGetRaw(CodeGen cg) {
            _wrapper.EmitGet(cg);
            cg.EmitPropertyGet(typeof(ModuleGlobalWrapper), "RawValue");
        }

        public override void EmitCheck(CodeGen cg, SymbolId name) {
            // checks are handled in the get_CurrentValue
        }

        public void EmitWrapperAddr(CodeGen cg) {
            _wrapper.EmitGetAddr(cg);
        }

        public void EmitWrapper(CodeGen cg) {
            _wrapper.EmitGet(cg);
        }

        public override void EmitGetAddr(CodeGen cg) {
            throw new NotSupportedException("Can't get address of module global.");
        }

        public override Type Type {
            get { return typeof(object); }
        }
    }
}
