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

namespace Microsoft.Scripting.Generation {

    /// <summary>
    /// Creates slots that are backed by local variables.
    /// </summary>
    public class LocalSlotFactory : SlotFactory {
        private CodeGen _codeGen;

        public LocalSlotFactory(CodeGen codeGen) {
            this._codeGen = codeGen;
        }

        protected override Slot CreateSlot(SymbolId name, Type type) {
            LocalBuilder b = _codeGen.DeclareLocal(type);
            if (_codeGen.EmitDebugInfo) b.SetLocalSymInfo(SymbolTable.IdToString(name));
            return new LocalSlot(b, _codeGen);
        }
    }
}
