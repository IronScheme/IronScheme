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
using System.Reflection.Emit;
using Microsoft.Scripting.Generation.Slots;

namespace Microsoft.Scripting.Generation.Factories
{

    /// <summary>
    /// Creates slots that are backed by local variables.
    /// </summary>
    internal class LocalSlotFactory : SlotFactory
    {
        private CodeGen _codeGen;

        public LocalSlotFactory(CodeGen codeGen)
        {
            _codeGen = codeGen;
        }

        protected override Slot CreateSlot(SymbolId name, Type type)
        {
            var b = _codeGen.DeclareLocal(type);

            if (_codeGen.EmitDebugInfo) PAL.SetLocalSymInfo(b, SymbolTable.IdToString(Ast.Variable.UnGenSym(name)));

            return new LocalSlot(b, _codeGen);
        }
    }
}
