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
using Microsoft.Scripting.Generation.Slots;

namespace Microsoft.Scripting.Generation.Factories
{

    /// <summary>
    /// Creates slots that are backed by fields in a type.
    /// </summary>
    public class FieldSlotFactory : SlotFactory
    {
        private TypeGen _typeGen;
        private Slot _instance;

        public FieldSlotFactory(TypeGen typeGen, Slot instance)
        {
            _typeGen = typeGen;
            _instance = instance;
        }
        protected override Slot CreateSlot(SymbolId name, Type type)
        {
            var fb = _typeGen.TypeBuilder.DefineField(SymbolTable.IdToString(name), type, FieldAttributes.Public);
            return new FieldSlot(_instance, fb);
        }
    }
}
