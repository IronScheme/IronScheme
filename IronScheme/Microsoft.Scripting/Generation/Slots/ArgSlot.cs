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
using System.Diagnostics;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Generation.Slots
{
    /// <summary>
    /// Argument access
    /// </summary>
    internal class ArgSlot : Slot
    {
        private Type _argType;
        private int _index;
        private CodeGen _codeGen;

        public ArgSlot(int index, Type type, CodeGen codeGen)
        {
            _index = index;
            _argType = type;
            _codeGen = codeGen;
        }

        public override void EmitGet(CodeGen cg)
        {
            Contract.RequiresNotNull(cg, "cg");
            Debug.Assert(cg == _codeGen);
            cg.EmitTrueArgGet(_index);
        }

        public override void EmitGetAddr(CodeGen cg)
        {
            Contract.RequiresNotNull(cg, "cg");
            Debug.Assert(cg == _codeGen);
            cg.EmitArgAddr(_index);
        }

        public override void EmitSet(CodeGen cg)
        {
            Contract.RequiresNotNull(cg, "cg");
            Debug.Assert(cg == _codeGen);
            if (_index < byte.MaxValue)
            {
                cg.Emit(OpCodes.Starg_S, (byte)_index);
            }
            else
            {
                cg.Emit(OpCodes.Starg, (short)_index);
            }
        }

        public override Type Type
        {
            get
            {
                return _argType;
            }
        }

        public override string ToString()
        {
            return string.Format("ArgSlot Index: {0} Type: {1}", _index, _argType.FullName);
        }
    }
}
