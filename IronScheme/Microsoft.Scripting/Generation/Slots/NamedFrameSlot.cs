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
using System.Diagnostics;

namespace Microsoft.Scripting.Generation.Slots
{
    /// <summary>
    /// NamedFrameSlot represens a global variables (or builtin) of CompiledCode _code executing 
    /// in the context of a CodeContext. They have to be looked up by name at runtime.
    /// </summary>
    sealed class NamedFrameSlot : Slot
    {
        // The CodeContext whose Namespace will be used to resolve the Name
        private readonly Slot _frame;
        private readonly SymbolId _name;

        public NamedFrameSlot(Slot frame, SymbolId name)
        {
            Debug.Assert(typeof(CodeContext).IsAssignableFrom(frame.Type), "invalid frame type");
            _frame = frame;
            _name = name;
        }

        public override void EmitGet(CodeGen cg)
        {
            //
            // Emit: RuntimeHelpers.LookupGlobalName(context, name)
            //
            _frame.EmitGet(cg);
            cg.EmitSymbolId(_name);
            cg.EmitCall(typeof(RuntimeHelpers), nameof(RuntimeHelpers.LookupGlobalName));
        }

        public override void EmitGetAddr(CodeGen cg)
        {
            //???how bad is it that we can't do this???
            throw new NotImplementedException("address of frame slot");
        }

        public override void EmitSet(CodeGen cg, Slot val)
        {
            //
            // Emit: RuntimeHelpers.SetGlobalName(context, name, value)
            //
            //_frame.EmitGet(cg);
            //cg.EmitSymbolId(_name);
            //val.EmitGet(cg);
            //cg.EmitCall(typeof(RuntimeHelpers), "SetGlobalName");
        }

        public override void EmitSetUninitialized(CodeGen cg)
        {
        }

        public override Type Type
        {
            get
            {
                return typeof(object);
            }
        }

        public override string ToString()
        {
            return string.Format("NamedFromSlot Name: ({0}) From: {1}", SymbolTable.IdToString(_name), _frame);
        }

    }
}
