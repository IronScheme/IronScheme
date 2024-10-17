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

namespace Microsoft.Scripting.Generation.Slots
{
    internal class LocalNamedFrameSlot : Slot
    {
        private readonly Slot _frame;
        private readonly SymbolId _name;

        public LocalNamedFrameSlot(Slot frame, SymbolId name)
        {
            _frame = frame;
            _name = name;
        }

        public override void EmitGet(CodeGen cg)
        {
            // RuntimeHelpers.LookupName(context, name)
            _frame.EmitGet(cg);
            cg.EmitSymbolId(_name);
            cg.EmitCall(typeof(RuntimeHelpers), nameof(RuntimeHelpers.LookupName));
        }

        public override void EmitGetAddr(CodeGen cg)
        {
            //???how bad is it that we can't do this???
            throw new NotImplementedException("address of local frame slot");
        }

        public override void EmitSet(CodeGen cg, Slot val)
        {
            // Emit the following:
            //    RuntimeHelpers.SetName(codeContext, name, value)
            //_frame.EmitGet(cg);
            //cg.EmitSymbolId(_name);
            //val.EmitGet(cg);
            //cg.EmitCall(typeof(RuntimeHelpers), "SetNameBoxed");
        }

        public override void EmitSetUninitialized(CodeGen cg)
        {
            // In interpreted mode, we depend on setting values to Uninitialized to implement scope rules correctly.
            if (cg.InterpretedMode)
            {
                base.EmitSetUninitialized(cg);
            }
        }

        public override Type Type
        {
            get
            {
                return typeof(object);
            }
        }

        public Slot Frame
        {
            get { return _frame; }
        }

        public SymbolId Name
        {
            get { return _name; }
        }

        public override string ToString()
        {
            return string.Format("LocalNamedFromSlot Name: ({0}) From: {1}", SymbolTable.IdToString(_name), _frame);
        }
    }
}
