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
    /// Local variable access
    /// Note that access of local variables of an enclosing function is done using a FieldSlot
    /// </summary>
    public class LocalSlot : Slot
    {
        private readonly LocalBuilder _localBuilder;

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1823:AvoidUnusedPrivateFields")]
        private readonly CodeGen _codeGen;           // LocalSlot's can only be used w/ codegen that created them

        public LocalSlot(LocalBuilder localBuilder, CodeGen cg)
        {
            _localBuilder = localBuilder;
            _codeGen = cg;
        }
        public override void EmitGet(CodeGen cg)
        {
            Contract.RequiresNotNull(cg, "cg");

            Debug.Assert(cg == _codeGen);

            cg.Emit(OpCodes.Ldloc, _localBuilder);
        }
        public override void EmitGetAddr(CodeGen cg)
        {
            Contract.RequiresNotNull(cg, "cg");

            Debug.Assert(cg == _codeGen);

            cg.Emit(OpCodes.Ldloca, _localBuilder);
        }

        public override void EmitSet(CodeGen cg)
        {
            Contract.RequiresNotNull(cg, "cg");

            Debug.Assert(cg == _codeGen);
            cg.Emit(OpCodes.Stloc, _localBuilder);
        }

        public override Type Type
        {
            get { return _localBuilder.LocalType; }
        }

        /// <summary>
        /// Gets the LocalBuilder that this Slot emits a local for
        /// </summary>
        public LocalBuilder LocalBuilder
        {
            get { return _localBuilder; }
        }

        public override string ToString()
        {
            return string.Format("LocalSlot Index: {0} Type {1}", _localBuilder.LocalIndex, _localBuilder.LocalType.FullName);
        }
    }
}
