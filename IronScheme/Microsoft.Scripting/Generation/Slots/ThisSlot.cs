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
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Generation.Slots
{
    /// <summary>
    /// Accessing self
    /// </summary>
    internal class ThisSlot : Slot
    {
        private readonly Type _type;

        public ThisSlot(Type type)
        {
            _type = type;
        }
        public override void EmitGet(CodeGen cg)
        {
            Contract.RequiresNotNull(cg, "cg");

            cg.EmitThis();
        }

        public override void EmitSet(CodeGen cg)
        {
            throw new InvalidOperationException(Resources.InvalidOperation_SetThis);
        }

        public override void EmitGetAddr(CodeGen cg)
        {
            Contract.RequiresNotNull(cg, "cg");

            cg.Emit(OpCodes.Ldarga, 0);
        }

        public override Type Type
        {
            get
            {
                return _type;
            }
        }

        public override string ToString()
        {
            return string.Format("ThisSlot Type: {0}", _type.FullName);
        }
    }
}
