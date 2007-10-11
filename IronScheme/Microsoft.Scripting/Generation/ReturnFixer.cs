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
using System.Diagnostics;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Generation {
    public sealed class ReturnFixer {
        private readonly Slot _argSlot;
        private readonly Slot _refSlot;

        private ReturnFixer(Slot refSlot, Slot argSlot) {
            Debug.Assert(refSlot.Type.IsGenericType && refSlot.Type.GetGenericTypeDefinition() == typeof(StrongBox<>));
            Debug.Assert(argSlot.Type.IsByRef);
            this._refSlot = refSlot;
            this._argSlot = argSlot;
        }

        public static ReturnFixer EmitArgument(CodeGen cg, Slot argSlot) {
            argSlot.EmitGet(cg);
            if (argSlot.Type.IsByRef) {
                Type elementType = argSlot.Type.GetElementType();
                Type concreteType = typeof(StrongBox<>).MakeGenericType(elementType);
                Slot refSlot = cg.GetLocalTmp(concreteType);
                cg.EmitLoadValueIndirect(elementType);
                cg.EmitNew(concreteType, new Type[] { elementType });
                refSlot.EmitSet(cg);
                refSlot.EmitGet(cg);
                return new ReturnFixer(refSlot, argSlot);
            } else {
                cg.EmitBoxing(argSlot.Type);
                return null;
            }
        }

        public void FixReturn(CodeGen cg) {
            _argSlot.EmitGet(cg);
            _refSlot.EmitGet(cg);
            cg.EmitCall(typeof(RuntimeHelpers).GetMethod("GetBox").MakeGenericMethod(_argSlot.Type.GetElementType()));
            cg.EmitStoreValueIndirect(_argSlot.Type.GetElementType());
        }
    }
}
