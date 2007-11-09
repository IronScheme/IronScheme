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

namespace Microsoft.Scripting.Generation {
    public interface ILazySlotFactory<T> {
        Slot GetConcreteSlot(CodeGen cg, T data);
    }


    public class LazySlot<T> : Slot {
        private ILazySlotFactory<T> _factory;
        private T _data;
        private Type _type;

        public LazySlot(ILazySlotFactory<T> factory, Type type, T data) {
            _factory = factory;
            _data = data;
            _type = type;
        }

        public override void EmitGet(CodeGen cg) {
            _factory.GetConcreteSlot(cg, _data).EmitGet(cg);
        }

        public override void EmitGetAddr(CodeGen cg) {
            _factory.GetConcreteSlot(cg, _data).EmitGetAddr(cg);
        }

        public override void EmitSet(CodeGen cg) {
            _factory.GetConcreteSlot(cg, _data).EmitSet(cg);
        }

        public override void EmitSet(CodeGen cg, Slot val) {
            _factory.GetConcreteSlot(cg, _data).EmitSet(cg, val);
        }

        public override Type Type {
            get { return _type; }
        }
    }
}
