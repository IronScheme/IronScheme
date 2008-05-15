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
    /// <summary>
    /// Slot that holds onto the environment for a method.  The environment supports both
    /// getting members and emitting a dictionary which contains the members of the environment.
    /// 
    /// The default base class represents a unified environment which is both a dictionary and
    /// any storage for environment promoted locals.  Today this is only used for compiled modules
    /// that have their locals stored in static fields.
    /// </summary>
    public class EnvironmentSlot : Slot {
        private Slot _storage;

        public EnvironmentSlot(Slot storage) {
            _storage = storage;
        }

        public override void EmitGet(CodeGen cg) {
            _storage.EmitGet(cg);
        }

        public override void EmitGetAddr(CodeGen cg) {
            _storage.EmitGetAddr(cg);
        }

        public override Type Type {
            get { return _storage.Type; }
        }

        public override void EmitSet(CodeGen cg) {
            _storage.EmitSet(cg);
        }

        public override void EmitSet(CodeGen cg, Slot val) {
            _storage.EmitSet(cg, val);
        }

        public virtual void EmitGetDictionary(CodeGen cg) {
            _storage.EmitGet(cg);
        }
    }
}
