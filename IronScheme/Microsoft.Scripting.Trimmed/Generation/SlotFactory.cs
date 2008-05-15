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
using System.Collections.Generic;

namespace Microsoft.Scripting.Generation {

    /// <summary>
    /// Base class for all other slot factories.  Supports creating either strongly typed
    /// slots or slots that are always of type object.
    /// </summary>
    public abstract class SlotFactory {
        private Dictionary<SymbolId, Slot> _fields = new Dictionary<SymbolId, Slot>();

        /// <summary>
        /// Gets or creates a new slot with the given name of the specified type.
        /// </summary>
        public Slot MakeSlot(SymbolId name, Type type) {
            Slot res;

            if (!_fields.TryGetValue(name, out res)) {
                _fields[name] = res = CreateSlot(name, type);
            }

            return res;
        }
 
        /// <summary>
        /// Overriden by the base type.  Creates a new slot of the given name and type.  Only called once for each name.
        /// </summary>
        protected abstract Slot CreateSlot(SymbolId name, Type type);

        /// <summary>
        /// Called before emitting code into the specified CodeGen.  Provides an opportunity to setup any
        /// method-local state for the slot factory.
        /// </summary>
        /// <param name="cg"></param>
        public virtual void PrepareForEmit(CodeGen cg) {
        }

        /// <summary>
        /// Provides all the fields created by the SlotFactory.
        /// </summary>
        public Dictionary<SymbolId, Slot> Fields {
            get {
                return _fields;
            }
        }
    }
}
