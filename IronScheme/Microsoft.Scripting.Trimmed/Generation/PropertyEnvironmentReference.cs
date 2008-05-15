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
using System.Diagnostics;

namespace Microsoft.Scripting.Generation {    
    class PropertyEnvironmentReference : Storage {
        private int _index;
        private Type _tupleType;
        private Type _type;

        public PropertyEnvironmentReference(Type tupleType, int index, Type type) {
            Debug.Assert(tupleType != null);
            Debug.Assert(index < Tuple.GetSize(tupleType));

            _tupleType = tupleType;
            _index = index;
            _type = type;
        }

        public override bool RequireAccessSlot {
            get { return true; }
        }

        public override Slot CreateSlot(Slot instance) {            
            Slot slot = instance;
            Type curType = null;
            foreach (PropertyInfo pi in Tuple.GetAccessPath(_tupleType, _index)) {
                slot = new PropertySlot(slot, pi);
                curType = pi.PropertyType;
            }
            if (_type != curType) {
                slot = new CastSlot(slot, _type);
            }
            return slot;
        }
    }    
}
