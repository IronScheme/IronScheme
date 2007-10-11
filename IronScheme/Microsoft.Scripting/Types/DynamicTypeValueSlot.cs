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
using System.Collections.Generic;
using System.Text;

namespace Microsoft.Scripting.Types {
    public class DynamicTypeValueSlot : DynamicTypeSlot {
        private object _value;

        public DynamicTypeValueSlot(object value) {
            _value = value;
        }

        public override bool TryGetValue(CodeContext context, object instance, DynamicMixin owner, out object value) {
            value = _value;
            return true;
        }

        public override bool TryDeleteValue(CodeContext context, object instance, DynamicMixin owner) {
            if (instance == null) {
                //!!! remove ValueSlot from dictionary
            } 
            return false;            
        }

        protected object Value {
            get {
                return _value;
            }
        }
    }
}
