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
    /// <summary>
    /// A TypeSlot is an item that gets stored in a type's dictionary.  Slots provide an 
    /// opportunity to customize access at runtime when a value is get or set from a dictionary.
    /// </summary>
    public class DynamicTypeSlot {
        /// <summary>
        /// Gets the value stored in the slot for the given instance. 
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1007:UseGenericsWhereAppropriate")]
        public virtual bool TryGetValue(CodeContext context, object instance, DynamicMixin owner, out object value) {
            value = null;
            return false;
        }

        /// <summary>
        /// Gets the value stored in the slot for the given instance, bound to the instance if possible
        /// </summary>
        public virtual bool TryGetBoundValue(CodeContext context, object instance, DynamicMixin owner, out object value) {
            return TryGetValue(context, instance, owner, out value);
        }

        /// <summary>
        /// Sets the value of the slot for the given instance.
        /// </summary>
        /// <returns>true if the value was set, false if it can't be set</returns>
        public virtual bool TrySetValue(CodeContext context, object instance, DynamicMixin owner, object value) {
            return false;
        }

        /// <summary>
        /// Deletes the value stored in the slot from the instance.
        /// </summary>
        /// <returns>true if the value was deleted, false if it can't be deleted</returns>
        public virtual bool TryDeleteValue(CodeContext context, object instance, DynamicMixin owner) {            
            object dummy;
            return DynamicHelpers.GetDynamicType(this).TryInvokeBinaryOperator(context,
                Operators.DeleteDescriptor,
                this,
                instance ?? owner,
                out dummy);
        }

        public virtual bool IsVisible(CodeContext context, DynamicMixin owner) {
            return true;
        }

        public virtual bool IsSetDescriptor(CodeContext context, DynamicMixin owner) {
            return false;
        }
    }
}
