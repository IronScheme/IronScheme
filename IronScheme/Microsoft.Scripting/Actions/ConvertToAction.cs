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

namespace Microsoft.Scripting.Actions {

    public class ConvertToAction : DynamicAction, IEquatable<ConvertToAction> {
        private Type _type;

        public static ConvertToAction Make(Type type) {
            return new ConvertToAction(type);
        }

        private ConvertToAction(Type type) { this._type = type; }

        public Type ToType { get { return _type; } }
        public override DynamicActionKind Kind { get { return DynamicActionKind.ConvertTo; } }

        public override bool Equals(object obj) {
            return Equals(obj as ConvertToAction);
        }

        public override int GetHashCode() {
            return (int)Kind << 28 ^ _type.GetHashCode();
        }

        public override string ToString() {
            return base.ToString() + " to " + _type.ToString();
        }

        #region IEquatable<ConvertToAction> Members

        public bool Equals(ConvertToAction other) {
            if (other == null) return false;
            return _type == other._type;
        }

        #endregion
    }

}
