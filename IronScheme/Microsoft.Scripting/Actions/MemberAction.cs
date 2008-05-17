
#if FULL
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

namespace Microsoft.Scripting.Actions {
    public abstract class MemberAction : DynamicAction, IEquatable<MemberAction> {
        private SymbolId _name;

        public SymbolId Name {
            get { return _name; }
        }

        protected MemberAction(SymbolId name) {
            _name = name;
        }

        public override bool Equals(object other) {
            return Equals(other as MemberAction);
        }

        public override int GetHashCode() {
            return (int)Kind << 28 ^ _name.GetHashCode();
        }

        public override string ToString() {
            return base.ToString() + " " + SymbolTable.IdToString(_name);
        }

        #region IEquatable<MemberAction> Members

        public bool Equals(MemberAction other) {
            if (other == null) return false;
            return _name == other._name && Kind == other.Kind;
        }

        #endregion
    }
}

#endif	
