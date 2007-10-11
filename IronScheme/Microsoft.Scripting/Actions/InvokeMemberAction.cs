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
using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Utils;
using Microsoft.Scripting.Generation;

namespace Microsoft.Scripting.Actions {

    // TODO: Ruby specific flags?
    [Flags]
    public enum InvokeMemberActionFlags {
        None = 0,
        ReturnNonCallable = 1,  // ??
        IsCallWithThis = 2      // HasExplicitTarget
    }

    public class InvokeMemberAction : MemberAction, IEquatable<InvokeMemberAction> {
        private readonly InvokeMemberActionFlags _flags;
        private readonly CallSignature _signature;

        public bool ReturnNonCallable { get { return (_flags & InvokeMemberActionFlags.ReturnNonCallable) != 0; } }
        public bool HasExplicitTarget { get { return (_flags & InvokeMemberActionFlags.IsCallWithThis) != 0; } }
        public CallSignature Signature { get { return _signature; } }

        protected InvokeMemberAction(SymbolId memberName, InvokeMemberActionFlags flags, CallSignature signature)
            : base(memberName) {
            _flags = flags;
            _signature = signature;
        }

        public static InvokeMemberAction Make(SymbolId memberName, InvokeMemberActionFlags flags, CallSignature signature) {
            return new InvokeMemberAction(memberName, flags, signature);
        }

        public override DynamicActionKind Kind {
            get { return DynamicActionKind.InvokeMember; }
        }

        public override bool Equals(object obj) {
            return Equals(obj as InvokeMemberAction);
        }

        public override int GetHashCode() {
            return _signature.GetHashCode() ^ (int)_flags;
        }

        public override string ToString() {
            return String.Format("{0}{1} {2}", base.ToString(), _signature, _flags);
        }


        #region IEquatable<InvokeMemberAction> Members

        public bool Equals(InvokeMemberAction other) {
            if (other == null) return false;
            return Name == other.Name && _flags == other._flags && _signature.Equals(other._signature);
        }

        #endregion
    }
}
