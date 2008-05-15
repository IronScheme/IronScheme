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
using System.Text;
using Microsoft.Scripting.Ast;
using System.Diagnostics;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Actions {
    public class CallAction : DynamicAction, IEquatable<CallAction> {
        private readonly CallSignature _signature;

        private static readonly CallAction[] _cached = new CallAction[] { 
            new CallAction(new CallSignature(0)),
            new CallAction(new CallSignature(1)),
            new CallAction(new CallSignature(2)),
            new CallAction(new CallSignature(3)),
            new CallAction(new CallSignature(4))
        };

        protected CallAction(CallSignature callSignature) {
            _signature = callSignature;
        }

        public static CallAction Make(CallSignature signature) {
            return new CallAction(signature);
        }

        public static CallAction Make(int argumentCount) {
            Contract.Requires(argumentCount >= 0, "argumentCount");
            if (argumentCount < _cached.Length) return _cached[argumentCount];
            return new CallAction(new CallSignature(argumentCount));
        }

        public CallSignature Signature {
            get { return _signature; }
        }

        public override DynamicActionKind Kind {
            get { return DynamicActionKind.Call; }
        }

        public bool Equals(CallAction other) {
            if (other == null || other.GetType() != GetType()) return false;
            return _signature.Equals(other._signature);
        }

        public override bool Equals(object obj) {
            return Equals(obj as CallAction);
        }

        public override int GetHashCode() {
            return (int)Kind << 28 ^ _signature.GetHashCode();
        }

        public override string ToString() {
            return base.ToString() + _signature.ToString();
        }
    }
}


