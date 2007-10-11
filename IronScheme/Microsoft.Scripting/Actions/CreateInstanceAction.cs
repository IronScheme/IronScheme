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
using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Actions {
    public class CreateInstanceAction : CallAction, IEquatable<CreateInstanceAction> {
        private static readonly CreateInstanceAction[] _cached = new CreateInstanceAction[] { 
            new CreateInstanceAction(new CallSignature(0)),
            new CreateInstanceAction(new CallSignature(1)),
            new CreateInstanceAction(new CallSignature(2)),
            new CreateInstanceAction(new CallSignature(3)),
            new CreateInstanceAction(new CallSignature(4))
        };
        
        protected CreateInstanceAction(CallSignature callSignature)
            : base(callSignature) {
        }

        public static new CreateInstanceAction Make(CallSignature signature) {
            return new CreateInstanceAction(signature);
        }

        public static new CreateInstanceAction Make(int argumentCount) {
            Contract.Requires(argumentCount >= 0, "argumentCount");
            if (argumentCount < _cached.Length) return _cached[argumentCount];
            return new CreateInstanceAction(new CallSignature(argumentCount));
        }

        public override bool Equals(object obj) {
            return Equals(obj as CreateInstanceAction);
        }

        public bool Equals(CreateInstanceAction other) {
            return base.Equals(other);
        }

        public override int GetHashCode() {
            return base.GetHashCode();
        }

        public override DynamicActionKind Kind {
            get {
                return DynamicActionKind.CreateInstance;
            }
        }
    }
}
