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

using Microsoft.Scripting.Generation;

namespace Microsoft.Scripting {
    public class OperatorMapping {
        public Operators Operator;
        public bool IsUnary;
        public bool IsBinary;
        public bool IsTernary;
        public bool IsReversed;
        public bool IsReversable;

        public OperatorMapping(Operators op, bool isUnary, bool isBinary, bool isTernary) {
            Operator = op;
            IsUnary = isUnary;
            IsBinary = isBinary;
            IsTernary = isTernary;
        }

        public OperatorMapping(Operators op, bool isUnary, bool isBinary, bool isTernary, bool isReversable) {
            Operator = op;
            IsUnary = isUnary;
            IsBinary = isBinary;
            IsTernary = isTernary;
            IsReversable = isReversable;
        }

        public OperatorMapping GetReversed() {
            Debug.Assert(IsReversable);

            OperatorMapping res;
            if (Operator != Operators.Power) {
                res = new OperatorMapping(CompilerHelpers.OperatorToReverseOperator(Operator),
                    IsUnary,
                    IsBinary,
                    IsTernary,
                    IsReversable);
            } else {
                res = new OperatorMapping(CompilerHelpers.OperatorToReverseOperator(Operator),
                    IsUnary,
                    IsBinary,
                    false,
                    IsReversable);
            }
            res.IsReversed = !IsReversed;
            return res;
        }

        public override bool Equals(object obj) {
            OperatorMapping other = obj as OperatorMapping;
            if (other == null) return false;

            return other.Operator == this.Operator &&
                other.IsUnary == this.IsUnary &&
                other.IsBinary == this.IsBinary &&
                other.IsTernary == this.IsTernary;
        }

        public int MinArgs {
            get {
                if (IsUnary) return 1;
                if (IsBinary) return 2;
                if (IsTernary) return 3;
                return 0;
            }
        }

        public int MaxArgs {
            get {
                if (IsTernary) return 3;
                if (IsBinary) return 2;
                if (IsUnary) return 1;
                return 0;
            }
        }
        public override int GetHashCode() {
            return ((int)Operator) |
                (IsUnary ? 0x40000000 : 0) |
                (IsBinary ? 0x20000000 : 0) |
                (IsTernary ? 0x10000000 : 0);
        }
    }
}
