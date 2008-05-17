
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
using Microsoft.Scripting.Generation;

namespace Microsoft.Scripting.Actions {

    public class DoOperationAction : DynamicAction {
        private readonly Operators _operation;

        public static DoOperationAction Make(Operators operation) {
            return new DoOperationAction(operation);
        }

        private DoOperationAction(Operators operation) {
            _operation = operation;
        }

        public Operators Operation {
            get { return _operation; }
        }

        public override DynamicActionKind Kind {
            get { return DynamicActionKind.DoOperation; }
        }

        public override bool Equals(object obj) {
            DoOperationAction other = obj as DoOperationAction;
            if (other == null) return false;
            return _operation == other._operation;
        }

        public override int GetHashCode() {
            return (int)Kind << 28 ^ ((int)_operation) ;
        }

        //??? Do these belong here or mone Operators enum
        public bool IsComparision {
            get {
                return CompilerHelpers.IsComparisonOperator(_operation);
            }
        }

        public bool IsUnary {
            get {
                switch(_operation){ 
                    case Operators.OnesComplement:
                    case Operators.Negate:
                    case Operators.Positive:
                    case Operators.AbsoluteValue:
                    case Operators.ConvertToBigInteger:
                    case Operators.ConvertToBoolean:
                    case Operators.ConvertToComplex:
                    case Operators.ConvertToDouble:
                    case Operators.ConvertToHex:
                    case Operators.ConvertToInt32:
                    case Operators.ConvertToOctal:
                    case Operators.ConvertToString:
                    case Operators.Not:
                        return true;                    
                }
                return false;
            }
        }

        public bool IsInPlace {
            get {
                return CompilerHelpers.InPlaceOperatorToOperator(_operation) != Operators.None;
            }
        }

        public Operators DirectOperation {
            get {
                Operators res = CompilerHelpers.InPlaceOperatorToOperator(_operation);
                if (res != Operators.None) return res;

                throw new InvalidOperationException();
            }
        }

        public override string ToString() {
            return base.ToString() + " " + _operation.ToString();
        }
    }
}

#endif	
