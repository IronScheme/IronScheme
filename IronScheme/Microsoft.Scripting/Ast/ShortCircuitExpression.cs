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
using System.Reflection;
using System.Reflection.Emit;
using Microsoft.Scripting.Utils;
using Microsoft.Scripting.Generation;

namespace Microsoft.Scripting.Ast {
    public class ShortCircuitExpression : Expression {
        private readonly MethodInfo _testOp;
        private readonly MethodInfo _resultOp;
        private readonly Expression _left, _right;

        internal ShortCircuitExpression(MethodInfo testOp, MethodInfo resultOp, Expression left, Expression right) {
            _testOp = testOp;
            _resultOp = resultOp;
            _left = left;
            _right = right;
        }

        public MethodInfo Test {
            get { return _testOp; }
        }

        public MethodInfo Result {
            get { return _resultOp; }
        }

        public Expression Right {
            get { return _right; }
        }

        public Expression Left {
            get { return _left; }
        }

        public override Type Type {
            get {
                return _resultOp.ReturnType;
            }
        }

        protected override object DoEvaluate(CodeContext context) {
            object left = _left.Evaluate(context);
            if (!((bool)_testOp.Invoke(null, new object[] { left }))) {
                object right = _right.Evaluate(context);
                return _resultOp.Invoke(null, new object[] { left, right });
            } else {
                return left;
            }
        }

        public override void Emit(CodeGen cg) {
            ParameterInfo[] parameters = _resultOp.GetParameters();

            // Emit the left value as the ParameterType required by the
            // resultOp (which is the same as that required by the testOp
            // (see validation in the constructor)
            _left.EmitAs(cg, parameters[0].ParameterType);

            // Call the _testOp. It takes one parameter
            cg.Emit(OpCodes.Dup);
            cg.EmitCall(_testOp);

            // Convert the result to bool
            cg.EmitConvert(_testOp.ReturnType, typeof(bool));

            Label l = cg.DefineLabel();
            cg.Emit(OpCodes.Brtrue, l);

            // Emit the right expression as the required parameter type
            _right.EmitAs(cg, parameters[1].ParameterType);

            // Call the _resultOp method. Its result type is the same
            // as parameters[0].ParameterType so the stack is in consistent
            // state.
            cg.EmitCall(_resultOp);
            cg.MarkLabel(l);
        }

        public override void Walk(Walker walker) {
            if (walker.Walk(this)) {
                _left.Walk(walker);
                _right.Walk(walker);
            }
            walker.PostWalk(this);
        }
    }

    /// <summary>
    /// Factory methods.
    /// </summary>
    public static partial class Ast {
        public static ShortCircuitExpression ShortCircuit(MethodInfo test, MethodInfo result, Expression left, Expression right) {
            Contract.RequiresNotNull(test, "test");
            Contract.RequiresNotNull(result, "result");
            Contract.RequiresNotNull(left, "left");
            Contract.RequiresNotNull(right, "right");

            ParameterInfo[] parameters;
            // The test must be public static
            if ((test.Attributes & (MethodAttributes.Public | MethodAttributes.Static)) !=
                (MethodAttributes.Public | MethodAttributes.Static)) {
                throw new ArgumentException("test must be public and static");
            }
            // And take exactly one parameter
            parameters = test.GetParameters();
            if (parameters.Length != 1) {
                throw new ArgumentException("test must have exactly one parameter");
            }
            Type testType = parameters[0].ParameterType;

            // The result must be public static also
            if ((result.Attributes & (MethodAttributes.Public | MethodAttributes.Static)) !=
                (MethodAttributes.Public | MethodAttributes.Static)) {
                throw new ArgumentException("result must be public static");
            }

            // And take exactly two parameters
            parameters = result.GetParameters();
            if (parameters.Length != 2) {
                throw new ArgumentException("result must have exactly two parameters");
            }

            // The first result parameter must be of the same type as test parameter
            // This is perhaps too restrictive, but certainly correct.
            if (parameters[0].ParameterType != testType) {
                throw new ArgumentException("test and result must have the same type of the first parameter");
            }

            // Finally, the test parameter type must be the same as the
            // result type of the _resultOp. This ensures that the expression
            // behaves consistently.
            if (testType != result.ReturnType) {
                throw new ArgumentException("test parameter type must be the same as the result return type");
            }

            return new ShortCircuitExpression(test, result, left, right);
        }
    }
}
