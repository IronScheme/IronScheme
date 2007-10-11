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
using System.Reflection.Emit;
using System.Collections.Generic;

using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Ast {
    /// <summary>
    /// Expression that represents list of expressions.
    /// The value of the CommaExpression is the expression identified by the index
    /// </summary>
    public class CommaExpression : Expression {
        private IList<Expression> _expressions;
        private int _valueIndex;

        internal CommaExpression(int valueIndex, IList<Expression> expressions) {
            _expressions = expressions;
            _valueIndex = valueIndex;
        }

        public IList<Expression> Expressions {
            get { return _expressions; }
        }

        public int ValueIndex {
            get { return _valueIndex; }
        }

        /// <summary>
        /// The expression type is the type of the expression being selected.
        /// </summary>
        public override Type Type {
            get {
                return _expressions[_valueIndex].Type;
            }
        }

        public override void EmitBranchTrue(CodeGen cg, Label label) {
            if (_valueIndex == _expressions.Count - 1) {
                Emit(cg, _valueIndex);
                _expressions[_valueIndex].EmitBranchTrue(cg, label);
            } else {
                base.EmitBranchTrue(cg, label);
            }
        }

        public override void EmitBranchFalse(CodeGen cg, Label label) {
            if (_valueIndex == _expressions.Count - 1) {
                Emit(cg, _valueIndex);
                _expressions[_valueIndex].EmitBranchFalse(cg, label);
            } else {
                base.EmitBranchFalse(cg, label);
            }
        }

        public override void Emit(CodeGen cg) {
            Emit(cg, _expressions.Count);
        }

        private void Emit(CodeGen cg, int count) {
            for (int index = 0; index < count; index++) {
                Expression current = _expressions[index];

                // Emit the expression
                current.Emit(cg);

                // If we don't want the expression just emitted as the result,
                // pop it off of the stack, unless it is a void expression.
                if (index != _valueIndex && current.Type != typeof(void)) {
                    cg.Emit(OpCodes.Pop);
                }
            }
        }

        protected override object DoEvaluate(CodeContext context) {
            object result = null;
            for (int index = 0; index < _expressions.Count; index++) {
                Expression current = _expressions[index];

                if (current != null) {
                    object val = current.Evaluate(context);
                    if (index == _valueIndex) {
                        // Save the value at the designated index
                        result = val;
                    }
                }
            }
            return result;
        }

        internal override object EvaluateAssign(CodeContext context, object value) {
            object result = null;
            for (int index = 0; index < _expressions.Count; index++) {
                Expression current = _expressions[index];

                if (current != null) {
                    if (index == _valueIndex) {
                        result = current.EvaluateAssign(context, value);
                    } else {
                        current.Evaluate(context);
                    }
                }
            }
            return result;
        }

        internal override void EmitAddress(CodeGen cg, Type asType) {
            for (int index = 0; index < _expressions.Count; index++) {
                Expression current = _expressions[index];

                // Emit the expression
                if (index == _valueIndex) {
                    current.EmitAddress(cg, asType);
                } else {
                    current.Emit(cg);
                    // If we don't want the expression just emitted as the result,
                    // pop it off of the stack, unless it is a void expression.
                    if (current.Type != typeof(void)) {
                        cg.Emit(OpCodes.Pop);
                    }
                }
            }
        }

        public override void Walk(Walker walker) {
            if (walker.Walk(this)) {
                foreach (Expression e in _expressions) {
                    e.Walk(walker);
                }
            }
            walker.PostWalk(this);
        }
    }

    public static partial class Ast {
        /// <summary>
        /// Creates a list of expressions whose value is the value of the last expression.
        /// </summary>
        public static CommaExpression Comma(params Expression[] expressions) {
            return Comma(-1, expressions);
        }

        /// <summary>
        /// Creates a list of expressions whose value is the value of the last expression.
        /// </summary>
        public static CommaExpression Comma(IList<Expression> expressions) {
            return Comma(-1, expressions);
        }

        /// <summary>
        /// Creates a list of expressions whose value is the value of the <paramref name="valueIndex"/>-th expression.
        /// A negative <paramref name="valueIndex"/> is equivalent to <paramref name="expressions"/>.Length + <paramref name="valueIndex"/> 
        /// (hence -1 designates the last expression specified).
        /// </summary>
        public static CommaExpression Comma(int valueIndex, params Expression[] expressions) {
            return Comma(valueIndex, (IList<Expression>)expressions);
        }

        /// <summary>
        /// Creates a list of expressions whose value is the value of the <paramref name="valueIndex"/>-th expression.
        /// A negative <paramref name="valueIndex"/> is equivalent to <paramref name="expressions"/>.Count + <paramref name="valueIndex"/> 
        /// (hence -1 designates the last expression specified).
        /// </summary>
        public static CommaExpression Comma(int valueIndex, IList<Expression> expressions) {
            Contract.RequiresNotEmpty(expressions, "expressions");
            Contract.RequiresNotNullItems(expressions, "expressions");

            if (valueIndex < 0) {
                valueIndex += expressions.Count;
            }

            Contract.RequiresArrayIndex(expressions, valueIndex, "valueIndex");

            return new CommaExpression(valueIndex, expressions);
        }
    }
}

