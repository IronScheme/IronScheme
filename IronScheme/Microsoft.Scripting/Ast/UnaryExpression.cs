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
using Microsoft.Scripting.Generation;
using System.Diagnostics;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Ast {
    public enum UnaryOperators {
        Convert,
        Not,
        Negate,
        OnesComplement
    }

    public class UnaryExpression : Expression {
        private readonly Expression _operand;
        private readonly UnaryOperators _op;
        private readonly Type _type;

        internal UnaryExpression(Expression expression, UnaryOperators op, Type type) {

            _operand = expression;
            _op = op;
            _type = type;
        }

        public Expression Operand {
            get { return _operand; }
        }

        public override Type Type {
            get { return _type; }
        }

        public UnaryOperators Operator {
            get { return _op; }
        }

        public override void Emit(CodeGen cg) {
            switch (_op) {
                case UnaryOperators.Convert:
                    _operand.Emit(cg);
                    cg.EmitCast(_operand.Type, _type);
                    break;

                case UnaryOperators.Not:
                    _operand.Emit(cg);
                    if (_operand.Type == typeof(bool)) {
                        cg.Emit(OpCodes.Ldc_I4_0);
                        cg.Emit(OpCodes.Ceq);
                    } else {
                        cg.Emit(OpCodes.Not);
                    }
                    break;
                case UnaryOperators.Negate:
                    _operand.Emit(cg);
                    cg.Emit(OpCodes.Neg);
                    break;
                case UnaryOperators.OnesComplement:
                    _operand.Emit(cg);
                    cg.Emit(OpCodes.Not);
                    break;
                default:
                    throw new NotImplementedException();
            }
        }

        internal override void EmitAddress(CodeGen cg, Type asType) {
            if (_op == UnaryOperators.Convert && Type == asType) {
                _operand.EmitAddress(cg, asType);
            } else {
                base.EmitAddress(cg, asType);
            }
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1800:DoNotCastUnnecessarily")]
        protected override object DoEvaluate(CodeContext context) {
            object x = _operand.Evaluate(context);
            switch (_op) {
                case UnaryOperators.Convert:
                    return Cast.Explicit(x, _type);

                case UnaryOperators.Not:
                    if (x is bool) return (bool)x ? RuntimeHelpers.False : RuntimeHelpers.True;
                    if (x is int) return (int)~(int)x;
                    if (x is long) return (long)~(long)x;
                    if (x is short) return (short)~(short)x;
                    if (x is uint) return (uint)~(uint)x;
                    if (x is ulong) return (ulong)~(ulong)x;
                    if (x is ushort) return (ushort)~(ushort)x;
                    if (x is byte) return (byte)~(byte)x;
                    if (x is sbyte) return (sbyte)~(sbyte)x;
                    throw new InvalidOperationException("can't perform unary not on type " + CompilerHelpers.GetType(x).Name);

                case UnaryOperators.Negate:
                    if (x is int) return (int)(-(int)x);
                    if (x is long) return (long)(-(long)x);
                    if (x is short) return (short)(-(short)x);
                    if (x is float) return -(float)x;
                    if (x is double) return -(double)x;
                    throw new InvalidOperationException("can't negate type " + CompilerHelpers.GetType(x).Name);
                default:
                    throw new NotImplementedException();
            }
        }

        public override void Walk(Walker walker) {
            if (walker.Walk(this)) {
                _operand.Walk(walker);
            }
            walker.PostWalk(this);
        }
    }

    /// <summary>
    /// Factory methods.
    /// </summary>
    public static partial class Ast {
        public static UnaryExpression Convert(Expression expression, Type type) {
            Contract.RequiresNotNull(expression, "expression");
            Contract.RequiresNotNull(type, "type");
            if (!type.IsVisible) throw new ArgumentException(String.Format(Resources.TypeMustBeVisible, type.FullName));

            return new UnaryExpression(expression, UnaryOperators.Convert, type);
        }

        public static Expression ConvertHelper(Expression expression, Type type) {
            Contract.RequiresNotNull(expression, "expression");
            Contract.RequiresNotNull(type, "type");

            if (expression.Type != type) {
                expression = Convert(expression, type);
            }
            return expression;
        }

        public static UnaryExpression Negate(Expression expression) {
            Contract.RequiresNotNull(expression, "expression");

            return new UnaryExpression(expression, UnaryOperators.Negate, expression.Type);
        }

        public static UnaryExpression Not(Expression expression) {
            Contract.RequiresNotNull(expression, "expression");
            Contract.Requires(TypeUtils.IsIntegerOrBool(expression.Type), "expression", "Expression type must be integer or boolean.");

            return new UnaryExpression(expression, UnaryOperators.Not, expression.Type);
        }
    }
}
