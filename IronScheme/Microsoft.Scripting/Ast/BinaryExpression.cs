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
using System.Diagnostics;
using System.Reflection;
using System.Reflection.Emit;

using Microsoft.Scripting.Utils;
using Microsoft.Scripting.Generation;

namespace Microsoft.Scripting.Ast {
    public class BinaryExpression : Expression {
        private readonly Expression /*!*/ _left;
        private readonly Expression /*!*/ _right;

        internal BinaryExpression(AstNodeType nodeType, Expression /*!*/ left, Expression /*!*/ right)
            : base(nodeType) {
            _left = left;
            _right = right;
        }

        public Expression Right {
            get { return _right; }
        }

        public Expression Left {
            get { return _left; }
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Maintainability", "CA1502:AvoidExcessiveComplexity")] // TODO: Fix by holding onto the type directly
        public override Type Type {
            get {
                switch (NodeType) {
                    case AstNodeType.Equal:
                    case AstNodeType.NotEqual:
                    case AstNodeType.GreaterThan:
                    case AstNodeType.LessThan:
                    case AstNodeType.LessThanOrEqual:
                    case AstNodeType.GreaterThanOrEqual:
                    case AstNodeType.AndAlso:
                    case AstNodeType.OrElse:
                        return typeof(bool);

                    case AstNodeType.Add:
                    case AstNodeType.AddChecked:
                    case AstNodeType.Multiply:
                    case AstNodeType.MultiplyChecked:
                    case AstNodeType.Subtract:
                    case AstNodeType.SubtractChecked:
                    case AstNodeType.Modulo:
                    case AstNodeType.Divide:
                    case AstNodeType.And:
                    case AstNodeType.Or:
                    case AstNodeType.ExclusiveOr:
                    case AstNodeType.LeftShift:
                    case AstNodeType.RightShift:
                        return _left.Type;

                    default:
                        throw new InvalidOperationException();
                }
            }
        }

        public override bool IsConstant(object value) {
            if (value == null) return false;

            switch (NodeType) {
                case AstNodeType.AndAlso:
                    if (value.Equals(true)) {
                        return _left.IsConstant(true) && _right.IsConstant(true);
                    }
                    if (value.Equals(false)) {
                        // if left isn't a constant it has to be evaluated
                        return _left.IsConstant(false);
                    }
                    break;

                case AstNodeType.OrElse:
                    if (value.Equals(true)) {
                        return _left.IsConstant(true);
                    }
                    if (value.Equals(false)) {
                        return _left.IsConstant(false) && _right.IsConstant(false);
                    }
                    break;
            }
            return false;
        }

        private bool EmitBranchTrue(CodeGen cg, AstNodeType nodeType, Label label) {
            switch (nodeType) {
                case AstNodeType.Equal:
                    if (_left.IsConstant(null)) {
                        _right.EmitAsObject(cg);
                        cg.Emit(OpCodes.Brfalse, label);
                    } else if (_right.IsConstant(null)) {
                        _left.EmitAsObject(cg);
                        cg.Emit(OpCodes.Brfalse, label);
                    } else {
                        _left.EmitAs(cg, GetEmitType());
                        _right.EmitAs(cg, GetEmitType());
                        cg.Emit(OpCodes.Beq, label);
                    }
                    return true;

                case AstNodeType.NotEqual:
                    if (_left.IsConstant(null)) {
                        _right.EmitAsObject(cg);
                        cg.Emit(OpCodes.Brtrue, label);
                    } else if (_right.IsConstant(null)) {
                        _left.EmitAsObject(cg);
                        cg.Emit(OpCodes.Brtrue, label);
                    } else {
                        _left.EmitAs(cg, GetEmitType());
                        _right.EmitAs(cg, GetEmitType());
                        cg.Emit(OpCodes.Ceq);
                        cg.Emit(OpCodes.Brfalse, label);
                    }
                    return true;

                case AstNodeType.AndAlso:
                    // if (left AND right) branch label

                    // if (left) then 
                    //   if (right) branch label
                    // endif
                    Label endif = cg.DefineLabel();
                    _left.EmitBranchFalse(cg, endif);
                    _right.EmitBranchTrue(cg, label);
                    cg.MarkLabel(endif);
                    return true;

                case AstNodeType.OrElse:
                    // if (left OR right) branch label

                    // if (left) then branch label endif
                    // if (right) then branch label endif
                    _left.EmitBranchTrue(cg, label);
                    _right.EmitBranchTrue(cg, label);
                    return true;
                default:
                    return false;
            }
        }

        private Type GetEmitType() {
            return _left.Type == _right.Type ? _left.Type : typeof(object);
        }

        public override void EmitBranchFalse(CodeGen cg, Label label) {
            switch (NodeType) {
                case AstNodeType.Equal:
                    EmitBranchTrue(cg, AstNodeType.NotEqual, label);
                    break;

                case AstNodeType.NotEqual:
                    EmitBranchTrue(cg, AstNodeType.Equal, label);
                    break;

                case AstNodeType.AndAlso:
                    // if NOT (left AND right) branch label

                    if (_left.IsConstant(false)) {
                        cg.Emit(OpCodes.Br, label);
                    } else {
                        if (!_left.IsConstant(true)) {
                            _left.EmitBranchFalse(cg, label);
                        }

                        if (_right.IsConstant(false)) {
                            cg.Emit(OpCodes.Br, label);
                        } else if (!_right.IsConstant(true)) {
                            _right.EmitBranchFalse(cg, label);
                        }
                    }
                    break;

                case AstNodeType.OrElse:
                    // if NOT left AND NOT right branch label

                    if (!_left.IsConstant(true) && !_right.IsConstant(true)) {
                        if (_left.IsConstant(false)) {
                            _right.EmitBranchFalse(cg, label);
                        } else if (_right.IsConstant(false)) {
                            _left.EmitBranchFalse(cg, label);
                        } else {
                            // if (NOT left) then 
                            //   if (NOT right) branch label
                            // endif

                            Label endif = cg.DefineLabel();
                            _left.EmitBranchTrue(cg, endif);
                            _right.EmitBranchFalse(cg, label);
                            cg.MarkLabel(endif);
                        }
                    }
                    break;

                default:
                    base.EmitBranchFalse(cg, label);
                    break;
            }
        }

        public override void EmitBranchTrue(CodeGen cg, Label label) {
            if (!EmitBranchTrue(cg, NodeType, label)) {
                base.EmitBranchTrue(cg, label);
            }
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Maintainability", "CA1502:AvoidExcessiveComplexity")]
        public override void Emit(CodeGen cg) {

            // TODO: code gen will be suboptimal for chained AndAlsos and AndAlso inside If
            if (NodeType == AstNodeType.AndAlso || NodeType == AstNodeType.OrElse) {
                EmitBooleanOperator(cg, NodeType == AstNodeType.AndAlso);
                return;
            }

            _left.EmitAs(cg, GetEmitType());
            _right.EmitAs(cg, GetEmitType());

            switch (NodeType) {
                case AstNodeType.Equal:
                    cg.Emit(OpCodes.Ceq);
                    break;

                case AstNodeType.NotEqual:
                    cg.Emit(OpCodes.Ceq);
                    cg.EmitInt(0);
                    cg.Emit(OpCodes.Ceq);
                    break;

                case AstNodeType.GreaterThan:
                    cg.Emit(OpCodes.Cgt);
                    break;

                case AstNodeType.LessThan:
                    cg.Emit(OpCodes.Clt);
                    break;

                case AstNodeType.GreaterThanOrEqual:
                    cg.Emit(OpCodes.Clt);
                    cg.EmitInt(0);
                    cg.Emit(OpCodes.Ceq);
                    break;

                case AstNodeType.LessThanOrEqual:
                    cg.Emit(OpCodes.Cgt);
                    cg.EmitInt(0);
                    cg.Emit(OpCodes.Ceq);
                    break;
                case AstNodeType.Multiply:
                    cg.Emit(OpCodes.Mul);
                    break;
                case AstNodeType.MultiplyChecked:
                    cg.Emit(OpCodes.Mul_Ovf);
                    break;
                case AstNodeType.Modulo:
                    cg.Emit(OpCodes.Rem);
                    break;
                case AstNodeType.Add:
                    cg.Emit(OpCodes.Add);
                    break;
                case AstNodeType.AddChecked:
                    cg.Emit(OpCodes.Add_Ovf);
                    break;
                case AstNodeType.Subtract:
                    cg.Emit(OpCodes.Sub);
                    break;
                case AstNodeType.SubtractChecked:
                    cg.Emit(OpCodes.Sub_Ovf);
                    break;
                case AstNodeType.Divide:
                    cg.Emit(OpCodes.Div);
                    break;
                case AstNodeType.LeftShift:
                    cg.Emit(OpCodes.Shl);
                    break;
                case AstNodeType.RightShift:
                    cg.Emit(OpCodes.Shr);
                    break;
                case AstNodeType.And:
                    cg.Emit(OpCodes.And);
                    break;
                case AstNodeType.Or:
                    cg.Emit(OpCodes.Or);
                    break;
                case AstNodeType.ExclusiveOr:
                    cg.Emit(OpCodes.Xor);
                    break;
                default:
                    throw new InvalidOperationException(NodeType.ToString());
            }
        }

        private void EmitBooleanOperator(CodeGen cg, bool isAnd) {
            Label otherwise = cg.DefineLabel();
            Label endif = cg.DefineLabel();

            // if (_left) 
            _left.EmitBranchFalse(cg, otherwise);
            // then

            if (isAnd) {
                _right.EmitAs(cg, typeof(bool));
            } else {
                cg.EmitInt(1);
            }

            cg.Emit(OpCodes.Br, endif);
            // otherwise
            cg.MarkLabel(otherwise);

            if (isAnd) {
                cg.EmitInt(0);
            } else {
                _right.EmitAs(cg, typeof(bool));
            }

            // endif
            cg.MarkLabel(endif);
            return;
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Maintainability", "CA1502:AvoidExcessiveComplexity")]
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1800:DoNotCastUnnecessarily")]
        protected override object DoEvaluate(CodeContext context) {
            if (NodeType == AstNodeType.AndAlso) {
                object ret = _left.Evaluate(context);
                return ((bool)ret) ? _right.Evaluate(context) : ret;
            } else if (NodeType == AstNodeType.OrElse) {
                object ret = _left.Evaluate(context);
                return ((bool)ret) ? ret : _right.Evaluate(context);
            }

            object l = _left.Evaluate(context);
            object r = _right.Evaluate(context);

            switch (NodeType) {
                case AstNodeType.GreaterThan:
                    return RuntimeHelpers.BooleanToObject(((IComparable)l).CompareTo(r) > 0);
                case AstNodeType.LessThan:
                    return RuntimeHelpers.BooleanToObject(((IComparable)l).CompareTo(r) < 0);
                case AstNodeType.GreaterThanOrEqual:
                    return RuntimeHelpers.BooleanToObject(((IComparable)l).CompareTo(r) >= 0);
                case AstNodeType.LessThanOrEqual:
                    return RuntimeHelpers.BooleanToObject(((IComparable)l).CompareTo(r) <= 0);
                case AstNodeType.Equal:
                    return RuntimeHelpers.BooleanToObject(TestEquals(l, r));

                case AstNodeType.NotEqual:
                    return RuntimeHelpers.BooleanToObject(!TestEquals(l, r));

                case AstNodeType.MultiplyChecked:
                case AstNodeType.Multiply: return EvalMultiply(l, r);
                case AstNodeType.AddChecked:
                case AstNodeType.Add: return EvalAdd(l, r);
                case AstNodeType.SubtractChecked:
                case AstNodeType.Subtract: return EvalSub(l, r);
                case AstNodeType.Divide: return EvalDiv(l, r);
                case AstNodeType.Modulo: return EvalMod(l, r);
                case AstNodeType.And: return EvalAnd(l, r);
                case AstNodeType.Or: return EvalOr(l, r);
                case AstNodeType.ExclusiveOr: return EvalXor(l, r);
                default:
                    throw new NotImplementedException(NodeType.ToString());
            }
        }

        private static object EvalMultiply(object l, object r) {
            if (l is int) return (int)l * (int)r;
            if (l is uint) return (uint)l * (uint)r;
            if (l is short) return (short)((short)l * (short)r);
            if (l is ushort) return (ushort)((ushort)l * (ushort)r);
            if (l is long) return (long)l * (long)r;
            if (l is ulong) return (ulong)l * (ulong)r;
            if (l is float) return (float)l * (float)r;
            if (l is double) return (double)l * (double)r;
            throw new InvalidOperationException("multiply: {0} " + CompilerHelpers.GetType(l).Name);
        }

        private static object EvalAdd(object l, object r) {
            if (l is int) return (int)l + (int)r;
            if (l is uint) return (uint)l + (uint)r;
            if (l is short) return (short)((short)l + (short)r);
            if (l is ushort) return (ushort)((ushort)l + (ushort)r);
            if (l is long) return (long)l + (long)r;
            if (l is ulong) return (ulong)l + (ulong)r;
            if (l is float) return (float)l + (float)r;
            if (l is double) return (double)l + (double)r;
            throw new InvalidOperationException("add: {0} " + CompilerHelpers.GetType(l).Name);
        }

        private static object EvalSub(object l, object r) {
            if (l is int) return (int)l - (int)r;
            if (l is uint) return (uint)l - (uint)r;
            if (l is short) return (short)((short)l - (short)r);
            if (l is ushort) return (ushort)((ushort)l - (ushort)r);
            if (l is long) return (long)l - (long)r;
            if (l is ulong) return (ulong)l - (ulong)r;
            if (l is float) return (float)l - (float)r;
            if (l is double) return (double)l - (double)r;
            throw new InvalidOperationException("sub: {0} " + CompilerHelpers.GetType(l).Name);
        }

        private static object EvalMod(object l, object r) {
            if (l is int) return (int)l % (int)r;
            if (l is uint) return (uint)l % (uint)r;
            if (l is short) return (short)((short)l % (short)r);
            if (l is ushort) return (ushort)((ushort)l % (ushort)r);
            if (l is long) return (long)l % (long)r;
            if (l is ulong) return (ulong)l % (ulong)r;
            if (l is float) return (float)l % (float)r;
            if (l is double) return (double)l % (double)r;
            throw new InvalidOperationException("mod: {0} " + CompilerHelpers.GetType(l).Name);
        }

        private static object EvalDiv(object l, object r) {
            if (l is int) return (int)l / (int)r;
            if (l is uint) return (uint)l / (uint)r;
            if (l is short) return (short)((short)l / (short)r);
            if (l is ushort) return (ushort)((ushort)l / (ushort)r);
            if (l is long) return (long)l / (long)r;
            if (l is ulong) return (ulong)l / (ulong)r;
            if (l is float) return (float)l / (float)r;
            if (l is double) return (double)l / (double)r;
            throw new InvalidOperationException("div: {0} " + CompilerHelpers.GetType(l).Name);
        }

        private static object EvalAnd(object l, object r) {
            if (l is int) return (int)l & (int)r;
            if (l is uint) return (uint)l & (uint)r;
            if (l is short) return (short)((short)l & (short)r);
            if (l is ushort) return (ushort)((ushort)l & (ushort)r);
            if (l is long) return (long)l & (long)r;
            if (l is ulong) return (ulong)l & (ulong)r;
            throw new InvalidOperationException("and: {0} " + CompilerHelpers.GetType(l).Name);
        }

        private static object EvalOr(object l, object r) {
            if (l is int) return (int)l | (int)r;
            if (l is uint) return (uint)l | (uint)r;
            if (l is short) return (short)((short)l | (short)r);
            if (l is ushort) return (ushort)((ushort)l | (ushort)r);
            if (l is long) return (long)l | (long)r;
            if (l is ulong) return (ulong)l | (ulong)r;
            throw new InvalidOperationException("or: {0} " + CompilerHelpers.GetType(l).Name);
        }

        private static object EvalXor(object l, object r) {
            if (l is int) return (int)l ^ (int)r;
            if (l is uint) return (uint)l ^ (uint)r;
            if (l is short) return (short)((short)l ^ (short)r);
            if (l is ushort) return (ushort)((ushort)l ^ (ushort)r);
            if (l is long) return (long)l ^ (long)r;
            if (l is ulong) return (ulong)l ^ (ulong)r;
            throw new InvalidOperationException("xor: {0} " + CompilerHelpers.GetType(l).Name);
        }

        private bool TestEquals(object l, object r) {
            // We don't need to go through the same type checks as the emit case,
            // since we know we're always dealing with boxed objects.

            return Object.Equals(l, r);
        }
    }

    public static partial class Ast {
        public static BinaryExpression Equal(Expression left, Expression right) {
            Contract.RequiresNotNull(left, "left");
            Contract.RequiresNotNull(right, "right");

            return new BinaryExpression(AstNodeType.Equal, left, right);
        }

        public static BinaryExpression NotEqual(Expression left, Expression right) {
            Contract.RequiresNotNull(left, "left");
            Contract.RequiresNotNull(right, "right");

            return new BinaryExpression(AstNodeType.NotEqual, left, right);
        }

        public static BinaryExpression GreaterThan(Expression left, Expression right) {
            return MakeBinaryComparisonExpression(AstNodeType.GreaterThan, left, right);
        }

        public static BinaryExpression LessThan(Expression left, Expression right) {
            return MakeBinaryComparisonExpression(AstNodeType.LessThan, left, right);
        }

        public static BinaryExpression GreaterThanEquals(Expression left, Expression right) {
            return MakeBinaryComparisonExpression(AstNodeType.GreaterThanOrEqual, left, right);
        }

        public static BinaryExpression LessThanEquals(Expression left, Expression right) {
            return MakeBinaryComparisonExpression(AstNodeType.LessThanOrEqual, left, right);
        }

        #region Boolean Expressions

        public static BinaryExpression AndAlso(Expression left, Expression right) {
            return LogicalBinary(AstNodeType.AndAlso, left, right);
        }

        public static BinaryExpression OrElse(Expression left, Expression right) {
            return LogicalBinary(AstNodeType.OrElse, left, right);
        }

        private static BinaryExpression LogicalBinary(AstNodeType nodeType, Expression left, Expression right) {
            Contract.RequiresNotNull(left, "left");
            Contract.RequiresNotNull(right, "right");
            Contract.Requires(TypeUtils.IsBool(left.Type), "left");
            Contract.Requires(TypeUtils.IsBool(right.Type), "right");
            Contract.Requires(left.Type == right.Type);

            return new BinaryExpression(nodeType, left, right);
        }

        #endregion

        #region Coalescing Expressions

        /// <summary>
        /// Null coalescing expression (LINQ).
        /// {result} ::= ((tmp = {_left}) == null) ? {right} : tmp
        /// '??' operator in C#.
        /// </summary>
        public static Expression Coalesce(CodeBlock currentBlock, Expression left, Expression right) {
            return CoalesceInternal(currentBlock, left, right, null, false);
        }

        /// <summary>
        /// True coalescing expression.
        /// {result} ::= IsTrue(tmp = {left}) ? {right} : tmp
        /// Generalized AND semantics.
        /// </summary>
        public static Expression CoalesceTrue(CodeBlock currentBlock, Expression left, Expression right, MethodInfo isTrue) {
            Contract.RequiresNotNull(isTrue, "isTrue");
            return CoalesceInternal(currentBlock, left, right, isTrue, false);
        }

        /// <summary>
        /// False coalescing expression.
        /// {result} ::= IsTrue(tmp = {left}) ? tmp : {right}
        /// Generalized OR semantics.
        /// </summary>
        public static Expression CoalesceFalse(CodeBlock currentBlock, Expression left, Expression right, MethodInfo isTrue) {
            Contract.RequiresNotNull(isTrue, "isTrue");
            return CoalesceInternal(currentBlock, left, right, isTrue, true);
        }

        private static Expression CoalesceInternal(CodeBlock currentBlock, Expression left, Expression right, MethodInfo isTrue, bool isReverse) {
            Contract.RequiresNotNull(currentBlock, "currentBlock");
            Contract.RequiresNotNull(left, "left");
            Contract.RequiresNotNull(right, "right");

            // A bit too strict, but on a safe side.
            Contract.Requires(left.Type == right.Type, "Expression types must match");

            Variable tmp = currentBlock.CreateTemporaryVariable(SymbolTable.StringToId("tmp_left"), left.Type);

            Expression condition;
            if (isTrue != null) {
                Contract.Requires(isTrue.ReturnType == typeof(bool), "isTrue", "Predicate must return bool.");
                ParameterInfo[] parameters = isTrue.GetParameters();
                Contract.Requires(parameters.Length == 1, "isTrue", "Predicate must take one parameter.");
                Contract.Requires(isTrue.IsStatic && isTrue.IsPublic, "isTrue", "Predicate must be public and static.");

                Type pt = parameters[0].ParameterType;
                Contract.Requires(TypeUtils.CanAssign(pt, left.Type), "left", "Incorrect left expression type");
                condition = Call(isTrue, Assign(tmp, left));
            } else {
                Contract.Requires(TypeUtils.CanCompareToNull(left.Type), "left", "Incorrect left expression type");
                condition = Equal(Assign(tmp, left), Null(left.Type));
            }

            Expression t, f;
            if (isReverse) {
                t = Read(tmp);
                f = right;
            } else {
                t = right;
                f = Read(tmp);
            }

            return Condition(condition, t, f);
        }

        #endregion

        /// <summary>
        /// Adds two arithmetic values of the same type.
        /// </summary>
        public static BinaryExpression Add(Expression left, Expression right) {
            return MakeBinaryArithmeticExpression(AstNodeType.Add, left, right);
        }

        /// <summary>
        /// Adds two arithmetic values of the same type.
        /// </summary>
        public static BinaryExpression AddChecked(Expression left, Expression right)
        {
          return MakeBinaryArithmeticExpression(AstNodeType.AddChecked, left, right);
        }

        /// <summary>
        /// Subtracts two arithmetic values of the same type.
        /// </summary>
        public static BinaryExpression Subtract(Expression left, Expression right) {
            return MakeBinaryArithmeticExpression(AstNodeType.Subtract, left, right);
        }


        /// <summary>
        /// Subtracts two arithmetic values of the same type.
        /// </summary>
        public static BinaryExpression SubtractChecked(Expression left, Expression right)
        {
          return MakeBinaryArithmeticExpression(AstNodeType.SubtractChecked, left, right);
        }

        /// <summary>
        /// Divides two arithmetic values of the same type.
        /// </summary>
        public static BinaryExpression Divide(Expression left, Expression right) {
            return MakeBinaryArithmeticExpression(AstNodeType.Divide, left, right);
        }

        /// <summary>
        /// Modulos two arithmetic values of the same type.
        /// </summary>
        public static BinaryExpression Modulo(Expression left, Expression right) {
            return MakeBinaryArithmeticExpression(AstNodeType.Modulo, left, right);
        }

        /// <summary>
        /// Multiples two arithmetic values of the same type.
        /// </summary>
        public static BinaryExpression Multiply(Expression left, Expression right) {
            return MakeBinaryArithmeticExpression(AstNodeType.Multiply, left, right);
        }

        /// <summary>
        /// Multiples two arithmetic values of the same type.
        /// </summary>
        public static BinaryExpression MultiplyChecked(Expression left, Expression right)
        {
          return MakeBinaryArithmeticExpression(AstNodeType.MultiplyChecked, left, right);
        }


        /// <summary>
        /// Left shifts one arithmetic value by another aritmetic value of the same type.
        /// </summary>
        public static BinaryExpression LeftShift(Expression left, Expression right) {
            return MakeBinaryArithmeticExpression(AstNodeType.LeftShift, left, right);
        }

        /// <summary>
        /// Right shifts one arithmetic value by another aritmetic value of the same type.
        /// </summary>
        public static BinaryExpression RightShift(Expression left, Expression right) {
            return MakeBinaryArithmeticExpression(AstNodeType.RightShift, left, right);
        }

        /// <summary>
        /// Performs bitwise and of two values of the same type.
        /// </summary>
        public static BinaryExpression And(Expression left, Expression right) {
            return MakeBinaryArithmeticExpression(AstNodeType.And, left, right);
        }

        /// <summary>
        /// Performs bitwise or of two values of the same type.
        /// </summary>
        public static BinaryExpression Or(Expression left, Expression right) {
            return MakeBinaryArithmeticExpression(AstNodeType.Or, left, right);
        }

        /// <summary>
        /// Performs exclusive or of two values of the same type.
        /// </summary>
        public static BinaryExpression ExclusiveOr(Expression left, Expression right) {
            return MakeBinaryArithmeticExpression(AstNodeType.ExclusiveOr, left, right);
        }

        private static BinaryExpression MakeBinaryArithmeticExpression(AstNodeType nodeType, Expression left, Expression right) {
            Contract.RequiresNotNull(left, "left");
            Contract.RequiresNotNull(left, "right");
            if (left.Type != right.Type || !TypeUtils.IsArithmetic(left.Type)) {
                throw new NotSupportedException(String.Format("{0} only supports identical arithmetic types, got {1} {2}", nodeType, left.Type.Name, right.Type.Name));
            }

            return new BinaryExpression(nodeType, left, right);
        }

        private static BinaryExpression MakeBinaryComparisonExpression(AstNodeType nodeType, Expression left, Expression right) {
            Contract.RequiresNotNull(left, "left");
            Contract.RequiresNotNull(left, "right");
            if (left.Type != right.Type || !TypeUtils.IsNumeric(left.Type)) {
                throw new NotSupportedException(String.Format("{0} only supports identical numeric types, got {1} {2}", nodeType, left.Type.Name, right.Type.Name));
            }

            return new BinaryExpression(nodeType, left, right);
        }
    }
}
