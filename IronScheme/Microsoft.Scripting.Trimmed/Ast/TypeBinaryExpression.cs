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
using System.Reflection.Emit;
using Microsoft.Scripting.Utils;
using Microsoft.Scripting.Generation;

namespace Microsoft.Scripting.Ast {
    public class TypeBinaryExpression : Expression {
        private readonly Expression /*!*/ _expression;
        private readonly Type /*!*/ _typeOperand;

        internal TypeBinaryExpression(AstNodeType nodeType, Expression /*!*/ expression, Type /*!*/ typeOperand)
            : base(nodeType) {
            _expression = expression;
            _typeOperand = typeOperand;
        }

        public Expression Expression {
            get { return _expression; }
        }

        public Type TypeOperand {
            get { return _typeOperand; }
        }

        public override Type Type {
            get {
                return typeof(bool);
            }
        }

        public override bool IsConstant(object value) {
            // allow constant TypeIs expressions to be optimized away
            if (value is bool && ((bool)value) == true) {
                return _typeOperand.IsAssignableFrom(_expression.Type);
            }
            return false;
        }

        public override void Emit(CodeGen cg) {
            if (_typeOperand.IsAssignableFrom(_expression.Type)) {
                // if its always true just emit the bool
                cg.EmitConstant(true);
                return;
            }

            _expression.EmitAsObject(cg);
            cg.Emit(OpCodes.Isinst, _typeOperand);
            cg.Emit(OpCodes.Ldnull);
            cg.Emit(OpCodes.Cgt_Un);
        }

        protected override object DoEvaluate(CodeContext context) {
            return RuntimeHelpers.BooleanToObject(
                _typeOperand.IsInstanceOfType(_expression.Evaluate(context)));
        }
    }

    /// <summary>
    /// Factory methods.
    /// </summary>
    public static partial class Ast {
        public static TypeBinaryExpression TypeIs(Expression expression, Type type) {
            Contract.RequiresNotNull(expression, "expression");
            Contract.RequiresNotNull(type, "type");
            Contract.Requires(!type.IsByRef, "type", "type must not be ByRef");

            if (!type.IsVisible) {
                throw new ArgumentException(String.Format(Resources.TypeMustBeVisible, type.FullName));
            }

            return new TypeBinaryExpression(AstNodeType.TypeIs, expression, type);
        }
    }
}
