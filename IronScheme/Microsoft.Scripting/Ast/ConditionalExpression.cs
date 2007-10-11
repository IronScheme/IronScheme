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
using System.Reflection.Emit;
using System.Diagnostics;

using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Ast {
    public class ConditionalExpression : Expression {
        private readonly Expression _test;
        private readonly Expression _true;
        private readonly Expression _false;
        private readonly Type _expressionType;

        internal ConditionalExpression(Expression testExpression, Expression trueExpression, Expression falseExpression, bool allowUpcast) {
            _test = testExpression;
            _true = trueExpression;
            _false = falseExpression;

            if (_true.Type.IsAssignableFrom(_false.Type)) {
                _expressionType = _true.Type;
            } else if (_false.Type.IsAssignableFrom(_true.Type)) {
                _expressionType = _false.Type;
            } else if (allowUpcast) {
                _expressionType = typeof(object);
                _true = Ast.Convert(_true, _expressionType);
                _false = Ast.Convert(_false, _expressionType);
            } else {
                throw new ArgumentException(String.Format("Cannot determine the type of the conditional expression: {0}, {1}.", _true.Type, _false.Type));
            }
        }

        public Expression FalseExpression {
            get { return _false; }
        }

        public Expression Test {
            get { return _test; }
        }

        public Expression TrueExpression {
            get { return _true; }
        }

        public override Type Type {
            get {
                return _expressionType;
            }
        }

        protected override object DoEvaluate(CodeContext context) {
            object ret = _test.Evaluate(context);
            if (context.LanguageContext.IsTrue(ret)) {
                return _true.Evaluate(context);
            } else {
                return _false.Evaluate(context);
            }
        }

        internal override object EvaluateAssign(CodeContext context, object value) {
            object ret = _test.Evaluate(context);
            if (context.LanguageContext.IsTrue(ret)) {
                return _true.EvaluateAssign(context, value);
            } else {
                return _false.EvaluateAssign(context, value);
            }
        }

        public override void Emit(CodeGen cg) {
            Label eoi = cg.DefineLabel();
            Label next = cg.DefineLabel();
            _test.EmitAs(cg, typeof(bool));
            cg.Emit(OpCodes.Brfalse, next);
            _true.EmitCast(cg, _expressionType);
            cg.Emit(OpCodes.Br, eoi);
            cg.MarkLabel(next);
            _false.EmitCast(cg, _expressionType);
            cg.MarkLabel(eoi);
        }

        internal override void EmitAddress(CodeGen cg, Type asType) {
            Label eoi = cg.DefineLabel();
            Label next = cg.DefineLabel();
            _test.EmitAs(cg, typeof(bool));
            cg.Emit(OpCodes.Brfalse, next);
            _true.EmitAddress(cg, asType);
            cg.Emit(OpCodes.Br, eoi);
            cg.MarkLabel(next);
            _false.EmitAddress(cg, asType);
            cg.MarkLabel(eoi);
        }

        public override void Walk(Walker walker) {
            if (walker.Walk(this)) {
                _test.Walk(walker);
                _true.Walk(walker);
                _false.Walk(walker);
            }
            walker.PostWalk(this);
        }
    }

    public static partial class Ast {
        public static ConditionalExpression Condition(Expression test, Expression trueValue, Expression falseValue) {
            return Condition(test, trueValue, falseValue, false);
        }

        /// <summary>
        /// AllowUpcast: casts both expressions to Object if neither is a subtype of the other.
        /// </summary>
        public static ConditionalExpression Condition(Expression test, Expression trueValue, Expression falseValue, bool allowUpcast) {
            Contract.RequiresNotNull(test, "test");
            Contract.RequiresNotNull(trueValue, "trueValue");
            Contract.RequiresNotNull(falseValue, "falseValue");

            return new ConditionalExpression(test, trueValue, falseValue, allowUpcast);
        }
    }
}