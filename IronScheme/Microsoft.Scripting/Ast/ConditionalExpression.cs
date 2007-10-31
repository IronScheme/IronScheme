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
        private readonly Expression/*!*/ _test;
        private readonly Expression/*!*/ _true;
        private readonly Expression/*!*/ _false;
        private readonly Type/*!*/ _expressionType;

        internal ConditionalExpression(Expression/*!*/ test, Expression/*!*/ ifTrue, Expression/*!*/ ifFalse, Type/*!*/ type)
            : base(AstNodeType.Conditional) {
            _test = test;
            _true = ifTrue;
            _false = ifFalse;
            _expressionType = type;
        }

        public Expression Test {
            get { return _test; }
        }

        public Expression IfTrue {
            get { return _true; }
        }

        public Expression IfFalse {
            get { return _false; }
        }

        public override Type Type {
            get {
                return _expressionType;
            }
        }

        protected override object DoEvaluate(CodeContext context) {
            object ret = _test.Evaluate(context);
            if ((bool)ret) {
                return _true.Evaluate(context);
            } else {
                return _false.Evaluate(context);
            }
        }

        internal override EvaluationAddress EvaluateAddress(CodeContext context) {
            object ret = _test.Evaluate(context);

            if ((bool)ret) {
                return _true.EvaluateAddress(context);
            } else {
                return _false.EvaluateAddress(context);
            }
        }

        public override void Emit(CodeGen cg) {
            Label eoi = cg.DefineLabel();
            Label next = cg.DefineLabel();
            _test.Emit(cg);
            cg.Emit(OpCodes.Brfalse, next);
            _true.Emit(cg);
            cg.Emit(OpCodes.Br, eoi);
            cg.MarkLabel(next);
            _false.Emit(cg);
            cg.MarkLabel(eoi);
        }

        internal override void EmitAddress(CodeGen cg, Type asType) {
            Label eoi = cg.DefineLabel();
            Label next = cg.DefineLabel();
            _test.Emit(cg);
            cg.Emit(OpCodes.Brfalse, next);
            _true.EmitAddress(cg, asType);
            cg.Emit(OpCodes.Br, eoi);
            cg.MarkLabel(next);
            _false.EmitAddress(cg, asType);
            cg.MarkLabel(eoi);
        }
    }

    public static partial class Ast {
        public static ConditionalExpression Condition(Expression test, Expression ifTrue, Expression ifFalse) {
            Contract.RequiresNotNull(test, "test");
            Contract.RequiresNotNull(ifTrue, "ifTrue");
            Contract.RequiresNotNull(ifFalse, "ifFalse");

            Contract.Requires(test.Type == typeof(bool), "test", "Test must be bool");
            Contract.Requires(ifTrue.Type == ifFalse.Type, "ifTrue", "Types must match");

            return new ConditionalExpression(test, ifTrue, ifFalse, ifTrue.Type);
        }
    }
}