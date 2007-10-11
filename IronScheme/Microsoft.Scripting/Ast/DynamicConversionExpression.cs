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
using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Ast {
    // TODO: Remove! Use ActionExpression instead.
    public class DynamicConversionExpression : Expression {
        private readonly Expression _expression;
        private readonly Type _conversion;

        internal DynamicConversionExpression(Expression expression, Type conversion) {
            _expression = expression;
            _conversion = conversion;
        }

        public override Type Type {
            get {
                return _conversion;
            }
        }

        public override void Emit(CodeGen cg) {
            _expression.Emit(cg);
            cg.EmitConvert(_expression.Type, _conversion);
        }

        protected override object DoEvaluate(CodeContext context) {
            object value = _expression.Evaluate(context);
            return context.LanguageContext.Binder.Convert(value, _conversion);
        }

        public override void Walk(Walker walker) {
            if (walker.Walk(this)) {
                _expression.Walk(walker);
            }
            walker.PostWalk(this);
        }
    }
    public static partial class Ast {
        public static DynamicConversionExpression DynamicConvert(Expression expression, Type conversion) {
            Contract.RequiresNotNull(expression, "expression");
            Contract.RequiresNotNull(conversion, "conversion");

            return new DynamicConversionExpression(expression, conversion);
        }
    }
}
