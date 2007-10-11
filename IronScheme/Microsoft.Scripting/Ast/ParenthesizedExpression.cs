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

namespace Microsoft.Scripting.Ast {
    public class ParenthesizedExpression : Expression {
        private readonly SourceLocation _start;
        private readonly SourceLocation _end;
        private readonly Expression _expression;

        internal ParenthesizedExpression(SourceSpan span, Expression expression) {
            Debug.Assert(expression != null);
            _start = span.Start;
            _end = span.End;
            _expression = expression;
        }

        public Expression Expression {
            get { return _expression; }
        }

        public override Type Type {
            get {
                return _expression.Type;
            }
        }

        protected override object DoEvaluate(CodeContext context) {
            return _expression.Evaluate(context);
        }

        public override void Emit(CodeGen cg) {
            _expression.Emit(cg);
        }

        public override void Walk(Walker walker) {
            if (walker.Walk(this)) {
                _expression.Walk(walker);
            }
            walker.PostWalk(this);
        }
    }

    /// <summary>
    /// Factory methods.
    /// </summary>
    public static partial class Ast {
        public static ParenthesizedExpression Parenthesize(Expression expression) {
            return Parenthesize(SourceSpan.None, expression);
        }
        public static ParenthesizedExpression Parenthesize(SourceSpan span, Expression expression) {
            if (expression == null) {
                throw new ArgumentNullException("expression");
            }
            return new ParenthesizedExpression(span, expression);
        }
    }
}
