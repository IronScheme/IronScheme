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

using Microsoft.Scripting.Generation;

namespace Microsoft.Scripting.Ast {
    public class ReturnStatement : Statement {
        private readonly Expression _expr;

        internal ReturnStatement(SourceSpan span, Expression expression)
            : base(AstNodeType.ReturnStatement, span) {
            _expr = expression;
        }

        public Expression Expression {
            get { return _expr; }
        }

        protected override object DoExecute(CodeContext context) {
            if (_expr != null) {
                return _expr.Evaluate(context);
            } else {
                return null;
            }
        }

        public override void Emit(CodeGen cg) {
            cg.EmitPosition(Start, End);
            cg.EmitReturn(_expr);
        }
    }

    /// <summary>
    /// Factory methods.
    /// </summary>
    public static partial class Ast {
        public static ReturnStatement Return() {
            return Return(SourceSpan.None, null);
        }

        public static ReturnStatement Return(Expression expression) {
            return Return(SourceSpan.None, expression);
        }

        public static ReturnStatement Return(SourceSpan span, Expression expression) {
            return new ReturnStatement(span, expression);
        }
    }
}
