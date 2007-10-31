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

using System.Reflection.Emit;
using Microsoft.Scripting.Generation;
using System;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Ast {
    public class DoStatement : Statement {
        private readonly SourceLocation _header;
        private readonly Expression /*!*/ _test;
        private readonly Statement /*!*/ _body;

        /// <summary>
        /// Called by <see cref="DoStatementBuilder"/>.
        /// </summary>
        internal DoStatement(SourceSpan span, SourceLocation header, Expression /*!*/ test, Statement /*!*/ body)
            : base(AstNodeType.DoStatement, span) {
            _header = header;
            _test = test;
            _body = body;
        }

        public SourceLocation Header {
            get { return _header; }
        }

        public Expression Test {
            get { return _test; }
        }

        public Statement Body {
            get { return _body; }
        }                

        protected override object DoExecute(CodeContext context) {
            object ret = NextStatement;
            
            do {
                ret = _body.Execute(context);
                if (ret == Statement.Break) {
                    break;
                } else if (!(ret is ControlFlow)) {
                    return ret;
                }
            } while (context.LanguageContext.IsTrue(_test.Evaluate(context)));

            return NextStatement;
        }

        public override void Emit(CodeGen cg) {
            Label startTarget = cg.DefineLabel();
            Label breakTarget = cg.DefineLabel();
            Label continueTarget = cg.DefineLabel();

            cg.MarkLabel(startTarget);                        
            cg.PushTargets(breakTarget, continueTarget, this);
            _body.Emit(cg);

            cg.MarkLabel(continueTarget);
            // TODO: Check if we need to emit position somewhere else also.
            cg.EmitPosition(Start, _header);

            _test.Emit(cg);
            cg.Emit(OpCodes.Brtrue, startTarget);

            cg.PopTargets();            
            cg.MarkLabel(breakTarget);
        }
    }
}
