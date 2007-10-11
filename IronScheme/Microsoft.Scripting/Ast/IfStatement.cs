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
using System.Collections.Generic;
using Microsoft.Scripting.Generation;
using System;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Ast {

    public class IfStatement : Statement {
        private readonly IfStatementTest[] _tests;
        private readonly Statement _else;

        internal IfStatement(SourceSpan span, IfStatementTest[] tests, Statement @else)
            : base(span) {
            Contract.RequiresNotNull(tests, "tests");

            _tests = tests;
            _else = @else;
        }

        public IList<IfStatementTest> Tests {
            get { return _tests; }
        }

        public Statement ElseStatement {
            get { return _else; }
        }

        protected override object DoExecute(CodeContext context) {
            foreach (IfStatementTest t in _tests) {
                object val = t.Test.Evaluate(context);
                if (context.LanguageContext.IsTrue(val)) {
                    return t.Body.Execute(context);
                }
            }
            if (_else != null) {
                return _else.Execute(context);
            }
            return NextStatement;
        }

        public override void Emit(CodeGen cg) {
            Label eoi = cg.DefineLabel();
            foreach (IfStatementTest t in _tests) {
                Label next = cg.DefineLabel();
                cg.EmitPosition(t.Start, t.Header);

                t.Test.EmitBranchFalse(cg, next);

                t.Body.Emit(cg);
                // optimize no else case                
                cg.EmitSequencePointNone();     // hide compiler generated branch.
                cg.Emit(OpCodes.Br, eoi);
                cg.MarkLabel(next);
            }
            if (_else != null) {
                _else.Emit(cg);
            }
            cg.MarkLabel(eoi);
        }

        public override void Walk(Walker walker) {
            if (walker.Walk(this)) {
                foreach (IfStatementTest t in _tests) t.Walk(walker);
                if (_else != null) _else.Walk(walker);
            }
            walker.PostWalk(this);
        }
    }
}
