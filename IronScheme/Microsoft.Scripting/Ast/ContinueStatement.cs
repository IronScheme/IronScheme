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
using Microsoft.Scripting.Generation;

namespace Microsoft.Scripting.Ast {
    public class ContinueStatement : Statement {
        private Statement _statement;

        internal ContinueStatement(SourceSpan span, Statement statement)
            : base(AstNodeType.ContinueStatement, span) {
            _statement = statement;
        }

        public Statement Statement {
            get { return _statement; }
            set { _statement = value; }
        }

        public override void Emit(CodeGen cg) {
            //cg.EmitPosition(Start, End);

            if (_statement != null) {
                cg.CheckAndPushTargets(_statement);
            }

            cg.EmitContinue();

            if (_statement != null) {
                cg.PopTargets();
            }
        }


#if FULL
        protected override object DoExecute(CodeContext context) {
            return Statement.Continue;
        } 
#endif

    }

    public static partial class Ast {
        public static ContinueStatement Continue() {
            return Continue(SourceSpan.None, null);
        }

        public static ContinueStatement Continue(SourceSpan span) {
            return Continue(span, null);
        }

        /// <param name="statement">The statement the label is pointing to (not the label itself).</param>
        public static ContinueStatement Continue(Statement statement) {
            return Continue(SourceSpan.None, statement);
        }

        public static ContinueStatement Continue(SourceSpan span, Statement statement) {
            return new ContinueStatement(span, statement);
        }
    }
}
