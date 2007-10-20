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
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Ast {
    public class IfStatementBuilder {
        private List<IfStatementTest> _clauses;
        private SourceSpan _statementSpan;

        internal IfStatementBuilder(SourceSpan statementSpan, SourceSpan conditionSpan, Expression condition, SourceLocation header, Statement body) {
            Contract.RequiresNotNull(condition, "condition");
            Contract.RequiresNotNull(body, "body");

            _clauses = CollectionUtils.MakeList(new IfStatementTest(conditionSpan, header, condition, body));
            _statementSpan = statementSpan;
        }

        public IfStatementBuilder ElseIf(Expression condition, params Statement[] body) {
            return ElseIf(SourceSpan.None, condition, SourceLocation.None, Ast.Block(body));
        }

        public IfStatementBuilder ElseIf(Expression condition, Statement body) {
            return ElseIf(SourceSpan.None, condition, SourceLocation.None, body);
        }

        public IfStatementBuilder ElseIf(SourceSpan span, Expression condition, SourceLocation bodyLocation, Statement body) {
            Contract.RequiresNotNull(body, "body");

            _clauses.Add(new IfStatementTest(span, bodyLocation, condition, body));
            return this;
        }

        public IfStatement Else(params Statement[] body) {
            return Else(Ast.Block(body));
        }

        public IfStatement Else(Statement body) {
            Contract.RequiresNotNull(body, "body");

            return new IfStatement(_statementSpan, _clauses.ToArray(), body);
        }

        public static implicit operator IfStatement(IfStatementBuilder builder) {
            return ToStatement(builder);
        }

        public static IfStatement ToStatement(IfStatementBuilder builder) {
            Contract.RequiresNotNull(builder, "builder");
            return new IfStatement(builder._statementSpan, builder._clauses.ToArray(), null);
        }
    }

    public static partial class Ast {
        public static IfStatementBuilder If(Expression condition, params Statement[] body) {
            return new IfStatementBuilder(SourceSpan.None, SourceSpan.None, condition, SourceLocation.None, Ast.Block(body));
        }

        public static IfStatementBuilder If(Expression condition, Statement body) {
            return new IfStatementBuilder(SourceSpan.None, SourceSpan.None, condition, SourceLocation.None, body);
        }

        public static IfStatementBuilder If(SourceSpan statementSpan, SourceSpan conditionSpan, Expression condition, SourceLocation header, Statement body) {
            return new IfStatementBuilder(statementSpan, conditionSpan, condition, header, body);
        }


        public static IfStatement If(IfStatementTest[] tests, Statement @else) {
            return If(SourceSpan.None, tests, @else);
        }

        public static IfStatement If(SourceSpan span, IfStatementTest[] tests, Statement @else) {
            return new IfStatement(span, tests, @else);
        }

        public static IfStatement IfThen(Expression condition, Statement body) {
            return IfThenElse(SourceSpan.None, condition, body, null);
        }

        public static IfStatement IfThen(Expression condition, params Statement[] body) {
            return IfThenElse(SourceSpan.None, condition, Block(body), null);
        }

        public static IfStatement IfThen(SourceSpan statementSpan, Expression condition, Statement body) {
            return IfThenElse(statementSpan, condition, body, null);
        }

        public static IfStatement IfThenElse(Expression condition, Statement body, Statement @else) {
            return IfThenElse(SourceSpan.None, condition, body, @else);
        }

        public static IfStatement IfThenElse(SourceSpan statementSpan, Expression condition, Statement body, Statement @else) {
            return new IfStatement(
                statementSpan,
                new IfStatementTest[] {
                    new IfStatementTest(SourceSpan.None, SourceLocation.None, condition, body)
                },
                @else
            );
        }

        public static IfStatement Unless(Expression condition, Statement body) {
            return IfThenElse(SourceSpan.None, condition, Ast.Empty(), body);
        }

        public static IfStatement Unless(SourceSpan span, Expression condition, Statement body) {
            return IfThenElse(span, condition, Ast.Empty(), body);
        }
    }
}
