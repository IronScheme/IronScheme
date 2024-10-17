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
using System.Collections.Generic;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Ast
{
    public class IfStatementBuilder {
        private readonly List<IfStatementTest> _clauses = new List<IfStatementTest>();
        private SourceSpan _statementSpan;

        internal IfStatementBuilder(SourceSpan statementSpan) {
            _statementSpan = statementSpan;
        }

        public IfStatementBuilder ElseIf(Expression test, params Statement[] body) {
            Contract.RequiresNotNullItems(body, "body");
            return ElseIf(SourceSpan.None, test, SourceLocation.None, Ast.Block(body));
        }

        public IfStatementBuilder ElseIf(Expression test, Statement body) {
            return ElseIf(SourceSpan.None, test, SourceLocation.None, body);
        }

        public IfStatementBuilder ElseIf(SourceSpan span, Expression test, SourceLocation bodyLocation, Statement body) {
            Contract.RequiresNotNull(test, "test");
            Contract.Requires(test.Type == typeof(bool), "test");
            Contract.RequiresNotNull(body, "body");
            _clauses.Add(Ast.IfCondition(span, bodyLocation, test, body));
            return this;
        }

        public IfStatement Else(params Statement[] body) {
            Contract.RequiresNotNullItems(body, "body");
            return Else(Ast.Block(body));
        }

        public IfStatement Else(Statement body) {
            Contract.RequiresNotNull(body, "body");
            return new IfStatement(
                _statementSpan,
                CollectionUtils.ToReadOnlyCollection(_clauses.ToArray()),
                body
            );
        }

        public IfStatement ToStatement() {
            return new IfStatement(
                _statementSpan,
                CollectionUtils.ToReadOnlyCollection(_clauses.ToArray()),
                null
            );
        }

        public static implicit operator IfStatement(IfStatementBuilder builder) {
            Contract.RequiresNotNull(builder, "builder");
            return builder.ToStatement();
        }
    }

    public static partial class Ast {
        public static IfStatementBuilder If(SourceSpan span) {
            return new IfStatementBuilder(span);
        }

        public static IfStatementBuilder If(Expression test, params Statement[] body) {
            return If(SourceSpan.None).ElseIf(test, body);
        }

        public static IfStatementBuilder If(Expression test, Statement body) {
            return If(SourceSpan.None).ElseIf(test, body);
        }

        public static IfStatementBuilder If(SourceSpan statementSpan, SourceSpan testSpan, Expression test, SourceLocation header, Statement body) {
            return If(statementSpan).ElseIf(testSpan, test, header, body);
        }

        public static IfStatement If(IfStatementTest[] tests, Statement @else) {
            return If(SourceSpan.None, tests, @else);
        }

        public static IfStatement If(SourceSpan span, IfStatementTest[] tests, Statement @else) {
            Contract.RequiresNotNullItems(tests, "tests");
            return new IfStatement(span, CollectionUtils.ToReadOnlyCollection(tests), @else);
        }

        public static IfStatement IfThen(Expression test, Statement body) {
            return IfThenElse(SourceSpan.None, test, body, null);
        }

        public static IfStatement IfThen(Expression test, params Statement[] body) {
            return IfThenElse(SourceSpan.None, test, Block(body), null);
        }

        public static IfStatement IfThen(SourceSpan statementSpan, Expression test, Statement body) {
            return IfThenElse(statementSpan, test, body, null);
        }

        public static IfStatement IfThenElse(Expression test, Statement body, Statement @else) {
            return IfThenElse(SourceSpan.None, test, body, @else);
        }

        public static IfStatement IfThenElse(SourceSpan span, Expression test, Statement body, Statement @else) {
            return If(
                span,
                new IfStatementTest[] {
                    Ast.IfCondition(SourceSpan.None, SourceLocation.None, test, body)
                },
                @else
            );
        }

        public static IfStatement Unless(Expression test, Statement body) {
            return IfThenElse(SourceSpan.None, test, Ast.Empty(), body);
        }

        public static IfStatement Unless(SourceSpan span, Expression test, Statement body) {
            return IfThenElse(span, test, Ast.Empty(), body);
        }
    }
}
