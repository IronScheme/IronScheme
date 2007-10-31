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
using Microsoft.Scripting.Utils;
namespace Microsoft.Scripting.Ast {
    public class IfStatementTest : Node {
        private readonly SourceLocation _start;
        private readonly SourceLocation _header;
        private readonly SourceLocation _end;

        private readonly Expression /*!*/ _test;
        private readonly Statement /*!*/ _body;

        internal IfStatementTest(SourceSpan span, SourceLocation header, Expression /*!*/ test, Statement /*!*/ body)
            : base(AstNodeType.IfStatementTest) {
            _test = test;
            _body = body;
            _header = header;
            _start = span.Start;
            _end = span.End;
        }

        public SourceLocation Start {
            get { return _start; }
        }

        public SourceLocation Header {
            get { return _header; }
        }

        public SourceLocation End {
            get { return _end; }
        }

        public SourceSpan Span {
            get {
                return new SourceSpan(_start, _end);
            }
        }

        public Expression Test {
            get { return _test; }
        }

        public Statement Body {
            get { return _body; }
        }
    }

    public static partial class Ast {
        public static IfStatementTest IfCondition(Expression test, Statement body) {
            return IfCondition(SourceSpan.None, SourceLocation.None, test, body);
        }

        public static IfStatementTest IfCondition(SourceSpan span, SourceLocation header, Expression test, Statement body) {
            Contract.RequiresNotNull(test, "test");
            Contract.RequiresNotNull(body, "body");
            Contract.Requires(test.Type == typeof(bool), "test", "Test must be boolean");

            return new IfStatementTest(span, header, test, body);
        }

        public static IfStatementTest[] IfConditions(params IfStatementTest[] tests) {
            Contract.RequiresNotNullItems(tests, "tests");
            return tests;
        }
    }
}
