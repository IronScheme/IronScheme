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
using System.Collections.Generic;

using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Ast {
    public class SwitchStatementBuilder {
        private readonly SourceSpan _span;
        private readonly SourceLocation _header;
        private Expression _test;
        private readonly List<SwitchCase> _cases = new List<SwitchCase>();
        private bool _default;

        internal SwitchStatementBuilder(SourceSpan span, SourceLocation header, Expression test) {
            _span = span;
            _header = header;
            _test = test;
        }

        public SwitchStatementBuilder Test(Expression test) {
            Contract.RequiresNotNull(test, "test");
            _test = test;
            return this;
        }

        public SwitchStatementBuilder Default(Statement body) {
            return Default(SourceLocation.None, body);
        }

        public SwitchStatementBuilder Default(SourceLocation header, Statement body) {
            Contract.Requires(_default == false, "body", "Already has default clause");
            _cases.Add(Ast.DefaultCase(header, body));
            _default = true;
            return this;
        }

        public SwitchStatementBuilder Case(int value, Statement body) {
            return Case(SourceLocation.None, value, body);
        }

        public SwitchStatementBuilder Case(SourceLocation header, int value, Statement body) {
            _cases.Add(Ast.SwitchCase(header, value, body));
            return this;
        }

        public Statement ToStatement() {
            Contract.Requires(_test != null);
            return Ast.Switch(_span, _header, _test, _cases.ToArray());
        }

        public static implicit operator Statement(SwitchStatementBuilder builder) {
            return builder.ToStatement();
        }
    }

    public static partial class Ast {
        public static SwitchStatementBuilder Switch() {
            return Switch(SourceSpan.None, SourceLocation.None);
        }

        public static SwitchStatementBuilder Switch(SourceSpan span, SourceLocation header) {
            return new SwitchStatementBuilder(span, header, null);
        }

        public static SwitchStatementBuilder Switch(Expression test) {
            return Switch(SourceSpan.None, SourceLocation.None, test);
        }

        public static SwitchStatementBuilder Switch(SourceSpan span, SourceLocation header, Expression test) {
            Contract.RequiresNotNull(test, "test");
            return new SwitchStatementBuilder(span, header, test);
        }
    }
}
