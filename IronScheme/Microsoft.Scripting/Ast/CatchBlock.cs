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
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Ast {
    public class CatchBlock : Node {
        private readonly SourceLocation _start;
        private readonly SourceLocation _header;
        private readonly SourceLocation _end;
        private readonly Type /*!*/ _test;
        private readonly Variable _var;
        private readonly Statement /*!*/ _body;

        private VariableReference _ref;

        private bool _yield;        // The catch block contains a yield

        internal CatchBlock(SourceSpan span, SourceLocation header, Type /*!*/ test, Variable target, Statement /*!*/ body)
            : base(AstNodeType.CatchBlock) {
            _test = test;
            _var = target;
            _body = body;
            _start = span.Start;
            _header = header;
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

        public Variable Variable {
            get { return _var; }
        }

        public Type Test {
            get { return _test; }
        }

        public Statement Body {
            get { return _body; }
        }

        internal VariableReference Ref {
            get { return _ref; }
            set {
                Debug.Assert(value.Variable == _var);
                Debug.Assert(_ref == null);
                _ref = value;
            }
        }

        internal Slot Slot {
            get { return _ref.Slot; }
        }

        internal bool Yield {
            get { return _yield; }
            set { _yield = value; }
        }
    }

    public static partial class Ast {
        public static CatchBlock Catch(Type type, Statement body) {
            return Catch(SourceSpan.None, SourceLocation.None, type, null, body);
        }

        public static CatchBlock Catch(Type type, Variable target, Statement body) {
            return Catch(SourceSpan.None, SourceLocation.None, type, target, body);
        }

        public static CatchBlock Catch(SourceSpan span, SourceLocation header, Type type, Variable target, Statement body) {
            Contract.RequiresNotNull(type, "type");
            Contract.Requires(target == null || TypeUtils.CanAssign(target.Type, type), "target");
            Contract.RequiresNotNull(body, "body");
            return new CatchBlock(span, header, type, target, body);
        }
    }
}
