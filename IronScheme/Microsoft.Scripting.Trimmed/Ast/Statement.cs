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
using System.Text;
using System.Reflection;
using System.Reflection.Emit;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;

using System.Diagnostics;
using Microsoft.Scripting.Generation;

namespace Microsoft.Scripting.Ast {
    public abstract class Statement : Node {
        private SourceLocation _start;
        private SourceLocation _end;

        // Hold on to one instance for each member of the ControlFlow enumeration to avoid unnecessary boxing
        internal static readonly object NextStatement = ControlFlow.NextStatement;
        internal static readonly object Break = ControlFlow.Break;
        internal static readonly object Continue = ControlFlow.Continue;

        protected Statement(AstNodeType nodeType, SourceSpan span)
            : base(nodeType) {
            _start = span.Start;
            _end = span.End;
        }

        public SourceLocation Start {
            get { return _start; }
        }

        public SourceLocation End {
            get { return _end; }
        }

        public SourceSpan Span {
            get {
                return new SourceSpan(_start, _end);
            }
        }

        public void SetLoc(SourceSpan span) {
            _start = span.Start;
            _end = span.End;
        }

        public object Execute(CodeContext context) {
            context.Scope.SourceLocation = Start;

            try {
                try {
#if DEBUG
                    ExpressionReturnException.CurrentDepth++;
#endif
                    return DoExecute(context);
                } catch (ExpressionReturnException ex) {
#if DEBUG
                    Debug.Assert(ex.Depth == ExpressionReturnException.CurrentDepth);
#endif
                    return ex.Value;
                }
            } finally {
#if DEBUG
                ExpressionReturnException.CurrentDepth--;
#endif
            }
        }

        protected virtual object DoExecute(CodeContext context) {
            throw new NotImplementedException(String.Format(CultureInfo.CurrentCulture, Resources.NotImplemented_Execute, this));
        }

        public abstract void Emit(CodeGen cg);
    }
}
