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
using System.Reflection.Emit;
using System.Diagnostics;

using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Ast {
    public class ThrowStatement : Statement {
        private readonly Expression _val;

        internal ThrowStatement(SourceSpan span, Expression value)
            : base(AstNodeType.ThrowStatement, span) {
            _val = value;
        }

        public Expression Value {
            get {
                return _val;
            }
        }


#if FULL
        protected override object DoExecute(CodeContext context) {
            if (_val == null) {
                throw TryStatement.LastEvalException;
            } else {
                throw (Exception)_val.Evaluate(context);
            }
        } 
#endif


        public override void Emit(CodeGen cg) {
            //cg.EmitPosition(Start, End);
            if (_val == null) {
                cg.Emit(OpCodes.Rethrow);
            } else {
                _val.Emit(cg);
                cg.Emit(OpCodes.Throw);
            }
        }

        public Expression Exception {
            get {
                return _val;
            }
        }
    }

    public static partial class Ast {
        public static ThrowStatement Rethrow() {
            return Throw(SourceSpan.None, null);
        }

        public static ThrowStatement Rethrow(SourceSpan span) {
            return Throw(span, null);
        }

        public static ThrowStatement Throw(Expression value) {
            return Throw(SourceSpan.None, value);
        }

        public static ThrowStatement Throw(SourceSpan span, Expression value) {
            if (value != null) {
                Contract.Requires(TypeUtils.CanAssign(typeof(Exception), value.Type));
            }
            return new ThrowStatement(span, value);
        }
    }
}
