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
using System.Reflection.Emit;

using Microsoft.Scripting.Generation;

namespace Microsoft.Scripting.Ast {
    public class ThrowExpression : Expression {
        private readonly Expression _val;

        internal ThrowExpression(Expression value) {
            _val = value;
        }

        public Expression Value {
            get {
                return _val;
            }
        }

        public override Type Type {
            get {
                return typeof(void);
            }
        }

        protected override object DoEvaluate(CodeContext context) {
            if (_val == null) {
                throw ExceptionHelpers.LastException;
            } else {
                throw (Exception)context.LanguageContext.Binder.Convert(_val.Evaluate(context), typeof(Exception));
            }
        }

        public override void Emit(CodeGen cg) {
            if (_val == null) {
                cg.Emit(OpCodes.Rethrow);
            } else {
                _val.EmitAs(cg, typeof(Exception));
                cg.Emit(OpCodes.Throw);
            }
        }

        public override void Walk(Walker walker) {
            if (walker.Walk(this)) {
                if (_val != null) _val.Walk(walker);
            }
            walker.PostWalk(this);
        }

        public Expression Exception {
            get {
                return _val;
            }
        }
    }

    public static partial class Ast {
        public static ThrowExpression Rethrow() {
            return Throw(null);
        }

        public static ThrowExpression Throw(Expression value) {
            return new ThrowExpression(value);
        }
    }
}
