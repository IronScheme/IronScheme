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
    public class CodeContextExpression : Expression {

        internal CodeContextExpression()
            : base(AstNodeType.CodeContextExpression) {
        }

        public override Type Type {
            get {
                return typeof(CodeContext);
            }
        }

        protected override object DoEvaluate(CodeContext context) {
            return context;
        }

        public override void Emit(CodeGen cg) {
            cg.EmitCodeContext();
        }
    }

    public static partial class Ast {
        public static CodeContextExpression CodeContext() {
            return new CodeContextExpression();
        }
    }
}
