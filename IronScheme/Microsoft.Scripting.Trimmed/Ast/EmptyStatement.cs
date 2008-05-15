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

using Microsoft.Scripting.Generation;

namespace Microsoft.Scripting.Ast {
    public class EmptyStatement : Statement {
        internal EmptyStatement(SourceSpan span)
            : base(AstNodeType.EmptyStatement, span) {
        }

        protected override object DoExecute(CodeContext context) {
            return NextStatement;
        }

        public override void Emit(CodeGen cg) {
            cg.EmitPosition(Start, End);
        }
    }

    public static partial class Ast {
        public static EmptyStatement Empty() {
            return Empty(SourceSpan.None);
        }

        public static EmptyStatement Empty(SourceSpan span) {
            return new EmptyStatement(span);
        }
    }
}
