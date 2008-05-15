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
using Microsoft.Scripting.Utils;
using Microsoft.Scripting.Generation;

namespace Microsoft.Scripting.Ast {
    public class DeleteUnboundExpression : Expression {
        private SymbolId _name;

        internal DeleteUnboundExpression(SymbolId name)
            : base(AstNodeType.DeleteUnboundExpression) {
            _name = name;
        }

        public SymbolId Name {
            get { return _name; }
        }

        public override void Emit(CodeGen cg) {
            // RuntimeHelpers.RemoveName(CodeContext, name)
            cg.EmitCodeContext();
            cg.EmitSymbolId(_name);
            cg.EmitCall(typeof(RuntimeHelpers), "RemoveName");
        }

        protected override object DoEvaluate(CodeContext context) {
            return RuntimeHelpers.RemoveName(context, _name);
        }
    }

    public static partial class Ast {
        public static DeleteUnboundExpression Delete(SymbolId name) {
            Contract.Requires(!name.IsInvalid && !name.IsEmpty, "name");
            return new DeleteUnboundExpression(name);
        }
    }
}
