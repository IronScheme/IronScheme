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
    public class UnboundExpression : Expression {
        private readonly SymbolId _name;

        internal UnboundExpression(SymbolId name)
            : base(AstNodeType.UnboundExpression) {
            _name = name;
        }

        public SymbolId Name {
            get { return _name; }
        }

        public override Type Type {
            get {
                return typeof(object);
            }
        }


#if FULL
        protected override object DoEvaluate(CodeContext context) {
            return RuntimeHelpers.LookupName(context, _name);
        } 
#endif


        public override void Emit(CodeGen cg) {
            // RuntimeHelpers.LookupName(CodeContext, name)
            cg.EmitCodeContext();
            cg.EmitSymbolId(_name);
            cg.EmitCall(typeof(RuntimeHelpers), "LookupName");
        }
    }

    /// <summary>
    /// Factory methods
    /// </summary>
    public static partial class Ast {
        public static UnboundExpression Read(SymbolId name) {
            Contract.Requires(!name.IsInvalid && !name.IsEmpty, "name", "Invalid or empty name is not allowed");
            return new UnboundExpression(name);
        }
    }
}
