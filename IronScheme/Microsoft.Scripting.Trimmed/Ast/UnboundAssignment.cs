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
using System.Diagnostics;
using Microsoft.Scripting.Utils;
using Microsoft.Scripting.Generation;

namespace Microsoft.Scripting.Ast {
    public class UnboundAssignment : Expression {
        private readonly SymbolId _name;
        private readonly Expression /*!*/ _value;

        internal UnboundAssignment(SymbolId name, Expression /*!*/ value)
            : base(AstNodeType.UnboundAssignment) {
            _name = name;
            _value = value;
        }

        public override Type Type {
            get {
                return typeof(object);
            }
        }

        public SymbolId Name {
            get { return _name; }
        }

        public Expression Value {
            get { return _value; }
        }

        protected override object DoEvaluate(CodeContext context) {
            object value = _value.Evaluate(context);
            RuntimeHelpers.SetName(context, _name, value);
            return value;
        }

        public override void Emit(CodeGen cg) {
          EmitLocation(cg);
            _value.EmitAsObject(cg);
            cg.EmitCodeContext();
            cg.EmitSymbolId(_name);
            cg.EmitCall(typeof(RuntimeHelpers), "SetNameReorder");
        }
    }

    /// <summary>
    /// Factory methods.
    /// </summary>
    public static partial class Ast {
        public static UnboundAssignment Assign(SymbolId name, Expression value) {
            Contract.Requires(!name.IsEmpty && !name.IsInvalid, "name", "Invalid or empty name is not allowed");
            Contract.RequiresNotNull(value, "value");
            return new UnboundAssignment(name, value);
        }
    }
}
