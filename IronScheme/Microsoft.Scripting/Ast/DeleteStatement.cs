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

using System.Diagnostics;
using Microsoft.Scripting.Utils;
using Microsoft.Scripting.Generation;

namespace Microsoft.Scripting.Ast {
    /// <summary>
    /// AST node representing deletion of the variable value.
    /// </summary>
    public class DeleteStatement : Statement {
        private readonly Variable /*!*/ _var;
        private VariableReference _ref;
        private bool _defined;

        internal DeleteStatement(SourceSpan span, Variable /*!*/ var)
            : base(AstNodeType.DeleteStatement, span) {
            _var = var;
        }

        internal bool IsDefined {
            get { return _defined; }
            set { _defined = value; }
        }

        public Variable Variable {
            get { return _var; }
        }

        internal VariableReference Ref {
            get { return _ref; }
            set {
                Debug.Assert(value.Variable == _var);
                Debug.Assert(_ref == null);
                _ref = value;
            }
        }


#if FULL
        protected override object DoExecute(CodeContext context) {
            switch (_var.Kind) {
                case Variable.VariableKind.Temporary:
                case Variable.VariableKind.GeneratorTemporary:
                    context.Scope.TemporaryStorage.Remove(_var);
                    break;
                case Variable.VariableKind.Global:
                    RuntimeHelpers.RemoveGlobalName(context, _var.Name);
                    break;
                default:
                    RuntimeHelpers.RemoveName(context, _var.Name);
                    break;
            }
            
            return Statement.NextStatement;
        } 
#endif


        public override void Emit(CodeGen cg) {
            //cg.EmitPosition(Start, End);
            _ref.Slot.EmitDelete(cg, _var.Name, !_defined);
        }
    }

    public static partial class Ast {
        public static DeleteStatement Delete(SourceSpan span, Variable variable) {
            Contract.RequiresNotNull(variable, "variable");
            return new DeleteStatement(span, variable);
        }
    }
}
