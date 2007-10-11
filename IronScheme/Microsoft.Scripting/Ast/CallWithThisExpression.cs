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

using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Ast {
    public class CallWithThisExpression : Expression {
        private readonly Expression _target;
        private readonly Expression _instance;
        private readonly Arg[] _args;

        internal CallWithThisExpression(Expression target, Expression instance, Arg[] args) {
            _target = target;
            _instance = instance;
            _args = args;
        }

        protected override object DoEvaluate(CodeContext context) {
            object target = _target.Evaluate(context);
            object instance = _instance != null ? _instance.Evaluate(context) : null;
            object[] args = null;

            if (_args != null) {
                args = new object[_args.Length];
                for (int arg = 0; arg < args.Length; arg++) {
                    args[arg] = _args[arg] != null ? _args[arg].Expression.Evaluate(context) : null;
                }
            }

            return RuntimeHelpers.CallWithThis(context, target, instance, args);
        }

        public override void Emit(CodeGen cg) {
            cg.EmitCodeContext();
            _target.EmitAsObject(cg);
            cg.EmitExprAsObjectOrNull(_instance);

            if (_args == null) {
                cg.EmitNull();
            } else {
                cg.EmitArray(typeof(object), _args.Length, delegate(int item) {
                    cg.EmitExprAsObjectOrNull(_args[item].Expression);
                });
            }

            cg.EmitCall(typeof(RuntimeHelpers), "CallWithThis");
        }

        public override void Walk(Walker walker) {
            if (walker.Walk(this)) {
                _target.Walk(walker);
                if (_instance != null) {
                    _instance.Walk(walker);
                }
                if (_args != null) {
                    foreach (Arg arg in _args) {
                        arg.Walk(walker);
                    }
                }
            }
        }
    }

    public static partial class Ast {
        public static CallWithThisExpression CallWithThis(Expression target, Expression instance, Arg[] args) {
            Contract.RequiresNotNull(target, "target");
            return new CallWithThisExpression(target, instance, args);
        }
    }
}
