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
using System.Collections.Generic;
using System.Collections.ObjectModel;

using System.Reflection;
using System.Reflection.Emit;

using System.Diagnostics;

using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Ast {
    public class NewExpression : Expression {
        private readonly ConstructorInfo _constructor;
        private readonly ReadOnlyCollection<Expression> _arguments;
        private readonly ParameterInfo[] _parameterInfos;

        internal NewExpression(ConstructorInfo constructor, IList<Expression> arguments, ParameterInfo[] parameters) {
            _constructor = constructor;
            _arguments = new ReadOnlyCollection<Expression>(arguments);
            _parameterInfos = parameters;
        }

        public ConstructorInfo Constructor {
            get { return _constructor; }
        }

        public ReadOnlyCollection<Expression> Arguments {
            get { return _arguments; }
        }

        public override Type Type {
            get {
                return _constructor.DeclaringType;
            }
        }

        public override void Emit(CodeGen cg) {
            for (int i = 0; i < _parameterInfos.Length; i++) {
                _arguments[i].EmitAs(cg, _parameterInfos[i].ParameterType);
            }
            cg.EmitNew(_constructor);
        }

        protected override object DoEvaluate(CodeContext context) {
            object[] args = new object[_arguments.Count];
            for (int i = 0; i < _arguments.Count; i++) {
                args[i] = _arguments[i].Evaluate(context);
            }
            try {
                return _constructor.Invoke(args);
            } catch (TargetInvocationException e) {
                throw ExceptionHelpers.UpdateForRethrow(e.InnerException);
            }
        }

        public override void Walk(Walker walker) {
            if (walker.Walk(this)) {
                if (_arguments != null) {
                    foreach (Expression e in _arguments) {
                        e.Walk(walker);
                    }
                }
            }
            walker.PostWalk(this);
        }
    }

    /// <summary>
    /// Factory methods.
    /// </summary>
    public static partial class Ast {
        public static NewExpression New(ConstructorInfo constructor, params Expression[] arguments) {
            Contract.RequiresNotNull(constructor, "constructor");
            Contract.RequiresNotNullItems(arguments, "arguments");

            ParameterInfo[] pi = constructor.GetParameters();
            Contract.Requires(CompilerHelpers.FormalParamsMatchActual(pi, arguments.Length), "constructor", "The number of parameters doesn't match the number of actual arguments");

            return new NewExpression(constructor, arguments, pi);
        }
    }
}
