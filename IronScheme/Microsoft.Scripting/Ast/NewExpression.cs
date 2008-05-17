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
using System.Reflection;
using System.Diagnostics;
using System.Collections.Generic;
using System.Collections.ObjectModel;

using Microsoft.Scripting.Utils;
using Microsoft.Scripting.Generation;

namespace Microsoft.Scripting.Ast {
    public class NewExpression : Expression {
        private readonly ConstructorInfo /*!*/ _constructor;
        private readonly ReadOnlyCollection<Expression> /*!*/ _arguments;
        private readonly ParameterInfo[] /*!*/ _parameterInfos;

        internal NewExpression(ConstructorInfo /*!*/ constructor, ReadOnlyCollection<Expression> /*!*/ arguments, ParameterInfo[] /*!*/ parameters)
            : base(AstNodeType.New) {
            _constructor = constructor;
            _arguments = arguments;
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
                _arguments[i].Emit(cg);
            }
            cg.EmitNew(_constructor);
        }


#if FULL
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
#endif

    }

    /// <summary>
    /// Factory methods.
    /// </summary>
    public static partial class Ast {
        public static NewExpression New(ConstructorInfo constructor, params Expression[] arguments) {
            return New(constructor, (IList<Expression>)arguments);
        }

        public static NewExpression New(ConstructorInfo constructor, IList<Expression> arguments) {
            Contract.RequiresNotNull(constructor, "constructor");
            Contract.RequiresNotNullItems(arguments, "arguments");

            ParameterInfo[] parameters = constructor.GetParameters();
            ValidateCallArguments(parameters, arguments);

            return new NewExpression(constructor, CollectionUtils.ToReadOnlyCollection(arguments), parameters);
        }

        public static NewExpression SimpleNewHelper(ConstructorInfo constructor, params Expression[] arguments) {
            Contract.RequiresNotNull(constructor, "constructor");
            Contract.RequiresNotNullItems(arguments, "arguments");

            ParameterInfo [] parameters = constructor.GetParameters();
            Contract.Requires(arguments.Length == parameters.Length, "arguments", "Incorrect number of arguments");

            return New(constructor, ArgumentConvertHelper(arguments, parameters));
        }
    }
}
