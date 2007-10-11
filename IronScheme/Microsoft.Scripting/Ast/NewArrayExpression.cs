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
using System.Diagnostics;
using System.Collections.Generic;

using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Ast {
    public class NewArrayExpression : Expression {
        private IList<Expression> _expressions;
        private Type _type;
        private System.Reflection.ConstructorInfo _constructor;

        internal NewArrayExpression(Type type, IList<Expression> expressions) {
            Contract.RequiresNotNull(expressions, "expressions");
            Contract.RequiresNotNull(type, "type");
            Contract.Requires(type.IsArray, "type", "Not an array type");
            Contract.RequiresNotNullItems(expressions, "expressions");

            _type = type;
            _expressions = expressions;
            _constructor = _type.GetConstructor(new Type[] { typeof(int) });
        }

        public IList<Expression> Expressions {
            get { return _expressions; }
        }

        public override Type Type {
            get {
                return _type;
            }
        }

        public override void Emit(CodeGen cg) {
            cg.EmitArrayFromExpressions(_type.GetElementType(), _expressions);
        }

        protected override object DoEvaluate(CodeContext context) {
            if (_type.GetElementType().IsValueType) {
                // value arrays cannot be cast to object arrays
                object contents = (object)_constructor.Invoke(new object[] { _expressions.Count });
                System.Reflection.MethodInfo setter = _type.GetMethod("Set");
                for (int i = 0; i < _expressions.Count; i++) {
                    setter.Invoke(contents, new object[] { i, _expressions[i].Evaluate(context) });
                }
                return contents;
            } else {
                object[] contents = (object[])_constructor.Invoke(new object[] { _expressions.Count });
                for (int i = 0; i < _expressions.Count; i++) {
                    contents[i] = _expressions[i].Evaluate(context);
                }
                return contents;
            }
        }

        public override void Walk(Walker walker) {
            if (walker.Walk(this)) {
                foreach (Expression expr in _expressions) {
                    expr.Walk(walker);
                }
            }
            walker.PostWalk(this);
        }
    }

    /// <summary>
    /// Factory methods.
    /// </summary>
    public static partial class Ast {
        /// <summary>
        /// Creates a new array expression of the specified type from the provided initializers.
        /// </summary>
        /// <param name="type">The type of the array (e.g. object[]).</param>
        /// <param name="initializers">The expressions used to create the array elements.</param>
        public static NewArrayExpression NewArray(Type type, IEnumerable<Expression> initializers) {
            return new NewArrayExpression(type, new List<Expression>(initializers));
        }

        /// <summary>
        /// Creates a new array expression of the specified type from the provided initializers.
        /// </summary>
        /// <param name="type">The type of the array (e.g. object[]).</param>
        /// <param name="initializers">The expressions used to create the array elements.</param>
        public static NewArrayExpression NewArray(Type type, params Expression[] initializers) {
            return new NewArrayExpression(type, new List<Expression>(initializers));
        }
    }
}
