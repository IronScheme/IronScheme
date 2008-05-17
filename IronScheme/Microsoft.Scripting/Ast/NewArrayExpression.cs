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
using System.Collections.Generic;
using System.Collections.ObjectModel;

using Microsoft.Scripting.Utils;
using Microsoft.Scripting.Generation;

namespace Microsoft.Scripting.Ast {
    public class NewArrayExpression : Expression {
        private ReadOnlyCollection<Expression> _expressions;
        private Type _type;
        private System.Reflection.ConstructorInfo _constructor;

        internal NewArrayExpression(Type type, ReadOnlyCollection<Expression> expressions)
            : base(AstNodeType.NewArrayExpression) {
            _type = type;
            _expressions = expressions;
            _constructor = _type.GetConstructor(new Type[] { typeof(int) });
        }

        public ReadOnlyCollection<Expression> Expressions {
            get { return _expressions; }
        }

        public override Type Type {
            get {
                return _type;
            }
        }

        public override void Emit(CodeGen cg) {
            cg.EmitArray(
                _type.GetElementType(),
                _expressions.Count,
                delegate(int index) {
                    _expressions[index].Emit(cg);
                }
            );
        }


#if FULL
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
#endif

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
        public static NewArrayExpression NewArray(Type type, params Expression[] initializers) {
            return NewArray(type, (IList<Expression>)initializers);
        }

        /// <summary>
        /// Creates a new array expression of the specified type from the provided initializers.
        /// </summary>
        /// <param name="type">The type of the array (e.g. object[]).</param>
        /// <param name="initializers">The expressions used to create the array elements.</param>
        public static NewArrayExpression NewArray(Type type, IList<Expression> initializers) {
            Contract.RequiresNotNull(initializers, "initializers");
            Contract.RequiresNotNull(type, "type");
            Contract.Requires(type.IsArray, "type", "Not an array type");
            Contract.RequiresNotNullItems(initializers, "initializers");

            Type element = type.GetElementType();
            foreach (Expression expression in initializers) {
                Contract.Requires(TypeUtils.CanAssign(element, expression.Type), "initializers");
            }

            return new NewArrayExpression(type, CollectionUtils.ToReadOnlyCollection(initializers));
        }

        public static NewArrayExpression NewArrayHelper(Type type, IList<Expression> initializers) {
            Contract.RequiresNotNullItems(initializers, "initializers");
            Contract.RequiresNotNull(type, "type");
            Contract.Requires(type.IsArray, "type", "Not an array type");

            Type element = type.GetElementType();
            Expression[] clone = null;
            for (int i = 0; i < initializers.Count; i++) {
                Expression initializer = initializers[i];
                if (!TypeUtils.CanAssign(element, initializer.Type)) {
                    if (clone == null) {
                        clone = new Expression[initializers.Count];
                        for (int j = 0; j < i; j++) {
                            clone[j] = initializers[j];
                        }
                    }
                    initializer = Ast.Convert(initializer, element);
                }
                if (clone != null) {
                    clone[i] = initializer;
                }
            }

            return NewArray(type, clone ?? initializers);
        }
    }
}
