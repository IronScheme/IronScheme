
#if FULL
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
using System.Collections.Generic;

using Microsoft.Scripting;
using Microsoft.Scripting.Generation;

namespace Microsoft.Scripting.Ast {
    /// <summary>
    /// An abstraction of an actual value
    /// </summary>
    public class AbstractValue {
        private enum AbstractKind {
            Constant,
            ExactType,
            LimitType,
            Error,
        }
        
        private object _value;
        private Type _type;
        private AbstractKind _kind;

        private Expression _expression;

        private AbstractValue(object value, Type type, AbstractKind kind, Expression expression) {
            this._value = value;
            this._type = type;
            this._kind = kind;
            this._expression = expression;

            //if (expression != null) {
            //    Debug.Assert(expression.Type == type);
            //}
        }

        public bool IsConstant {
            get { return _kind == AbstractKind.Constant; }
        }

        public bool HasExpression {
            get { return _expression != null; }
        }

        public bool IsExact {
            get { return _kind == AbstractKind.Constant || _kind == AbstractKind.ExactType; }
        }

        public Type Type {
            get { return _type; }
        }

        public Expression Expression {
            get { return _expression; }
        }

        public object Value {
            get { return _value; }
        }

        public static Type[] GetTypes(IList<AbstractValue> values) {
            Type[] types = new Type[values.Count];
            for (int i = 0; i < types.Length; i++) {
                types[i] = values[i].Type;
            }
            return types;
        }

        public static AbstractValue Constant(object value, Expression expression) {
            return new AbstractValue(value, CompilerHelpers.GetType(value), AbstractKind.Constant, expression);
        }

        public static AbstractValue ExactType(Type type, Expression expression) {
            return new AbstractValue(null, type, AbstractKind.ExactType, expression);
        }

        public static AbstractValue LimitType(Type type, Expression expression) {
            if (CompilerHelpers.IsSealed(type)) return ExactType(type, expression);

            return new AbstractValue(null, type, AbstractKind.LimitType, expression);
        }

        public static AbstractValue TypeError(string message) {
            throw RuntimeHelpers.SimpleTypeError(message);
            //return new AbstractValue(null, Exception, AbstractKind.Error, null);
        }

        public static AbstractValue FromExpression(Expression expression) {
            //??? special handling of constant expressions???
            return LimitType(expression.Type, expression);
        }
    }
}

#endif	
