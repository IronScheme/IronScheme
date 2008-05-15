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
using System.Text;
using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Actions;

namespace Microsoft.Scripting.Generation {
    using Ast = Microsoft.Scripting.Ast.Ast;
    using System.Reflection;

    /// <summary>
    /// Builds the argument for an out argument when not passed a StrongBox.  The out parameter
    /// is returned as an additional return value.
    /// </summary>
    class OutArgBuilder : ArgBuilder {
        private Type _parameterType;
        private bool _isRef;
        private Variable _tmp;

        public OutArgBuilder(ParameterInfo parameter) {
            _parameterType = parameter.ParameterType.IsByRef ? parameter.ParameterType.GetElementType() : parameter.ParameterType;
            _isRef = parameter.ParameterType.IsByRef;
        }

        public override int Priority {
            get { return 5; }
        }

        internal override Expression ToExpression(MethodBinderContext context, Expression[] parameters) {
            if (_isRef) {
                if (_tmp == null) {
                    _tmp = context.GetTemporary(_parameterType, "outParam");
                }
                return Ast.Read(_tmp);
            }

            return GetDefaultValue();
        }

        internal override Expression CheckExpression(MethodBinderContext context, Expression[] parameters) {
            return null;
        }

        internal override Expression ToReturnExpression(MethodBinderContext context) {
            if (_isRef) {
                return Ast.Read(_tmp);
            }

            return GetDefaultValue();
        }

        public override object Build(CodeContext context, object[] args) {
            return null;
        }

        private Expression GetDefaultValue() {
            if (_parameterType.IsValueType) {
                // default(T)                
                return Ast.Constant(Activator.CreateInstance(_parameterType));
            }
            return Ast.Constant(null);
        }
    }
}
