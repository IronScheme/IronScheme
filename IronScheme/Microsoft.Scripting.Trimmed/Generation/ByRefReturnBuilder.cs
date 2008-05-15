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
using System.Diagnostics;

using Microsoft.Scripting.Actions;
using Microsoft.Scripting;
using Microsoft.Scripting.Ast;

namespace Microsoft.Scripting.Generation {
    using Ast = Microsoft.Scripting.Ast.Ast;

    public class ByRefReturnBuilder : ReturnBuilder {
        private IList<int> _returnArgs;
        private ActionBinder _binder;

        public ByRefReturnBuilder(ActionBinder binder, IList<int> returnArgs)
            : base(typeof(object)) {
            _returnArgs = returnArgs;
            _binder = binder;
        }

        internal override Expression ToExpression(MethodBinderContext context, IList<ArgBuilder> args, IList<Expression> parameters, Expression ret) {
            if (_returnArgs.Count == 1) {
                if (_returnArgs[0] == -1) {
                    return ret;
                }
                return Ast.Comma(ret, args[_returnArgs[0]].ToReturnExpression(context));
            }

            Expression[] retValues = new Expression[_returnArgs.Count];
            int rIndex = 0;
            bool usesRet = false;
            foreach (int index in _returnArgs) {
                if (index == -1) {
                    usesRet = true;
                    retValues[rIndex++] = ret;
                } else {
                    retValues[rIndex++] = args[index].ToReturnExpression(context);
                }
            }

            Expression retArray = Ast.NewArrayHelper(typeof(object[]), retValues);
            if (!usesRet) {
                retArray = Ast.Comma(ret, retArray);
            }

            return Ast.Call(
                Ast.ReadProperty(
                    Ast.ReadProperty(
                        Ast.CodeContext(),
                        typeof(CodeContext).GetProperty("LanguageContext")
                    ),
                    typeof(LanguageContext).GetProperty("Binder")
                ),
                typeof(ActionBinder).GetMethod("GetByRefArray"),
                retArray
            );
        }

        public override object Build(CodeContext context, object[] args, object[] parameters, object ret) {
            if (_returnArgs.Count == 1) {
                return GetValue(args, ret, _returnArgs[0]);
            } else {
                object[] retValues = new object[_returnArgs.Count];
                int rIndex = 0;
                foreach (int index in _returnArgs) {
                    retValues[rIndex++] = GetValue(args, ret, index);
                }
                return _binder.GetByRefArray(retValues);
            }
        }

        private static object GetValue(object[] args, object ret, int index) {
            if (index == -1) return ConvertToObject(ret);
            return ConvertToObject(args[index]);
        }

        private static Expression GetValue(Expression[] args, Expression ret, int index) {
            if (index == -1) return ret;
            Debug.Assert(index < args.Length);
            return args[index];
        }

        public override int CountOutParams {
            get { return _returnArgs.Count; }
        }


    }
}
