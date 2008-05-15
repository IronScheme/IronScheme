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
using Microsoft.Scripting.Utils;
using Microsoft.Scripting.Actions;

namespace Microsoft.Scripting.Generation {
    using Ast = Microsoft.Scripting.Ast.Ast;

    /// <summary>
    /// Builds the parameter for a params dictionary argument - this collects all the extra name/value
    /// pairs provided to the function into a SymbolDictionary which is passed to the function.
    /// </summary>
    class ParamsDictArgBuilder : ArgBuilder {
        private SymbolId[] _names;
        private int[] _nameIndexes;
        private int _argIndex;

        public ParamsDictArgBuilder(int argIndex, SymbolId[] names, int []nameIndexes) {
            _argIndex = argIndex;
            _names = names;
            _nameIndexes = nameIndexes;
        }

        public override int Priority {
            get { return 3; }
        }

        internal override Expression ToExpression(MethodBinderContext context, Expression[] parameters) {
            Expression res = Ast.Call(
                typeof(BinderOps).GetMethod("MakeSymbolDictionary"),
                Ast.NewArray(typeof(SymbolId[]), ConstantNames()),
                Ast.NewArrayHelper(typeof(object[]), GetParameters(parameters))
            );

            return res;
        }

        internal override Expression CheckExpression(MethodBinderContext context, Expression[] parameters) {
            return null;
        }

        private Expression[] GetParameters(Expression[] parameters) {
            Expression[] res = new Expression[_nameIndexes.Length];
            for (int i = 0; i < _nameIndexes.Length; i++) {
                res[i] = parameters[_nameIndexes[i] + _argIndex];
            }
            return res;
        }

        private Expression[] ConstantNames() {
            Expression[] res = new Expression[_names.Length];
            for (int i = 0; i < _names.Length; i++) {
                res[i] = Ast.Constant(_names[i]);
            }
            return res;
        }

        public override object Build(CodeContext context, object[] args) {
            SymbolDictionary res = new SymbolDictionary();
            for (int i = _argIndex; i < _argIndex + _names.Length; i++) {
                res.Add(_names[i - _argIndex], args[i]);
            }
            return res;
        }
    }
}
