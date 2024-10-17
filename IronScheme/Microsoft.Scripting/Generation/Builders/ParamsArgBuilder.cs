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
using System.Reflection.Emit;

using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Actions;

namespace Microsoft.Scripting.Generation.Builders
{
    using Ast = Ast.Ast;
    using System.Diagnostics;
    using Microsoft.Scripting.Utils;

    internal class ParamsArgBuilder : ArgBuilder
    {
        private int _start;
        private int _count;
        private Type _elementType;
        public ParamsArgBuilder(int start, int count, Type elementType)
        {
            Contract.RequiresNotNull(elementType, "elementType");
            if (start < 0) throw new ArgumentOutOfRangeException("start");
            if (count < 0) throw new ArgumentOutOfRangeException("count");

            _start = start;
            _count = count;
            _elementType = elementType;
        }

        public override int Priority
        {
            get { return 4; }
        }

        public override object Build(CodeContext context, object[] args)
        {
            var paramsArray = Array.CreateInstance(_elementType, _count);
            for (var i = 0; i < _count; i++)
            {
                paramsArray.SetValue(context.LanguageContext.Binder.Convert(args[i + _start], _elementType), i);
            }
            return paramsArray;
        }

        internal override Expression ToExpression(MethodBinderContext context, Expression[] parameters)
        {
            var elems = new Expression[_count];
            for (var i = 0; i < _count; i++)
            {
                elems[i] = context.ConvertExpression(parameters[_start + i], _elementType);
            }

            return Ast.NewArray(_elementType.MakeArrayType(), elems);
        }

        internal override Expression CheckExpression(MethodBinderContext context, Expression[] parameters)
        {
            if (_count == 0) return null;

            var res = context.CheckExpression(parameters[_start], _elementType);
            for (var i = 1; i < _count; i++)
            {
                res = Ast.AndAlso(res, context.CheckExpression(parameters[_start + i], _elementType));
            }
            return res;
        }
    }
}
