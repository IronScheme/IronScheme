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
using Microsoft.Scripting.Ast;

namespace Microsoft.Scripting.Generation.Builders
{
    internal class ReturnBuilder
    {
        private Type _returnType;

        /// <summary>
        /// Creates a ReturnBuilder
        /// </summary>
        /// <param name="returnType">the type the ReturnBuilder will leave on the stack</param>
        public ReturnBuilder(Type returnType) { _returnType = returnType; }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="context"></param>
        /// <param name="args">The physical arguments being passed to the function</param>
        /// <param name="parameters">The arguments the user provided to call the function</param>
        /// <param name="ret">The return value of the function</param>
        /// <returns></returns>
        public virtual object Build(CodeContext context, object[] args, object[] parameters, object ret)
        {
            return ConvertToObject(ret);
        }

        internal virtual Expression ToExpression(MethodBinderContext context, IList<ArgBuilder> args, IList<Expression> parameters, Expression ret)
        {
            return ret;
        }

        public virtual int CountOutParams
        {
            get { return 0; }
        }

        public Type ReturnType
        {
            get
            {
                return _returnType;
            }
        }

        protected static object ConvertToObject(object ret)
        {
            if (ret is bool)
            {
                return RuntimeHelpers.BooleanToObject((bool)ret);
            }
            else if (ret is int)
            {
                return RuntimeHelpers.Int32ToObject((int)ret);
            }
            return ret;
        }
    }
}
