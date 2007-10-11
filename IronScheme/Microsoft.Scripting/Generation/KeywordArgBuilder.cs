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
using System.Text;
using Microsoft.Scripting.Ast;

namespace Microsoft.Scripting.Generation {
    /// <summary>
    /// ArgBuilder which provides a value for a keyword argument.  
    /// 
    /// The KeywordArgBuilder calculates its position at emit time using it's initial 
    /// offset within the keyword arguments, the number of keyword arguments, and the 
    /// total number of arguments provided by the user.  It then delegates to an 
    /// underlying ArgBuilder which only receives the single correct argument.
    /// </summary>
    public class KeywordArgBuilder : ArgBuilder {
        private int _kwArgCount, _kwArgIndex;
        private ArgBuilder _builder;

        public KeywordArgBuilder(ArgBuilder builder, int kwArgCount, int kwArgIndex) {
            _builder = builder;
            _kwArgCount = kwArgCount;
            _kwArgIndex = kwArgIndex;
        }

        public override int Priority {
            get { return _builder.Priority; }
        }

        internal override Expression ToExpression(MethodBinderContext context, Expression[] parameters) {
            return _builder.ToExpression(context, new Expression[] { parameters[GetKeywordIndex(parameters.Length)] });
        }

        internal override Expression CheckExpression(MethodBinderContext context, Expression[] parameters) {
            return _builder.CheckExpression(context, new Expression[] { parameters[GetKeywordIndex(parameters.Length)] });
        }

        internal override Expression ToReturnExpression(MethodBinderContext context) {
            return _builder.ToReturnExpression(context);
        }

        internal override Expression UpdateFromReturn(MethodBinderContext context, Expression[] parameters) {
            return _builder.UpdateFromReturn(context, new Expression[] { parameters[GetKeywordIndex(parameters.Length)] });
        }

        public override void UpdateFromReturn(object callArg, object[] args) {
            _builder.UpdateFromReturn(callArg, new object[] { args[GetKeywordIndex(args.Length)] });
        }

        public override object Build(CodeContext context, object[] args) {
            return _builder.Build(context, new object[] { args[GetKeywordIndex(args.Length)] });
        }

        private int GetKeywordIndex(int paramCount) {
            return paramCount - _kwArgCount + _kwArgIndex;
        }
    }
}
