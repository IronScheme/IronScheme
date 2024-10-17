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

using Microsoft.Scripting.Ast;

namespace Microsoft.Scripting.Generation.Builders
{
    using Ast = Ast.Ast;

    /// <summary>
    /// ArgBuilder which always produces null.  
    /// </summary>
    internal class NullArgBuilder : ArgBuilder
    {
        public NullArgBuilder() { }

        public override int Priority
        {
            get { return 0; }
        }

        public override object Build(CodeContext context, object[] args)
        {
            return null;
        }

        internal override Expression ToExpression(MethodBinderContext context, Expression[] parameters)
        {
            return Ast.Null();
        }

        internal override Expression CheckExpression(MethodBinderContext context, Expression[] parameters)
        {
            return null;
        }
    }
}
