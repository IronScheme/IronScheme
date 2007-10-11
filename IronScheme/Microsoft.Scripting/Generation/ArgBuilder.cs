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

using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Actions;

namespace Microsoft.Scripting.Generation {
    /// <summary>
    /// ArgBuilder provides an argument value used by the MethodBinder.  One ArgBuilder exists for each
    /// physical parameter defined on a method.  
    /// </summary>
    public abstract class ArgBuilder {
        public abstract int Priority {
            get;
        }

        public virtual AbstractValue AbstractBuild(AbstractContext context, IList<AbstractValue> parameters) {
            throw new NotImplementedException();
        }

        /// <summary>
        /// Provides the Expression which provides the value to be passed to the argument.
        /// </summary>
        internal abstract Expression ToExpression(MethodBinderContext context, Expression[] parameters);

        /// <summary>
        /// Provides an Expression which returns a Boolean determining whether or not the parameter is convertible
        /// to the receiving arguments type.  This is currently only used for a MethodBinder of BinderType.BinaryOperator.
        /// 
        /// This function should be removed when BinaryOperator support is removed.
        /// </summary>
        internal abstract Expression CheckExpression(MethodBinderContext context, Expression[] parameters);

        /// <summary>
        /// Provides an Expression which will update the provided value after a call to the method.  May
        /// return null if no update is required.
        /// </summary>
        internal virtual Expression UpdateFromReturn(MethodBinderContext context, Expression[] parameters) {
            return null;
        }

        /// <summary>
        /// If the argument produces a return value (e.g. a ref or out value) this provides
        /// the additional value to be returned.
        /// </summary>
        internal virtual Expression ToReturnExpression(MethodBinderContext context) {
            throw new InvalidOperationException();
        }

        /// <summary>
        /// Builds the value of the argument to be passed for a call via reflection.
        /// </summary>
        public abstract object Build(CodeContext context, object[] args);

        /// <summary>
        /// If the argument produces a return value (e.g. a ref or out value) this
        /// provides the additional value to be returned.
        /// </summary>
        public virtual void UpdateFromReturn(object callArg, object[] args) { }

    }
}
