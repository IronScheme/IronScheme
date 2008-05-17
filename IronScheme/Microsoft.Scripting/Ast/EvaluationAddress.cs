
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

namespace Microsoft.Scripting.Ast {
    class EvaluationAddress {
        private readonly Expression /*!*/ _expr;

        public EvaluationAddress(Expression /*!*/ expression) {
            _expr = expression;
        }

        public virtual object GetValue(CodeContext context, bool outParam) {            
            return _expr.Evaluate(context);            
        }

        public virtual object AssignValue(CodeContext context, object value) {
            return _expr.EvaluateAssign(context, value);
        }

        protected Expression Expression {
            get {
                return _expr;
            }
        }
    }   
}

#endif	
