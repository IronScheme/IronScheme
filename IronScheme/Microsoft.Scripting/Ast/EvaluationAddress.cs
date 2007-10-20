using System;
using System.Collections.Generic;
using System.Text;

namespace Microsoft.Scripting.Ast {
    class EvaluationAddress {
        private Expression _expr;

        public EvaluationAddress(Expression expression) {
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
