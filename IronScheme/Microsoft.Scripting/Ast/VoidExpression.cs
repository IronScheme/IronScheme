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
using System.Diagnostics;
using Microsoft.Scripting.Utils;
using Microsoft.Scripting.Generation;

namespace Microsoft.Scripting.Ast {
    public class VoidExpression : Expression {
        private Statement _statement;

        internal VoidExpression(Statement statement)
            : base(AstNodeType.VoidExpression) {
            _statement = statement;
        }

        public override Type Type {
            get {
                return typeof(void);
            }
        }

        public Statement Statement {
            get { return _statement; }
        }

        public override void Emit(CodeGen cg) {
            _statement.Emit(cg);
        }


#if FULL
        protected override object DoEvaluate(CodeContext context) {
            object ret = _statement.Execute(context);
            // The interpreter is not able to deal with control flow inside of expressions

            if (ret != Statement.NextStatement) {
                throw new ExpressionReturnException(ret);
            }

            return null;
        } 
#endif

    }

    /// <summary>
    /// Factory methods
    /// </summary>
    public static partial class Ast {
        public static VoidExpression Void(Statement statement) {
            Contract.RequiresNotNull(statement, "statement");
            return new VoidExpression(statement);
        }
    }
}
