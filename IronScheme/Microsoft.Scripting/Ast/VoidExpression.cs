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
using System.Diagnostics;
using Microsoft.Scripting.Utils;
using Microsoft.Scripting.Generation;

namespace Microsoft.Scripting.Ast {
    public class VoidExpression : Expression {
        private Statement _statement;

        internal VoidExpression(Statement statement) {
            if (statement == null) {
                throw new ArgumentNullException("statement");
            }
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

        protected override object DoEvaluate(CodeContext context) {
            object ret = _statement.Execute(context);
            // The interpreter is not able to deal with control flow inside of expressions

            if (ret != Statement.NextStatement) {
                throw new ExpressionReturnException(ret);
            }

            return null;
        }

        public override void Walk(Walker walker) {
            if (walker.Walk(this)) {
                _statement.Walk(walker);
            }
            walker.PostWalk(this);
        }
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

    [Serializable]
    internal class ExpressionReturnException : Exception {
        public readonly object Value;
#if DEBUG
        // used verify that we always handle the exception w/ the correct handler
        // and no catch(Exception) sneaks in and catches our exception.
        [ThreadStatic]
        internal static int CurrentDepth;
        public int Depth;
#endif

        public ExpressionReturnException(object value) {
            Value = value;
#if DEBUG
            Depth = CurrentDepth;
#endif
        }
    }
}
