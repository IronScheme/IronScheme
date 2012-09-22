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

using System.Collections.Generic;
using System.Collections.ObjectModel;
using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Ast {
    public class BlockStatement : Statement {
        private readonly ReadOnlyCollection<Statement> /*!*/ _statements;

        public ReadOnlyCollection<Statement> /*!*/ Statements {
            get { return _statements; }
        }

        internal BlockStatement(SourceSpan span, ReadOnlyCollection<Statement> /*!*/ statements)
            : base(AstNodeType.BlockStatement, span) {
            _statements = statements;
            }


        public override void Emit(CodeGen cg) {
            //cg.EmitPosition(Span.Start, Span.End);
            // Should emit nop for the colon?
            foreach (Statement stmt in _statements) {
                stmt.Emit(cg);
            }
        }
    }

    public static partial class Ast {
        public static Statement Block(List<Statement> statements) {
            Contract.RequiresNotNullItems(statements, "statements");

            if (statements.Count == 1) {
                return statements[0];
            } else {
                return Block(statements.ToArray());
            }
        }

        public static BlockStatement Block(params Statement[] statements) {
            return Block(SourceSpan.None, statements);
        }

        public static BlockStatement Block(SourceSpan span, params Statement[] statements) {
            Contract.RequiresNotNullItems(statements, "statements");
            return new BlockStatement(span, CollectionUtils.ToReadOnlyCollection(statements));
        }
    }
}
