
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

using System;
using System.Collections.Generic;

namespace Microsoft.Scripting.Ast {
    /// <summary>
    /// ForestRewriter rewrites trees which contain multiple code blocks.
    /// It will do so by identifying all code blocks and rewriting each
    /// of them using AstRewriter
    /// 
    /// TODO: Consider merging this pass with ClosureBinder, or extract
    /// all CodeBlocks into a top level AST node so we don't have to go
    /// looking for them each time we need them.
    /// </summary>
    class ForestRewriter : Walker {
        /// <summary>
        /// List of blocks identified in the AST
        /// </summary>
        private List<CodeBlock> _blocks;
        private bool _rewrite;

        #region Forest rewriter entry point

        public static void Rewrite(CodeBlock block) {
            ForestRewriter fr = new ForestRewriter();

            // Collect the blocks that need rewriting
            fr.WalkNode(block);


#if FULL
            // If any do need rewriting, rewrite the blocks now
            if (fr._blocks != null) {
                foreach (CodeBlock cb in fr._blocks) {
                    AstRewriter.RewriteBlock(cb);
                }
            } 
#endif

        }

        #endregion

        // Void expression triggers rewrite
        protected internal override void PostWalk(VoidExpression node) {
            _rewrite = true;
        }

        // Yield statement triggers rewrite
        protected internal override void PostWalk(YieldStatement node) {
            _rewrite = true;
        }

        protected internal override bool Walk(CodeBlock node) {
            // Simple stack of flags
            bool backup = _rewrite;
            _rewrite = false;

            // Walk the code block body
            WalkNode(node.Body);

            // Save it if it needs rewriting
            if (_rewrite) {
                if (_blocks == null) {
                    _blocks = new List<CodeBlock>();
                }
                _blocks.Add(node);
            }

            // Pop from the stack
            _rewrite = backup;

            // Do not walk the body anymore
            return false;
        }
    }
}

#endif	
