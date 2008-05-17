
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

using System.Diagnostics;

namespace Microsoft.Scripting.Ast {
    /// <summary>
    /// Determine if we should evaluate a given tree in interpreted mode.
    /// Currently, we can evaluate everything that does not contain as-yet-unsupported nodes.
    /// In the future, we may consider using heuristics based on size, presence of loops, etc.
    /// </summary>
    class InterpretChecker : Walker {
        private bool _hasUnsupportedNodes = false;
        private bool _hasLoops = false;
        private int _voidExpressionsDepth = 0;

        public bool EvaluationOK(bool useHeuristics) {
            if (useHeuristics) {
                return !_hasUnsupportedNodes && !_hasLoops;
            } else {
                return !_hasUnsupportedNodes;
            }
        }

        public static bool CanEvaluate(CodeBlock node, bool useHeuristics) {
            InterpretChecker walker = new InterpretChecker();
            walker.WalkNode(node);
            Debug.Assert(walker._voidExpressionsDepth == 0);
            return walker.EvaluationOK(useHeuristics);
        }

        //
        // Nodes we don't support.
        // Rather than adding to this category, consider implementing Evaluate() on the node!
        //

        protected internal override bool Walk(ParamsExpression node) {
            _hasUnsupportedNodes = true;
            return false;
        }

        protected internal override bool Walk(EnvironmentExpression node) {
            _hasUnsupportedNodes = true;
            return false;
        }

        protected internal override bool Walk(SwitchStatement node) {
            _hasUnsupportedNodes = true;
            return false;
        }

        protected internal override bool Walk(UnboundExpression node) {
            // Right now, locals() fails for nested functions.
            // This is a crude test, but at least it errs on the side of disabling evaluation.
            if (SymbolTable.IdToString(node.Name) == "locals") {
                _hasUnsupportedNodes = true;
                return false;
            } else {
                return true;
            }
        }

        protected internal override void PostWalk(CodeBlockExpression node) {
            // We use PostWalk here since Walk is only called for IsDeclarative=true

            // Currently, CodeBlockExpression ignores the DelegateType. This will result in the generation of 
            // an incorrectly type delegate. For eg, for event hookup with a source code snippet, a language might 
            // generate a rule that creates CodeBlockExpressions with the source code snippet typed as EventHandler.
            // The interpreter will ignore that a create a CallWithContextN delegate, which will cause a problem 
            // with the event hookup
            if (node.DelegateType != null) {
                _hasUnsupportedNodes = true;
            }
        }

        // Normally, expressions do not affect control-flow significantly. Only statements do.
        // Only statements check whether they should keep executing more statements or if
        // they should return a value. However, VoidExpression is a way to mix up expressions 
        // and statements. If a VoidExpression contains control flow, that causes a problem

        protected internal override bool Walk(VoidExpression node) {
            _voidExpressionsDepth++;
            return true;
        }

        protected internal override void PostWalk(VoidExpression node) {
            _voidExpressionsDepth--;
        }

        private bool DisallowControlFlowInVoidExpression() { 
            if (_voidExpressionsDepth > 0) {
                _hasUnsupportedNodes = true;
                return false;
            }
            return true;
        }
        
        protected internal override bool Walk(ReturnStatement node) { return DisallowControlFlowInVoidExpression(); }
        protected internal override bool Walk(ContinueStatement node) { return DisallowControlFlowInVoidExpression(); }
        protected internal override bool Walk(BreakStatement node) { return DisallowControlFlowInVoidExpression(); }

        //
        // Nodes that may take a long time to execute
        //
        protected internal override bool Walk(LoopStatement node) {
            _hasLoops = true;
            return false;
        }

        protected internal override bool Walk(DoStatement node) {
            _hasLoops = true;
            return false;
        }
    }
}

#endif	
