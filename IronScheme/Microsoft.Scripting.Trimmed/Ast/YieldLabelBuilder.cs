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
using System.Collections.Generic;
using Microsoft.Scripting.Generation;

namespace Microsoft.Scripting.Ast {
    class YieldLabelBuilder : CodeBlockWalker {
        sealed class ExceptionBlock {
            public enum TryStatementState {
                Try,
                Handler,
                Finally
            };

            private readonly TryStatement _statement;
            private TryStatementState _state;
            private int _handler;

            public ExceptionBlock(TryStatement statement) {
                Debug.Assert(statement != null);

                _state = TryStatementState.Try;
                _statement = statement;
            }

            internal TryStatementState State {
                get { return _state; }
                set { _state = value; }
            }

            internal int Handler {
                get { return _handler; }
                set { _handler = value; }
            }

            /// <summary>
            /// Adds yield target to the current try statement and returns the label
            /// to which the outer code must jump to to route properly to this label.
            /// </summary>
            internal TargetLabel AddYieldTarget(TargetLabel label, int index) {
                switch (State) {
                    case TryStatementState.Try:
                        return _statement.AddTryYieldTarget(label, index);
                    case TryStatementState.Handler:
                        return _statement.AddCatchYieldTarget(label, index, _handler);
                    case TryStatementState.Finally:
                        return _statement.AddFinallyYieldTarget(label, index);

                    default:
                        Debug.Assert(false, "Invalid try statement state " + State.ToString());
                        throw new System.InvalidOperationException();
                }
            }
        }

        private readonly Stack<ExceptionBlock> _tryBlocks = new Stack<ExceptionBlock>();
        private readonly List<YieldTarget> _topTargets = new List<YieldTarget>();
        private int _temps;

        private YieldLabelBuilder() {
        }

        internal static void BuildYieldTargets(GeneratorCodeBlock g, out List<YieldTarget> topTargets, out int temps) {
            YieldLabelBuilder b = new YieldLabelBuilder();
            b.WalkNode(g.Body);
            topTargets = b._topTargets;
            temps = b._temps;
        }

        #region AstWalker method overloads

        protected internal override bool Walk(TryStatement node) {
            ExceptionBlock block = new ExceptionBlock(node);

            _tryBlocks.Push(block);
            WalkNode(node.Body);

            IList<CatchBlock> handlers = node.Handlers;
            if (handlers != null) {
                block.State = ExceptionBlock.TryStatementState.Handler;
                for (int handler = 0; handler < handlers.Count; handler++) {
                    block.Handler = handler;
                    WalkNode(handlers[handler].Body);
                }
            }

            if (node.FinallyStatement != null) {
                block.State = ExceptionBlock.TryStatementState.Finally;
                WalkNode(node.FinallyStatement);
            }

            Debug.Assert((object)block == (object)_tryBlocks.Peek());
            _tryBlocks.Pop();

            return false;
        }

        protected internal override void PostWalk(TryStatement node) {
            _temps += node.GetGeneratorTempCount();
        }

        protected internal override void PostWalk(YieldStatement node) {
            // Assign the yield statement index for codegen
            int index = _topTargets.Count;
            TargetLabel label = new TargetLabel();
            node.Target = new YieldTarget(index, label);

            foreach (ExceptionBlock eb in _tryBlocks) {
                // The exception statement must determine
                // the label for the enclosing code to jump to
                // to return to the given yield target

                label = eb.AddYieldTarget(label, index);
            }

            // Insert the top target to the top yields
            Debug.Assert(_topTargets.Count == index);
            _topTargets.Add(new YieldTarget(index, label));
        }

        #endregion
    }
}
