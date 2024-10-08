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
    /// The structure to capture the result of the control flow for the
    /// try statement code generation.
    /// </summary>
    struct TryFlowResult {
        /// <summary>
        /// There is a top level "break" in the analyzed statement.
        /// Breaks nested in "switch" or loops do not count because they
        /// do not exit the block of code being analyzed.
        /// </summary>
        private bool _break;

        /// <summary>
        /// There is a top level continue in the analyzed block.
        /// Continue statement nested in the loop doesn't count as it
        /// doesn't leave the block being analyzed.
        /// </summary>
        private bool _continue;

        /// <summary>
        /// There is a return statement in the analyzed block.
        /// </summary>
        private bool _return;

        public bool Break {
            get { return _break; }
            set { _break = value; }
        }
        public bool Continue {
            get { return _continue; }
            set { _continue = value; }
        }
        public bool Return {
            get { return _return; }
            set { _return = value; }
        }

        public bool Any {
            get { return _break || _continue || _return; }
        }
    };

    /// <summary>
    /// AST walker to analyze control flow for the try statement code generation.
    /// </summary>
    class TryFlowAnalyzer : CodeBlockWalker {
        /// <summary>
        /// Tracking the result of the analysis.
        /// </summary>
        private TryFlowResult _result;

        public static TryFlowResult Analyze(Statement statement) {
            if (statement == null) {
                return new TryFlowResult();
            } else {
                // find it now.
                TryFlowAnalyzer tfa = new TryFlowAnalyzer();
                tfa.WalkNode(statement);
                return tfa._result;
            }
        }

        protected internal override bool Walk(ContinueStatement node) {
            _result.Continue = true;
            return true;
        }
        protected internal override bool Walk(ReturnStatement node) {
            _result.Return = true;
            return true;
        }
    }
}
