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
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;

using Microsoft.Scripting.Generation;

/*
 * The data flow.
 * 
 * Each local name is represented as 2 bits:
 * One is for definitive assignment, the other is for uninitialized use detection.
 * The only difference between the two is behavior on delete.
 * On delete, the name is not assigned to meaningful value (we need to check at runtime if it's initialized),
 * but it is not uninitialized either (because delete statement will set it to Uninitialized.instance).
 * This way, codegen doesn't have to emit an explicit initialization for it.
 * 
 * Consider:
 * 
 * def f():
 *     print a  # uninitialized use
 *     a = 10
 * 
 * We compile this into:
 * 
 * static void f$f0() {
 *     object a = Uninitialized.instance; // explicit initialization because of the uninitialized use
 *     // print statement
 *     if(a == Uninitialized.instance)
 *       throw ThrowUnboundLocalError("a");
 *     else
 *       Ops.Print(a);
 *     // a = 10
 *     a = 10
 * }
 * 
 * Whereas:
 * 
 * def f():
 *     a = 10
 *     del a        # explicit deletion which will set to Uninitialized.instance
 *     print a
 * 
 * compiles into:
 * 
 * static void f$f0() {
 *     object a = 10;                        // a = 10
 *     a = Uninitialized.instance;           // del a
 *     if(a == Uninitialized.instance)       // print a
 *       throw ThrowUnboundLocalError("a");
 *     else
 *       Ops.Print(a);
 * }
 * 
 * The bit arrays in the flow checker hold the state and upon encountering NameExpr we figure
 * out whether the name has not yet been initialized at all (in which case we need to emit the
 * first explicit assignment to Uninitialized.instance and guard the use with an inlined check
 * or whether it is definitely assigned (we don't need to inline the check)
 * or whether it may be uninitialized, in which case we must only guard the use by inlining the Uninitialized check
 * 
 * More details on the bits.
 * 
 * First bit:
 *  1 .. value is definitely assigned (initialized and != Uninitialized.instance)
 *  0 .. value may be uninitialized or set to Uninitialized.instance
 * Second bit:
 *  1 .. is definitely initialized  (including definitely initialized to Uninitialized.instance)
 *  0 .. may be uninitialized
 * 
 * In combination:
 *  11 .. initialized
 *  10 .. invalid
 *  01 .. deleted
 *  00 .. may be uninitialized
 */

namespace Microsoft.Scripting.Ast {
    class FlowChecker : Walker {

        private struct ExitState {
            readonly BitArray _exit;
            readonly Statement _statement;

            public ExitState(BitArray exit)
                : this(exit, null) {
            }

            public ExitState(BitArray exit, Statement statement) {
                _exit = exit;
                _statement = statement;
            }

            public Statement Statement {
                get { return _statement; }
            }

            public BitArray Exit {
                get { return _exit; }
            }

            public void And(BitArray bits) {
                if (_exit != null) {
                    _exit.And(bits);
                }
            }
        }

        private BitArray _bits;
        private List<ExitState> _exit;

        CodeBlock _block;

        Dictionary<Variable, int> _indices = new Dictionary<Variable,int>();

        private FlowChecker(CodeBlock block) {
            List<Variable> variables = block.Variables;
            List<Variable> parameters = block.Parameters;

            _bits = new BitArray((variables.Count + parameters.Count) * 2);
            int index = 0;
            foreach (Variable variable in variables) {
                _indices[variable] = index++;
            }
            foreach (Variable parameter in parameters) {
                _indices[parameter] = index++;
            }
            _block = block;
        }

        private bool TryGetIndex(Variable variable, out int index) {
            if (variable != null) {
                if (_indices.TryGetValue(variable, out index)) {
                    index *= 2;
                    return true;
                } else {
                  if (variable.Name == SymbolTable.StringToId("body")
                    && variable.Kind == Variable.VariableKind.Parameter
                    && variable.Block.Name.StartsWith("define-macro"))
                  {
                    Trace.WriteLine("skipping problem variable");
                  }
                  else
                  {
                    // locals and parameters must have be tracked, except for global scope
                    Debug.Assert(
                        variable.Kind != Variable.VariableKind.Local &&
                        variable.Kind != Variable.VariableKind.Parameter ||
                        variable.Lift ||
                        variable.Block.IsGlobal,
                        "Untracked local/parameter " + variable.Name.ToString()
                    );
                  }
                }
            }

            index = -1;
            return false;
        }

        private bool TryCheckVariable(Variable variable, out bool defined) {
            int index;
            if (TryGetIndex(variable, out index)) {
                Debug.Assert(index < _bits.Count);
                defined = _bits.Get(index);
                if (!defined) {
                    variable.UninitializedUse();
                }
                if (!_bits.Get(index + 1)) {
                    // Found an unbound use of the name => need to initialize to Uninitialized.instance
                    variable.UnassignedUse();
                }
                return true;
            } else {
                // TODO: Report unbound name - error
                defined = false;
                return false;
            }
        }

        #region Public API

        public static void Check(CodeBlock block) {
            FlowChecker fc = new FlowChecker(block);
            fc.WalkNode(block);
        }

        #endregion

        public void Define(Variable variable) {
            int index;
            if (TryGetIndex(variable, out index)) {
                _bits.Set(index, true);      // is assigned
                _bits.Set(index + 1, true);  // cannot be unassigned
            }
        }

        public void Delete(Variable variable) {
            int index;
            if (TryGetIndex(variable, out index)) {
                _bits.Set(index, false);     // is not initialized
                _bits.Set(index + 1, true);  // is assigned (to Uninitialized.instance)
            }
        }

        private void PushStatement(BitArray ba) {
            PushStatement(ba, null);
        }

        private void PushStatement(BitArray ba, Statement statement) {
            if (_exit == null) {
                _exit = new List<ExitState>();
            }
            _exit.Add(new ExitState(ba, statement));
        }

        private ExitState PeekStatement(Statement statement) {
            Debug.Assert(_exit != null && _exit.Count > 0);
            if (statement == null) {
                return _exit[_exit.Count - 1];
            } else {
                for (int i = _exit.Count - 1; i >= 0; i--) {
                    if (_exit[i].Statement == statement) {
                        return _exit[i];
                    }
                }
                return default(ExitState);
            }
        }

        private void PopStatement() {
            Debug.Assert(_exit != null && _exit.Count > 0);
            _exit.RemoveAt(_exit.Count - 1);
        }

        #region AstWalker Methods

        // BoundExpression
        protected internal override bool Walk(BoundExpression node) {
            bool defined;
            if (TryCheckVariable(node.Variable, out defined)) {
                node.IsDefined = defined;
            }
            return true;
        }

        // BoundAssignment
        protected internal override bool Walk(BoundAssignment node) {
            WalkNode(node.Value);
            Define(node.Variable);
            return false;
        }


        // BreakStatement
        protected internal override bool Walk(BreakStatement node) {
            ExitState exit = PeekStatement(node.Statement);
            exit.And(_bits);
            return true;
        }

        // ContinueStatement
        protected internal override bool Walk(ContinueStatement node) { return true; }

        // DelStatement
        protected internal override bool Walk(DeleteStatement node) {
            bool defined;
            if (TryCheckVariable(node.Variable, out defined)) {
                node.IsDefined = defined;
            }
            return true;
        }

        protected internal override void PostWalk(DeleteStatement node) {
            Delete(node.Variable);
        }

        // CodeBlockExpression interrupt flow analysis
        protected internal override bool Walk(CodeBlockExpression node) {
            return false;
        }

        // CodeBlock
        protected internal override bool Walk(CodeBlock node) {
            foreach (Variable p in node.Parameters) {
                // Define the parameters
                Define(p);
            }
            return true;
        }

        // GeneratorCodeBlock
        protected internal override bool Walk(GeneratorCodeBlock node) {
            return Walk((CodeBlock)node);
        }

        // DoStatement
        protected internal override bool Walk(DoStatement node) {
            BitArray loop = new BitArray(_bits); // State at the loop entry with which the loop runs
            BitArray save = _bits;               // Save the state at loop entry

            // Prepare loop exit state
            BitArray exit = new BitArray(_bits.Length, true);
            PushStatement(exit);

            // Loop will be flown starting from the current state
            _bits = loop;

            // Walk the loop
            WalkNode(node.Body);
            // Walk the test in the context of the loop
            WalkNode(node.Test);

            // Handle the loop exit
            PopStatement();
            _bits.And(exit);

            // Restore the state after walking the loop
            _bits = save;
            _bits.And(loop);

            return false;
        }

        // IfStatement
        protected internal override bool Walk(IfStatement node) {
            BitArray result = new BitArray(_bits.Length, true);
            BitArray save = _bits;

            _bits = new BitArray(_bits.Length);

            foreach (IfStatementTest ist in node.Tests) {
                // Set the initial branch value to bits
                _bits.SetAll(false);
                _bits.Or(save);

                // Flow the test first
                WalkNode(ist.Test);
                // Flow the body
                WalkNode(ist.Body);
                // Intersect
                result.And(_bits);
            }

            // Set the initial branch value to bits
            _bits.SetAll(false);
            _bits.Or(save);

            if (node.ElseStatement != null) {
                // Flow the else_
                WalkNode(node.ElseStatement);
            }

            // Intersect
            result.And(_bits);

            _bits = save;

            // Remember the result
            _bits.SetAll(false);
            _bits.Or(result);
            return false;
        }

        // LoopStatement
        protected internal override bool Walk(LoopStatement node) {
            // Expression is executed always at least once
            if (node.Test != null) {
                WalkNode(node.Test);
            }

            // Beyond this point, either body will be executed (test succeeded),
            // or else is getting executed (test failed). There is no guarantee
            // that both will be executed, though.
            BitArray opte = new BitArray(_bits);
            BitArray exit = new BitArray(_bits.Length, true);

            PushStatement(exit);
            WalkNode(node.Body);
            WalkNode(node.Increment);
            PopStatement();

            _bits.And(exit);

            if (node.ElseStatement != null) {
                // Flow the else
                BitArray save = _bits;
                _bits = opte;
                WalkNode(node.ElseStatement);
                // Restore the bits
                _bits = save;
            }

            // Intersect
            _bits.And(opte);

            return false;
        }

        // TryStatement
        protected internal override bool Walk(TryStatement node) {
            // The try body is guaranteed to be entered, but not completed,
            // the catch blocks are not guaranteed to be entered at all,
            // the finally block is guaranteed to be entered
            //
            // Any catch can be preceded by partial execution of try block,
            // so any 'damage' (deletes) the try block does must be affected
            // in the entry to the catch block. All catches have identical
            // starting situation.
            //
            // The finally can be preceded by partial execution of the try,
            // and at most one of the catches (any of them) so again, the
            // 'damage' the try and catch blocks do to the local state
            // affects the entry to the finally block.
            BitArray entry = _bits;
            _bits = new BitArray(_bits);

            // 1. Flow the body
            WalkNode(node.Body);
            entry.And(_bits);

            // 2. Flow the catch clauses, starting always with the initial state,
            //    but also including the 'damage' that try block could have done.
            int handlerCount;
            if (node.Handlers != null && (handlerCount = node.Handlers.Count) > 0) {
                for (int i = 0; i < handlerCount; i++) {
                    // Initialize the bits for flowing the catch clause
                    _bits.SetAll(false);
                    _bits.Or(entry);

                    // Flow the catch clause and propagate the 'damage' to the state we'll use for finally.
                    WalkNode(node.Handlers[i]);
                    entry.And(_bits);
                }
            }

            // 3. Restore the state to the original (including the effects the body and catch clauses had)
            _bits = entry;

            // 4. Flow the finally clause, if present.
            if (node.FinallyStatement != null) {
                WalkNode(node.FinallyStatement);
            }

            return false;
        }

        // SwitchStatement
        protected internal override bool Walk(SwitchStatement node) {
            // The expression is evaluated always.
            // Then each case clause expression is evaluated until match is found.
            // Therefore, the effects of the case clause expressions accumulate.
            // Default clause is evaluated last (so all case clause expressions must
            // accumulate first)
            WalkNode(node.TestValue);

            // Flow all the cases, they all start with the same initial state
            int count;
            IList<SwitchCase> cases = node.Cases;
            if (cases != null && (count = cases.Count) > 0) {
                SwitchCase @default = null;
                // Save the initial state
                BitArray entry = _bits;
                // The state to progressively accumualte effects of the case clause expressions
                BitArray values = new BitArray(entry);
                // State to flow the case clause bodies.
                BitArray caseFlow = new BitArray(_bits.Length);
                // The state to accumulate results into
                BitArray result = new BitArray(_bits.Length, true);

                PushStatement(result);

                for (int i = 0; i < count; i++) {
                    if (cases[i].IsDefault) {
                        Debug.Assert(@default == null);

                        // postpone the default case
                        @default = cases[i];
                        continue;
                    }

                    // Set the state for the walking of the body
                    caseFlow.SetAll(false);
                    caseFlow.Or(values);

                    // Walk the body
                    _bits = caseFlow;
                    WalkNode(cases[i].Body);

                    // Accumulate the result into the overall case statement result.
                    result.And(caseFlow);
                }

                // Walk the default at the end.
                if (@default != null) {
                    // Initialize
                    caseFlow.SetAll(false);
                    caseFlow.Or(values);

                    // Walk the default body
                    _bits = caseFlow;
                    WalkNode(@default.Body);

                    // Accumulate.
                    result.And(caseFlow);

                    // If there's a default clause, exactly one case got executed.
                    // The final state is 'and' across all cases, stored in 'result'
                    entry.SetAll(false);
                    entry.Or(result);
                } else {
                    // In the absence of default clause, we may have executed case,
                    // but didn't have to, so the result is an 'and' between the cases
                    // and the initial state.
                    entry.And(result);
                }

                PopStatement();

                // Restore the original state.
                _bits = entry;
            }

            return false;
        }

        // LabeledStatement
        protected internal override bool Walk(LabeledStatement node) {
            BitArray exit = new BitArray(_bits.Length, true);
            PushStatement(exit, node);

            WalkNode(node.Statement);

            PopStatement();
            _bits.And(exit);
            return false;
        }

        #endregion

        [Conditional("DEBUG")]
        public void Dump(BitArray bits) {
            System.Text.StringBuilder sb = new System.Text.StringBuilder();
            sb.AppendFormat("FlowChecker ({0})", _block is CodeBlock ? ((CodeBlock)_block).Name : "");
            sb.Append('{');
            bool comma = false;

            foreach (KeyValuePair<Variable, int> kv in _indices) {
                Variable variable = kv.Key;
                int index = kv.Value * 2;

                if (comma) sb.Append(", ");
                else comma = true;
                sb.AppendFormat("{0}:{1}{2}",
                    SymbolTable.IdToString(variable.Name),
                    bits.Get(index) ? "*" : "-",
                    bits.Get(index + 1) ? "-" : "*");
                if (variable.Unassigned)
                    sb.Append("#");
            }
            sb.Append('}');
            Debug.WriteLine(sb.ToString());
        }
    }
}
