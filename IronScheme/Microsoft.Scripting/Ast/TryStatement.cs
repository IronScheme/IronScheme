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
using System.Reflection.Emit;
using System.Collections.Generic;
using System.Collections.ObjectModel;

using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Generation.Slots;

namespace Microsoft.Scripting.Ast
{
    public class TryStatement : Statement {

        private struct CatchRecord {
            private Slot _slot;
            private CatchBlock _block;

            public CatchRecord(Slot slot, CatchBlock block) {
                _slot = slot;
                _block = block;
            }

            public Slot Slot {
                get { return _slot; }
            }

            public CatchBlock Block {
                get { return _block; }
            }
        }
        
        private readonly SourceLocation _header;
        private readonly Statement _body;
        private readonly ReadOnlyCollection<CatchBlock> _handlers;
        private readonly Statement _finally;

        /// <summary>
        /// Called by <see cref="TryStatementBuilder"/>.
        /// Creates a try/catch/finally/else block.
        /// 
        /// The body is protected by the try block.
        /// The handlers consist of a set of language-dependent tests which call into the LanguageContext.
        /// The elseSuite runs if no exception is thrown.
        /// The finallySuite runs regardless of how control exits the body.
        /// </summary>
        internal TryStatement(SourceSpan span, SourceLocation header, Statement body, ReadOnlyCollection<CatchBlock> handlers, Statement @finally)
            : base(AstNodeType.TryStatement, span) {
            _body = body;
            _handlers = handlers;
            _finally = @finally;
            _header = header;
        }

        public SourceLocation Header {
            get { return _header; }
        }

        public Statement Body {
            get { return _body; }
        }

        public ReadOnlyCollection<CatchBlock> Handlers {
            get { return _handlers; }
        }

        public Statement FinallyStatement {
            get { return _finally; }
        }

        private bool HaveHandlers() {
            return _handlers != null && _handlers.Count > 0;
        }

        private void EmitSimpleTry(CodeGen cg, TryFlowResult flow) {
            //
            // Initialize the flow control flag
            //
            Slot flowControlFlag = null;
            if (flow.Any) {
                Debug.Assert(_finally != null);

                flowControlFlag = cg.GetLocalTmp(typeof(int));
                cg.EmitInt(CodeGen.FinallyExitsNormally);
                flowControlFlag.EmitSet(cg);

                //  If there is a control flow in finally, emit outer:
                //  try {
                //      // try block body and all catch handling
                //  } catch (Exception all) {
                //      saved = all;
                //  } finally {
                //      finally_body
                //      if (saved != null) {
                //          throw saved;
                //      }
                //  }

                if (HaveHandlers()) {
                    cg.PushExceptionBlock(TargetBlockType.Try, flowControlFlag);
                    cg.BeginExceptionBlock();
                }
            }

            //******************************************************************
            // 1. ENTERING TRY
            //******************************************************************

            cg.PushExceptionBlock(TargetBlockType.Try, flowControlFlag);
            cg.BeginExceptionBlock();

            //******************************************************************
            // 2. Emit the try statement body
            //******************************************************************

            _body.Emit(cg);
            //cg.EmitSequencePointNone();

            //******************************************************************
            // 3. Emit the catch blocks
            //******************************************************************

            if (HaveHandlers()) {
                cg.PushExceptionBlock(TargetBlockType.Catch, flowControlFlag);

                foreach (CatchBlock cb in _handlers) {
                    // Begin the strongly typed exception block
                    cg.BeginCatchBlock(cb.Test);

                    // Save the exception (if the catch block asked for it) or pop
                    EmitSaveExceptionOrPop(cg, cb);

                    //
                    // Emit the catch block body
                    //
                    cb.Body.Emit(cg);
                }

                cg.PopTargets(TargetBlockType.Catch);
            }

            //******************************************************************
            // 4. Emit the finally block
            //******************************************************************

            if (_finally != null) {
                Slot rethrow = null;
                if (flow.Any) {
                    // If there is a control flow in finally, end the catch
                    // statement and emit the catch-all and finally clause
                    // with rethrow at the end.

                    if (HaveHandlers()) {
                        cg.EndExceptionBlock();
                        cg.PopTargets(TargetBlockType.Try);
                    }

                    cg.PushExceptionBlock(TargetBlockType.Catch, flowControlFlag);
                    cg.BeginCatchBlock(typeof(Exception));

                    rethrow = cg.GetLocalTmp(typeof(Exception));
                    rethrow.EmitSet(cg);

                    cg.PopTargets(TargetBlockType.Catch);
                }

                cg.PushExceptionBlock(TargetBlockType.Finally, flowControlFlag);
                cg.BeginFinallyBlock();

                //
                // Emit the finally block body
                //
                _finally.Emit(cg);

                if (flow.Any) {
                    Debug.Assert(rethrow != null);
                    Label noRethrow = cg.DefineLabel();

                    rethrow.EmitGet(cg);
                    cg.EmitNull();
                    cg.Emit(OpCodes.Beq, noRethrow);
                    rethrow.EmitGet(cg);
                    cg.Emit(OpCodes.Throw);
                    cg.MarkLabel(noRethrow);
                }

                cg.EndExceptionBlock();
                cg.PopTargets(TargetBlockType.Finally);
            } else {
                cg.EndExceptionBlock();
            }

            cg.PopTargets(TargetBlockType.Try);

            //
            // Emit the flow control for finally, if there was any.
            //
            EmitFinallyFlowControl(cg, flow, flowControlFlag);

            cg.FreeLocalTmp(flowControlFlag);
        }

        /// <summary>
        /// If the finally statement contains break, continue, return or yield, we need to
        /// handle the control flow statement after we exit out of finally via OpCodes.Endfinally.
        /// </summary>
        private static void EmitFinallyFlowControl(CodeGen cg, TryFlowResult flow, Slot flag) {
            if (flow.Return) {
                Debug.Assert(flag != null);

                Label noReturn = cg.DefineLabel();

                flag.EmitGet(cg);
                cg.EmitInt(CodeGen.BranchForReturn);
                cg.Emit(OpCodes.Bne_Un, noReturn);

                if (flow.Any) {
                    // return the actual value
                    cg.EmitReturnValue();
                    cg.EmitReturn();
                }
                cg.MarkLabel(noReturn);
            }

            // Only emit break handling if it is actually needed
            if (flow.Break) {
                Debug.Assert(flag != null);

                Label noReturn = cg.DefineLabel();
                flag.EmitGet(cg);
                cg.EmitInt(CodeGen.BranchForBreak);
                cg.Emit(OpCodes.Bne_Un, noReturn);
                cg.EmitBreak();
                cg.MarkLabel(noReturn);
            }

            // Only emit continue handling if it if actually needed
            if (flow.Continue) {
                Debug.Assert(flag != null);

                Label noReturn = cg.DefineLabel();
                flag.EmitGet(cg);
                cg.EmitInt(CodeGen.BranchForContinue);
                cg.Emit(OpCodes.Bne_Un, noReturn);
                cg.EmitContinue();
                cg.MarkLabel(noReturn);
            }
        }

        private static void EmitSaveExceptionOrPop(CodeGen cg, CatchBlock cb) {
            if (cb.Variable != null) {
                Debug.Assert(cb.Slot != null);
                // If the variable is present, store the exception
                // in the variable.
                cb.Slot.EmitSet(cg);
            } else {
                // Otherwise, pop it off the stack.
                cg.Emit(OpCodes.Pop);
            }
        }

        public override void Emit(CodeGen cg) {
            // Codegen is affected by presence/absence of loop control statements
            // (break/continue) or return/yield statement in finally clause
            TryFlowResult flow = TryFlowAnalyzer.Analyze(FinallyStatement);

            // If there's a yield anywhere, go for a complex codegen
            EmitSimpleTry(cg, flow);
        }
    }
}
