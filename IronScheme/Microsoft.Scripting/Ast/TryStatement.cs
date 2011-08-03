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

namespace Microsoft.Scripting.Ast {
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

        private TargetLabel _target;    // The entry point into the try statement

        /// <summary>
        /// One or more of the catch blocks includes yield.
        /// </summary>
        private bool _yieldInCatch;

        /// <summary>
        /// Labels for the yields inside a try block.
        /// </summary>
        private List<YieldTarget> _tryYields;

        /// <summary>
        /// Labels for the yields inside the catch clause.
        /// This is only valid for the try statement with a 'finally' clause,
        /// in which case we need to enter the outer try, and then dispatch
        /// to the yield labels
        /// For try statement without finally, the yields contained within
        /// catch are hoisted outside of the try and as such don't need
        /// to be tracked
        /// </summary>
        private List<YieldTarget> _catchYields;

        /// <summary>
        /// Labels for the yields inside a finally block.
        /// </summary>
        private List<YieldTarget> _finallyYields;

        [ThreadStatic]
        private static List<Exception> _evalExceptions;

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


#if FULL
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2219:DoNotRaiseExceptionsInExceptionClauses")]
        protected override object DoExecute(CodeContext context) {
            bool rethrow = false;
            Exception savedExc = null;
            object ret = Statement.NextStatement;
            try {
                ret = _body.Execute(context);
            } catch (Exception exc) {
                rethrow = true;
                savedExc = exc;
                if (_handlers != null) {
                    PushEvalException(exc);
                    try {
                        foreach (CatchBlock handler in _handlers) {
                            if (handler.Test.IsInstanceOfType(exc)) {
                                rethrow = false;
                                if (handler.Variable != null) {
                                    BoundAssignment.EvaluateAssign(context, handler.Variable, exc);
                                }
                                ret = handler.Body.Execute(context);
                                break;
                            }
                        }
                    } finally {
                        PopEvalException();
                    }
                }
            } finally {
                if (_finally != null) {
                    object finallyRet = _finally.Execute(context);
                    if (finallyRet != Statement.NextStatement) {
                        ret = finallyRet;
                        rethrow = false;
                    }
                }
                if (rethrow) {
                    throw ExceptionHelpers.UpdateForRethrow(savedExc);
                }
            }

            return ret;
        } 
#endif


        private static void PopEvalException() {
            _evalExceptions.RemoveAt(_evalExceptions.Count - 1);
            if (_evalExceptions.Count == 0) _evalExceptions = null;
        }

        private static void PushEvalException(Exception exc) {
            if (_evalExceptions == null) _evalExceptions = new List<Exception>();
            _evalExceptions.Add(exc);
        }

        internal static Exception LastEvalException {
            get {
                if (_evalExceptions == null || _evalExceptions.Count == 0) {
                    throw new InvalidOperationException("rethrow outside of catch block");
                }

                return _evalExceptions[_evalExceptions.Count - 1];
            }
        }

        private void EmitGeneratorTry(CodeGen cg, TryFlowResult flow) {
            //
            // Initialize the flow control flag
            //
            Slot flowControlFlag = null;

            if (flow.Any) {
                flowControlFlag = cg.GetLocalTmp(typeof(int));
                cg.EmitInt(CodeGen.FinallyExitsNormally);
                flowControlFlag.EmitSet(cg);
            }

            Slot exception = null;
            if (_finally != null) {
                exception = cg.GetTemporarySlot(typeof(Exception));
                cg.EmitNull();
                exception.EmitSet(cg);
            }

            //******************************************************************
            // Entering the try block
            //******************************************************************

            if (_target != null) {
                cg.MarkLabel(_target.EnsureLabel(cg));
            }

            //******************************************************************
            // If we have a 'finally', transform it into try..catch..finally
            // and rethrow
            //******************************************************************
            Label endFinallyBlock = new Label();
            if (_finally != null) {
                cg.PushExceptionBlock(TargetBlockType.Try, flowControlFlag);
                cg.BeginExceptionBlock();
                endFinallyBlock = cg.DefineLabel();

                //**************************************************************
                // If there is a yield in any catch, that catch will be hoisted
                // and we need to dispatch to it from here
                //**************************************************************
                if (_yieldInCatch) {
                    EmitYieldDispatch(_catchYields, cg);
                }

                if (YieldInBlock(_finallyYields)) {
                    foreach (YieldTarget yt in _finallyYields) {
                        cg.GotoRouter.EmitGet(cg);
                        cg.EmitInt(yt.Index);
                        cg.Emit(OpCodes.Beq, endFinallyBlock);
                    }
                }
            }

            //******************************************************************
            // If we have a 'catch', start a try block to handle all the catches
            //******************************************************************

            Label endCatchBlock = new Label();
            if (HaveHandlers()) {
                cg.PushExceptionBlock(TargetBlockType.Try, flowControlFlag);
                endCatchBlock = cg.BeginExceptionBlock();
            }

            //******************************************************************
            // Emit the try block body
            //******************************************************************

            // First, emit the dispatch within the try block
            EmitYieldDispatch(_tryYields, cg);

            // Then, emit the actual body
            _body.Emit(cg);
            //cg.EmitSequencePointNone();

            //******************************************************************
            // Emit the catch blocks
            //******************************************************************

            if (HaveHandlers()) {
                List<CatchRecord> catches = new List<CatchRecord>();
                cg.PushExceptionBlock(TargetBlockType.Catch, flowControlFlag);

                foreach (CatchBlock cb in _handlers) {
                    cg.BeginCatchBlock(cb.Test);

                    if (cb.Yield) {
                        // The catch block body contains yield, therefore
                        // delay the body emit till after the try block.
                        Slot slot = cg.GetLocalTmp(cb.Test);
                        slot.EmitSet(cg);
                        catches.Add(new CatchRecord(slot, cb));
                    } else {
                        // Save the exception (if the catch block asked for it) or pop
                        EmitSaveExceptionOrPop(cg, cb);
                        // Emit the body right now, since it doesn't contain yield
                        cb.Body.Emit(cg);
                    }
                }

                cg.PopTargets(TargetBlockType.Catch);
                cg.EndExceptionBlock();
                cg.PopTargets(TargetBlockType.Try);

                //******************************************************************
                // Emit the postponed catch block bodies (with yield in them)
                //******************************************************************
                foreach (CatchRecord cr in catches) {
                    Label next = cg.DefineLabel();
                    cr.Slot.EmitGet(cg);
                    cg.EmitNull();
                    cg.Emit(OpCodes.Beq, next);

                    if (cr.Block.Slot != null) {
                        cr.Block.Slot.EmitSet(cg, cr.Slot);
                    }

                    cg.FreeLocalTmp(cr.Slot);
                    cr.Block.Body.Emit(cg);
                    cg.MarkLabel(next);
                    //cg.EmitSequencePointNone();
                }
            }

            //******************************************************************
            // Emit the finally body
            //******************************************************************

            if (_finally != null) {
                cg.MarkLabel(endFinallyBlock);
                cg.PushExceptionBlock(TargetBlockType.Catch, flowControlFlag);
                cg.BeginCatchBlock(typeof(Exception));
                exception.EmitSet(cg);

                cg.PopTargets(TargetBlockType.Catch);

                cg.PushExceptionBlock(TargetBlockType.Finally, flowControlFlag);
                cg.BeginFinallyBlock();

                Label noExit = cg.DefineLabel();
                cg.GotoRouter.EmitGet(cg);
                cg.EmitInt(CodeGen.GotoRouterYielding);
                cg.Emit(OpCodes.Bne_Un_S, noExit);
                cg.Emit(OpCodes.Endfinally);
                cg.MarkLabel(noExit);

                EmitYieldDispatch(_finallyYields, cg);

                // Emit the finally body

                _finally.Emit(cg);

                // Rethrow the exception, if any

                Label noThrow = cg.DefineLabel();
                exception.EmitGet(cg);
                cg.EmitNull();
                cg.Emit(OpCodes.Beq, noThrow);
                exception.EmitGet(cg);
                cg.Emit(OpCodes.Throw);
                cg.MarkLabel(noThrow);
                cg.FreeLocalTmp(exception);

                cg.EndExceptionBlock();
                cg.PopTargets(TargetBlockType.Finally);
                cg.PopTargets(TargetBlockType.Try);

                //
                // Emit the flow control for finally, if there was any.
                //
                EmitFinallyFlowControl(cg, flow, flowControlFlag);

                //cg.EmitSequencePointNone();
            }

            // Clear the target labels

            ClearLabels(_tryYields);
            ClearLabels(_catchYields);

            if (_target != null) {
                _target.Clear();
            }
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
            if (flow.Return || flow.Yield) {
                Debug.Assert(flag != null);

                Label noReturn = cg.DefineLabel();

                flag.EmitGet(cg);
                cg.EmitInt(CodeGen.BranchForReturn);
                cg.Emit(OpCodes.Bne_Un, noReturn);

                if (cg.IsGenerator) {
                    // return true from the generator method
                    cg.Emit(OpCodes.Ldc_I4_1);
                    cg.EmitReturn();
                } else if (flow.Any) {
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

            //cg.EmitPosition(Start, _header);

            // If there's a yield anywhere, go for a complex codegen
            if (YieldInBlock(_tryYields) || _yieldInCatch || YieldInBlock(_finallyYields)) {
                EmitGeneratorTry(cg, flow);
            } else {
                EmitSimpleTry(cg, flow);
            }
        }

        private TargetLabel EnsureTopTarget() {
            if (_target == null) {
                _target = new TargetLabel();
            }
            return _target;
        }

        internal static void AddYieldTarget(ref List<YieldTarget> list, TargetLabel target, int index) {
            if (list == null) {
                list = new List<YieldTarget>();
            }
            list.Add(new YieldTarget(index, target));
        }

        internal static void ClearLabels(List<YieldTarget> targets) {
            if (targets != null) {
                foreach (YieldTarget yt in targets) {
                    yt.Clear();
                }
            }
        }

        internal static bool YieldInBlock(List<YieldTarget> block) {
            return block != null && block.Count > 0;
        }

        internal static void EmitYieldDispatch(List<YieldTarget> targets, CodeGen cg) {
            if (YieldInBlock(targets)) {
                Debug.Assert(cg.GotoRouter != null);

                // TODO: Emit as switch!
                foreach (YieldTarget yt in targets) {
                    cg.GotoRouter.EmitGet(cg);
                    cg.EmitInt(yt.Index);
                    cg.Emit(OpCodes.Beq, yt.EnsureLabel(cg));
                }
            }
        }

        internal TargetLabel AddTryYieldTarget(TargetLabel label, int index) {
            // Yield inside try stays inside try block, so we need to
            // remember the target label.

            AddYieldTarget(ref _tryYields, label, index);
            return EnsureTopTarget();
        }

        internal TargetLabel AddCatchYieldTarget(TargetLabel label, int index, int handler) {
            // Yields inside catch blocks are hoisted out of the catch.
            // If the catch block has a finally, though, it will get wrapped in
            // another try block, in which case the direct jump is not possible
            // and code must route through the top target.

            Debug.Assert(_handlers != null && handler < _handlers.Count);
            CatchBlock cb = _handlers[handler];

            cb.Yield = true;
            _yieldInCatch = true;

            if (_finally != null) {
                AddYieldTarget(ref _catchYields, label, index);
                return EnsureTopTarget();
            } else {
                return label;
            }
        }

        internal TargetLabel AddFinallyYieldTarget(TargetLabel label, int index) {
            // Yields inside finally stay inside finally so we need to keep track
            // of them.
            AddYieldTarget(ref _finallyYields, label, index);
            return EnsureTopTarget();
        }

        internal int GetGeneratorTempCount() {
            return _finally != null ? 1 : 0;
        }
    }
}
