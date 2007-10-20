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
using System.Collections.Generic;
using System.Reflection.Emit;
using System.Diagnostics;

using Microsoft.Scripting.Generation;
using System.Reflection;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Ast {
    public class SwitchStatement : Statement {
        private readonly SourceLocation _header;
        private readonly Expression _testValue;

        private readonly MethodInfo _tryGetSwitchIndexMethod;
        private readonly MethodInfo _equalMethod;

        // TODO: Make SwitchCase[]
        private readonly List<SwitchCase> _cases;

        private const int JMPTABLE_SPARSITY = 10;

        internal SwitchStatement(SourceSpan span, SourceLocation header, Expression testValue, List<SwitchCase> cases, 
            MethodInfo tryGetSwitchIndexMethod, MethodInfo equalMethod)
            : base(span) {
            Assert.NotNullItems(cases);
            Assert.NotNull(tryGetSwitchIndexMethod, equalMethod);
            
            _testValue = testValue;
            _cases = cases;
            _header = header;
            _tryGetSwitchIndexMethod = tryGetSwitchIndexMethod;
            _equalMethod = equalMethod;
        }

        public Expression TestValue {
            get { return _testValue; }
        }

        public List<SwitchCase> Cases {
            get { return _cases; }
        }

        public SourceLocation Header {
            get { return _header; }
        }

        public override void Emit(CodeGen cg) {
            Label breakTarget = cg.DefineLabel();
            Nullable<Label> continueTarget = cg.BlockContinueLabel;

            cg.EmitPosition(Start, _header);

            int defaultCaseLocation = -1;
            Label defaultTarget = breakTarget;
            Label[] labels = new Label[_cases.Count];

            // For all the "cases" create the labels
            for (int i = 0; i < _cases.Count; i++) {
                labels[i] = cg.DefineLabel();

                // Default case.
                if (_cases[i].Value == null) {

                    // Store the default case's location
                    defaultCaseLocation = i;

                    // Set the default target
                    defaultTarget = labels[defaultCaseLocation];
                    continue;
                }
            }

            // Emit the test value
            _testValue.EmitAsObject(cg);

            // Check if jmp table can be emitted
            bool emitJumpTable = TryEmitJumpTable(cg, labels, defaultCaseLocation, defaultTarget);

            // There might be scenario(s) where the jmp table is not emitted
            // Emit the switch as conditional branches then
            if (!emitJumpTable) {
                EmitConditionalBranches(cg, labels, defaultCaseLocation);
            }

            // If "default" present, execute default code, else exit the switch            
            cg.Emit(OpCodes.Br, defaultTarget);

            cg.PushTargets(breakTarget, continueTarget, this);

            // Emit the bodies
            for (int i = 0; i < _cases.Count; i++) {

                // First put the corresponding labels
                cg.MarkLabel(labels[i]);

                // And then emit the Body!!
                _cases[i].Body.Emit(cg);
            }

            cg.PopTargets();
            cg.MarkLabel(breakTarget);
        }

        // Emits the switch as if stmts
        private void EmitConditionalBranches(CodeGen cg, Label[] labels, int defaultCaseLocation) {

            Slot testValueSlot = cg.GetNamedLocal(typeof(object), "switchTestValue");
            testValueSlot.EmitSet(cg);

            // For all the "cases" create their conditional branches
            for (int i = 0; i < _cases.Count; i++) {

                // Not default case emit the condition
                if (i != defaultCaseLocation) {

                    // Test for equality of case value and the test expression
                    cg.EmitCodeContext(); 
                    _cases[i].Value.EmitAsObject(cg);
                    testValueSlot.EmitGet(cg);
                    cg.EmitCall(_equalMethod);
                    cg.Emit(OpCodes.Brtrue, labels[i]);
                }
            }
        }

        // Tries to emit switch as a jmp table
        private bool TryEmitJumpTable(CodeGen cg, Label[] labels, int defaultCaseLocation, Label defaultTarget) {

            int[] values = new int[_cases.Count];
            int min = Int32.MaxValue;
            int max = Int32.MinValue;

            // Check if the values are int/double. Also get the min and max of the values
            for (int i = 0; i < _cases.Count; ++i) {

                // Not the default case.
                if (i != defaultCaseLocation) {

                    // Get the case value
                    if (!GetExpressionValue(_cases[i].Value, ref values[i]))
                        return false;

                    if (min > values[i]) min = values[i];
                    if (max < values[i]) max = values[i];
                }
            }

            // Too sparse case value distribution. Don't emit jmp table
            if ((max - min - _cases.Count) > JMPTABLE_SPARSITY) return false;

            // The actual jmp table of switch
            int len = max - min + 1;
            Label[] jmpLabels = new Label[len];

            // Put the default target for all labels initially
            for (int i = 0; i < len; ++i) {
                jmpLabels[i] = defaultTarget;
            }

            // Replace with the actual label target for all cases
            for (int i = 0; i < _cases.Count; ++i) {
                if (i != defaultCaseLocation) {
                    // normalise the values to min
                    // and first case value takes precedence in case of same case values
                    if (jmpLabels[values[i] - min] == defaultTarget)
                        jmpLabels[values[i] - min] = labels[i];
                }
            }

            Slot testValueSlot = cg.GetNamedLocal(typeof(object), "switchTestValue");
            testValueSlot.EmitSet(cg);

            // Call TryGetSwitchIndex(codeContext, testValue, out index)
            cg.EmitCodeContext();
            testValueSlot.EmitGet(cg);

            Slot indexSlot = cg.GetLocalTmp(typeof(int));
            indexSlot.EmitGetAddr(cg);

            // Call the helper method to determine if the value is int/double and get it's range
            cg.EmitCall(_tryGetSwitchIndexMethod);

            // If the helper returns false (switch val not int/double) goto the default target
            cg.Emit(OpCodes.Brfalse, defaultTarget);

            // Emit the normalized index and then switch based on that
            indexSlot.EmitGet(cg);
            cg.EmitInt(min);
            cg.Emit(OpCodes.Sub); 

            cg.Emit(OpCodes.Switch, jmpLabels);

            return true;
        }

        private bool GetExpressionValue(Expression expr, ref int val) {
            bool ret = false;
            ConstantExpression cexpr = expr as ConstantExpression;
            if (cexpr != null) {

                // Value is int
                if (cexpr.Value is int) {
                    val = (int)cexpr.Value;

                    ret = true;
                } else if (cexpr.Value is double) {
                    double dvalue = (double)cexpr.Value;

                    val = Convert.ToInt32(dvalue);

                    // Handles the Num.0 case
                    if (val == dvalue) ret = true;
                }

                // Ignore too large numbers to avoid overflows
                if (val > 1000000000) ret = false;
            }
            // TODO: After switch is cleaned up, this will not be needed anyway.
            //else if (expr is UnaryExpression) {
            //    UnaryExpression uexpr = expr as UnaryExpression;

            //    ret = GetExpressionValue(uexpr.Expression, ref val);

            //    if (ret) {
            //        // Alow the unary minus and unary plus
            //        if (uexpr.Operator == UnaryOperators.Negate) {
            //            val = -val;
            //        } else if (uexpr.Operator != UnaryOperators.Pos) {
            //            ret = false;
            //        }
            //    }
            //}

            return ret;
        }

        protected override object DoExecute(CodeContext context) {
            throw new NotImplementedException();
        }

        public override void Walk(Walker walker) {
            if (walker.Walk(this)) {
                _testValue.Walk(walker);
                List<SwitchCase>.Enumerator cases = _cases.GetEnumerator();
                while (cases.MoveNext()) {
                    SwitchCase currentCase = cases.Current;
                    if (currentCase.Value != null) {
                        currentCase.Value.Walk(walker);
                    }
                    currentCase.Body.Walk(walker);
                }
            }
            walker.PostWalk(this);
        }
    }

    /// <summary>
    /// Factory methods.
    /// </summary>
    public static partial class Ast {
        public static SwitchStatement Switch(SourceSpan span, SourceLocation header, Expression testValue, List<SwitchCase> cases, 
            MethodInfo tryGetSwitchIndexMethod, MethodInfo equalMethod) {

            Debug.Assert(ReflectionUtils.SignatureEquals(tryGetSwitchIndexMethod, typeof(CodeContext), typeof(object), typeof(int).MakeByRefType(), typeof(bool)));
            Debug.Assert(ReflectionUtils.SignatureEquals(equalMethod, typeof(CodeContext), typeof(object), typeof(object), typeof(bool)));

            return new SwitchStatement(span, header, testValue, cases, tryGetSwitchIndexMethod, equalMethod);
        }
    }
}