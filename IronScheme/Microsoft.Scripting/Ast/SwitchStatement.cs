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
using System.Reflection;
using System.Collections;
using System.Diagnostics;
using System.Reflection.Emit;
using System.Collections.Generic;
using System.Collections.ObjectModel;

using Microsoft.Scripting.Utils;
using Microsoft.Scripting.Generation;

namespace Microsoft.Scripting.Ast {
    public class SwitchStatement : Statement {
        private readonly SourceLocation _header;
        private readonly Expression _testValue;
        private readonly ReadOnlyCollection<SwitchCase> _cases;

        private const int MaxJumpTableSize = 65536;
        private const double MaxJumpTableSparsity = 10;

        internal SwitchStatement(SourceSpan span, SourceLocation header, Expression/*!*/ testValue, ReadOnlyCollection<SwitchCase>/*!*/ cases)
            : base(AstNodeType.SwitchStatement, span) {
            Assert.NotNullItems(cases);

            _testValue = testValue;
            _cases = cases;
            _header = header;
        }

        public Expression TestValue {
            get { return _testValue; }
        }

        public ReadOnlyCollection<SwitchCase> Cases {
            get { return _cases; }
        }

        public SourceLocation Header {
            get { return _header; }
        }

        public override void Emit(CodeGen cg) {
            cg.EmitPosition(Start, _header);

            Label breakTarget = cg.DefineLabel();
            Label defaultTarget = breakTarget;
            Label[] labels = new Label[_cases.Count];

            // Create all labels
            for (int i = 0; i < _cases.Count; i++) {
                labels[i] = cg.DefineLabel();

                // Default case.
                if (_cases[i].IsDefault) {
                    // Set the default target
                    defaultTarget = labels[i];
                }
            }

            // Emit the test value
            _testValue.Emit(cg);

            // Check if jmp table can be emitted
            if (!TryEmitJumpTable(cg, labels, defaultTarget)) {
                // There might be scenario(s) where the jmp table is not emitted
                // Emit the switch as conditional branches then
                EmitConditionalBranches(cg, labels);
            }

            // If "default" present, execute default code, else exit the switch            
            cg.Emit(OpCodes.Br, defaultTarget);

            cg.PushTargets(breakTarget, cg.BlockContinueLabel, this);

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
        private void EmitConditionalBranches(CodeGen cg, Label[] labels) {
            Slot testValueSlot = cg.GetNamedLocal(typeof(int), "switchTestValue");
            testValueSlot.EmitSet(cg);

            // For all the "cases" create their conditional branches
            for (int i = 0; i < _cases.Count; i++) {
                // Not default case emit the condition
                if (!_cases[i].IsDefault) {
                    // Test for equality of case value and the test expression
                    cg.EmitInt(_cases[i].Value);
                    testValueSlot.EmitGet(cg);
                    cg.Emit(OpCodes.Beq, labels[i]);
                }
            }
        }

        // Tries to emit switch as a jmp table
        private bool TryEmitJumpTable(CodeGen cg, Label[] labels, Label defaultTarget) {
            if (_cases.Count > MaxJumpTableSize) {
                return false;
            }

            int min = Int32.MaxValue;
            int max = Int32.MinValue;

            // Find the min and max of the values
            for (int i = 0; i < _cases.Count; ++i) {
                // Not the default case.
                if (!_cases[i].IsDefault) {
                    int val = _cases[i].Value;
                    if (min > val) min = val;
                    if (max < val) max = val;
                }
            }

            long delta = (long)max - (long)min;
            if (delta > MaxJumpTableSize) {
                return false;
            }

            // Value distribution is too sparse, don't emit jump table.
            if (delta > _cases.Count + MaxJumpTableSparsity) {
                return false;
            }

            // The actual jmp table of switch
            int len = (int)delta + 1;
            Label[] jmpLabels = new Label[len];

            // Initialize all labels to the default
            for (int i = 0; i < len; i++) {
                jmpLabels[i] = defaultTarget;
            }

            // Replace with the actual label target for all cases
            for (int i = 0; i < _cases.Count; i++) {
                SwitchCase sc = _cases[i];
                if (!sc.IsDefault) {
                    jmpLabels[sc.Value - min] = labels[i];
                }
            }

            // Emit the normalized index and then switch based on that
            if (min != 0) {
                cg.EmitInt(min);
                cg.Emit(OpCodes.Sub);
            }
            cg.Emit(OpCodes.Switch, jmpLabels);
            return true;
        }


#if FULL
        protected override object DoExecute(CodeContext context) {
            throw new NotImplementedException();
        } 
#endif

    }

    /// <summary>
    /// Factory methods.
    /// </summary>
    public static partial class Ast {
        public static SwitchStatement Switch(SourceSpan span, SourceLocation header, Expression value, params SwitchCase[] cases) {
            Contract.RequiresNotNull(value, "value");
            Contract.Requires(value.Type == typeof(int), "value", "Value must be int");
            Contract.RequiresNotEmpty(cases, "cases");
            Contract.RequiresNotNullItems(cases, "cases");

            bool @default = false;
            int max = Int32.MinValue;
            int min = Int32.MaxValue;
            foreach (SwitchCase sc in cases) {
                if (sc.IsDefault) {
                    Contract.Requires(@default == false, "cases", "Only one default clause allowed");
                    @default = true;
                } else {
                    int val = sc.Value;
                    if (val > max) max = val;
                    if (val < min) min = val;
                }
            }

            Contract.Requires(UniqueCaseValues(cases, min, max), "cases", "Case values must be unique");

            return new SwitchStatement(span, header, value, CollectionUtils.ToReadOnlyCollection(cases));
        }

        // Below his threshold we'll use brute force N^2 algorithm
        private const int N2Threshold = 10;

        // If values are in a small range, we'll use bit array
        private const long BitArrayThreshold = 1024;

        private static bool UniqueCaseValues(SwitchCase[] cases, int min, int max) {
            int length = cases.Length;

            // If we have small number of cases, use straightforward N2 algorithm
            // which doesn't allocate memory
            if (length < N2Threshold) {
                for (int i = 0; i < length; i++) {
                    SwitchCase sci = cases[i];
                    if (sci.IsDefault) {
                        continue;
                    }
                    for (int j = i + 1; j < length; j++) {
                        SwitchCase scj = cases[j];
                        if (scj.IsDefault) {
                            continue;
                        }

                        if (sci.Value == scj.Value) {
                            // Duplicate value found
                            return false;
                        }
                    }
                }

                return true;
            }

            // We have at least N2Threshold items so the min and max values
            // are set to actual values and not the Int32.MaxValue and Int32.MaxValue
            Debug.Assert(min <= max);
            long delta = (long)max - (long)min;
            if (delta < BitArrayThreshold) {
                BitArray ba = new BitArray((int)delta + 1, false);

                for (int i = 0; i < length; i++) {
                    SwitchCase sc = cases[i];
                    if (sc.IsDefault) {
                        continue;
                    }
                    // normalize to 0 .. (max - min)
                    int val = sc.Value - min;
                    if (ba.Get(val)) {
                        // Duplicate value found
                        return false;
                    }
                    ba.Set(val, true);
                }

                return true;
            }

            // Too many values that are too spread around. Use dictionary
            // Using Dictionary<int, object> as it is used elsewhere to
            // minimize the impact of generic instantiation
            Dictionary<int, object> dict = new Dictionary<int, object>(length);
            for (int i = 0; i < length; i++) {
                SwitchCase sc = cases[i];
                if (sc.IsDefault) {
                    continue;
                }
                int val = sc.Value;
                if (dict.ContainsKey(val)) {
                    // Duplicate value found
                    return false;
                }
                dict[val] = null;
            }

            return true;
        }
    }
}
