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
// TODO: Remove this
using System.Diagnostics;
using System.Reflection.Emit;
using Microsoft.Scripting.Generation;

namespace Microsoft.Scripting.Ast {
    internal class TargetLabel {
        private Label _label;
        private bool _initialized;

        internal TargetLabel() {
        }

        internal Label EnsureLabel(CodeGen cg) {
            if (!_initialized) {
                _label = cg.DefineLabel();
                _initialized = true;
            }
            return _label;
        }

        internal void Clear() {
            _initialized = false;
        }
    }

    internal struct YieldTarget {
        private readonly int _index;
        private readonly TargetLabel _target;

        public YieldTarget(int index, TargetLabel label) {
            _index = index;
            _target = label;
        }

        public int Index {
            get { return _index; }
        }

        public TargetLabel Target {
            get { return _target; }
        }

        public Label EnsureLabel(CodeGen cg) {
            Debug.Assert(_target != null);
            return _target.EnsureLabel(cg);
        }

        internal void Clear() {
            if (_target != null) {
                _target.Clear();
            }
        }
    }
}
