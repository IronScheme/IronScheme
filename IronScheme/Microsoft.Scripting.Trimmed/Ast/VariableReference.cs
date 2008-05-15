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
using Microsoft.Scripting.Generation;

namespace Microsoft.Scripting.Ast {
    /// <summary>
    /// Reference is representation of the use(s) of the variable (_variable).
    /// It is used to resolve closures and create actual slots for the code generation using
    /// the information stored in the Binding.
    /// </summary>
    internal class VariableReference {
        private readonly Variable _variable;
        private Slot _slot;

        public VariableReference(Variable variable) {
            Debug.Assert(variable != null);
            _variable = variable;
        }

        /// <summary>
        /// The referenced variable. For references dynamically resolved at runtime,
        /// the variable is null.
        /// </summary>
        public Variable Variable {
            get { return _variable; }
        }

        public Slot Slot {
            get { return _slot; }
        }

        public void CreateSlot(CodeGen cg) {
            _slot = _variable.CreateSlot(cg);
        }

      public override bool Equals(object obj)
      {
        VariableReference b = obj as VariableReference;
        return b.Variable.Name == Variable.Name && b.Variable.Block == Variable.Block;
      }

      public override int GetHashCode()
      {
        return base.GetHashCode();
      }
    }
}
