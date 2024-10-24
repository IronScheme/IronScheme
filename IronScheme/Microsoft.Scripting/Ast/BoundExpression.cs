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
using Microsoft.Scripting.Utils;
using System.Collections.Generic;

namespace Microsoft.Scripting.Ast {
    public class BoundExpression : Expression {
        private Variable /*!*/ _variable;
        private bool _defined;

      public delegate void Emitter(CodeGen cg, bool tailcall);

      readonly static Dictionary<SymbolId, Emitter> fixups = new Dictionary<SymbolId, Emitter>();
      readonly static Dictionary<SymbolId, Type[]> fixuptypes = new Dictionary<SymbolId, Type[]>();

      public static Dictionary<SymbolId, Type[]> FixupTypes
      {
        get { return fixuptypes; }
      } 


      public static Dictionary<SymbolId, Emitter> Fixups
      {
        get { return fixups; }
      } 
 

        // Implementation detail
        private VariableReference _vr;

        internal BoundExpression(Variable /*!*/ variable)
            : base(AstNodeType.BoundExpression) {
            _variable = variable;
        }

        public Variable Variable {
            get { return _variable; }
          set 
          { 
            _variable = value;
            _vr = null;
          }
        }

        internal VariableReference Ref {
            get { return _vr; }
            set {
                Debug.Assert(value.Variable == _variable);
                // the _vr == value is true for DAGs
                Debug.Assert(_vr == null || _vr.Equals(value));
                _vr = value;
            }
        }

        public SymbolId Name {
            get { return _variable.Name; }
        }

        public bool IsDefined {
            get { return _defined; }
            internal set { _defined = value; }
        }

        public override Type Type {
            get { return _variable.Type; }
        }

        public override string ToString() {
            return "BoundExpression : " + SymbolTable.IdToString(Name);
        }

        internal override void EmitAddress(CodeGen cg, Type asType) {
            if (asType == Type) {
                _vr.Slot.EmitGetAddr(cg);
            } else {
                base.EmitAddress(cg, asType);
            }
        }

        public override void Emit(CodeGen cg) {
            bool check = _variable.Uninitialized;
            cg.EmitGet(_vr.Slot, Name, check);
        }
    }

    public static partial class Ast {
        public static BoundExpression Read(Variable variable) {
            Contract.RequiresNotNull(variable, "variable");
            return new BoundExpression(variable);
        }

        public static BoundExpression ReadDefined(Variable variable) {
            BoundExpression ret = Read(variable);
            ret.IsDefined = true;
            return ret;
        }
    }
}
