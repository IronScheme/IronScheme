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

      public static Dictionary<SymbolId, Emitter> Fixups
      {
        get { return BoundExpression.fixups; }
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

#if FULL

        protected override object DoEvaluate(CodeContext context) {
            object ret;
            switch (_variable.Kind) {
                case Variable.VariableKind.Temporary:
                case Variable.VariableKind.GeneratorTemporary:
                    if (!context.Scope.TemporaryStorage.TryGetValue(_variable, out ret)) {
                        throw context.LanguageContext.MissingName(_variable.Name);
                    } else {
                        return ret;
                    }
                case Variable.VariableKind.Parameter:
                    // This is sort of ugly: parameter variables can be stored either as locals or as temporaries (in case of $argn).
                    if (!context.Scope.TemporaryStorage.TryGetValue(_variable, out ret) || ret == Uninitialized.Instance) {
                        return RuntimeHelpers.LookupName(context, _variable.Name);
                    } else {
                        return ret;
                    }
                case Variable.VariableKind.Global:
                    return RuntimeHelpers.LookupGlobalName(context, _variable.Name);
                default:
                    if (!context.LanguageContext.TryLookupName(context, _variable.Name, out ret)) {
                        throw context.LanguageContext.MissingName(_variable.Name);
                    } else if (ret == Uninitialized.Instance) {
                        RuntimeHelpers.ThrowUnboundLocalError(_variable.Name);
                        return null;
                    } else {
                        return ret;
                    }
            }
        } 
#endif



#if FULL
        class VariableAddress : EvaluationAddress {
            public VariableAddress(Expression expr)
                : base(expr) {
            }

            public override object GetValue(CodeContext context, bool outParam) {
                if (outParam) {
                    return null;
                }

                return base.GetValue(context, outParam);
            }
        } 
#endif



#if FULL
        internal override EvaluationAddress EvaluateAddress(CodeContext context) {
            return new VariableAddress(this);
        } 
#endif



#if FULL
        internal override object EvaluateAssign(CodeContext context, object value) {
            return BoundAssignment.EvaluateAssign(context, Variable, value);
        } 
#endif



#if FULL
        public override AbstractValue AbstractEvaluate(AbstractContext context) {
            return context.Lookup(_variable);
        } 
#endif


        internal override void EmitAddress(CodeGen cg, Type asType) {
            if (asType == Type) {
                _vr.Slot.EmitGetAddr(cg);
            } else {
                base.EmitAddress(cg, asType);
            }
        }

        public override void Emit(CodeGen cg) {
          EmitLocation(cg);
            // Do not emit CheckInitialized for variables that are defined, or for temp variables.
            // Only emit CheckInitialized for variables of type object
            bool check = _variable.Uninitialized;// !_defined && !_variable.IsTemporary && _variable.Type == typeof(object);
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
