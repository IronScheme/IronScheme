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
using System.Reflection.Emit;

using System.Diagnostics;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Generation {
    /// <summary>
    /// Slot refers to a reference to an object. For eg, a global variable, a local variable, etc.
    /// A Slot is referred to using a Name. The Namespace is used to map a Name to a Slot.
    /// Multiple Names can refer to the same Slot.
    /// For eg. multiple closures can refer to the same Slot of a local variable in the enclosing
    /// function. Though each closure will use the same string (the name of the variable), each
    /// string is considered a unique Name or symbol.
    /// </summary>
    public abstract class Slot {
        private bool _local;
        private Type _knownType;
        public abstract void EmitGet(CodeGen cg);
        public abstract void EmitGetAddr(CodeGen cg);

        // Must override at least one of these two methods or get infinite loop
        public virtual void EmitSet(CodeGen cg, Slot val) {
            Contract.RequiresNotNull(val, "val");
            Contract.RequiresNotNull(cg, "cg");

            val.EmitGet(cg);
            EmitSet(cg);
        }

        // This override assumes that the IL stack already holds the value to be assigned from.
        public virtual void EmitSet(CodeGen cg) {
            Contract.RequiresNotNull(cg, "cg");

            // localTmpVal = <top of IL stack>
            Slot localTmpVal = cg.GetLocalTmp(typeof(object));
            localTmpVal.EmitSet(cg);

            // <slot> = localTmpVal
            EmitSet(cg, localTmpVal);

            cg.FreeLocalTmp(localTmpVal);
        }

        // Slots are to be implemented as dictionaries for Python. However,
        // for performance and better integration with the CLI, the engine
        // implements many Slot types as CLI entities, which cannot be
        // deleted once they are created. Hence, to implement "del",
        // an alternate scheme is used. We just assign Uninitialized.instance
        // to represent that the Slot has been deleted.
        // Any access to the Slot first checks if it is holding Uninitialized.instance,
        // which means that it should virtually not exist

        public virtual void EmitSetUninitialized(CodeGen cg) {
            Contract.RequiresNotNull(cg, "cg");

            // Emit the following:
            //     <name> = Uninitialized.instance;

            Debug.Assert(Type == typeof(object));

            cg.EmitUninitialized();
            EmitSet(cg);
        }

        public virtual void EmitDelete(CodeGen cg, SymbolId name, bool check) {
            Contract.RequiresNotNull(cg, "cg");

            // First check that the Name exists. Otherwise, deleting it
            // should cause a NameError
            if (check /*&& Options.CheckInitialized*/) {
                EmitGet(cg);
                EmitCheck(cg, name);
                cg.Emit(OpCodes.Pop);
            }

            EmitSetUninitialized(cg);
        }

        public virtual void EmitCheck(CodeGen cg, SymbolId name) {
            Contract.RequiresNotNull(cg, "cg");

            Label endCheck = cg.DefineLabel();
            cg.Emit(OpCodes.Dup);
            cg.EmitUninitialized();
            cg.Emit(OpCodes.Bne_Un_S, endCheck);
            if (_local) {
                cg.EmitSymbolId(name);
                cg.EmitUnbox(typeof(SymbolId));
                cg.EmitCall(typeof(RuntimeHelpers), "ThrowUnboundLocalError");
            } else {
                cg.Emit(OpCodes.Pop);
                cg.EmitCodeContext();
                cg.EmitSymbolId(name);
                cg.EmitUnbox(typeof(SymbolId));
                cg.EmitCall(typeof(RuntimeHelpers), "LookupName");
            }
            cg.MarkLabel(endCheck);
        }

        public void EmitGetAs(CodeGen cg, Type asType) {
            Contract.RequiresNotNull(cg, "cg");
            Contract.RequiresNotNull(asType, "asType");

            EmitGet(cg);
            if (asType == typeof(object) && this.Type.IsValueType) {
                cg.EmitBoxing(this.Type);
                return;
            }

            if (asType.IsAssignableFrom(this.Type)) {
                return;
            }

            if (asType.IsAssignableFrom(_knownType)) {
                cg.EmitUnbox(asType);
            } else {
                // A special case for int-> double until we can do the proper matrix
                if (asType == typeof(double)) {
                    if (this.Type == typeof(int)) {
                        cg.Emit(OpCodes.Conv_R8);
                        return;
                    } else if (this.KnownType == typeof(int)) {
                        cg.EmitUnbox(typeof(int));
                        cg.Emit(OpCodes.Conv_R8);
                        return;
                    }
                }


                if (this.Type != typeof(object)) {
                    // TODO make this efficient, for now just go to object and back
                    //throw new InvalidOperationException();
                    cg.EmitBoxing(this.Type);
                }
                cg.EmitConvertFromObject(asType);
            }
        }


        public abstract Type Type { get; }

        /// <summary>
        /// True if the slot represents a local variable
        /// </summary>
        public bool Local {
            get { return _local; }
            set { _local = value; }
        }

        public Type KnownType {
            get { return _knownType; }
            set { _knownType = value; }
        }

        public override string ToString() {
            return String.Format("{0} Type: {1}", GetType().Name, Type.FullName);
        }
    }
}
