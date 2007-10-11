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
using System.Reflection;
using System.Diagnostics;
using System.Collections.Generic;

namespace Microsoft.Scripting.Generation {

    /// <summary>
    /// Slot factory that indexes into a tuple.  The slot factory will generate a TupleDictionary 
    /// and a NewTuple object that backs it with the types generated from requests to CreateSlot.
    /// 
    /// All slots need to be created before accessing the TupleType property.
    /// </summary>
    public class TupleSlotFactory : SlotFactory, ILazySlotFactory<int> {
        private List<Slot> _slots = new List<Slot>();
        private List<SymbolId> _names = new List<SymbolId>();
        private Type _dictType, _tupleType;
        private Dictionary<CodeGen, List<Slot>> _concreteSlots;

        public TupleSlotFactory(Type dictType) {
            _dictType = dictType;
        }

        protected virtual Slot PrepareSlotForEmit(CodeGen cg) {
            // Emit globals from context and cast to tuple type            

            // tmpLocal = ((tupleDictType)codeContext.Scope.GlobalScope.GetDictionary(context)).Tuple
            cg.EmitCodeContext();
            cg.EmitPropertyGet(typeof(CodeContext), "Scope");
            cg.EmitPropertyGet(typeof(Scope), "ModuleScope");
            cg.EmitCall(typeof(RuntimeHelpers).GetMethod("GetTupleDictionaryData").MakeGenericMethod(TupleType));

            Slot tmpLocal = cg.GetLocalTmp(TupleType);
            tmpLocal.EmitSet(cg);

            return tmpLocal;
        }

        public override void PrepareForEmit(CodeGen cg) {
            if (_concreteSlots == null) _concreteSlots = new Dictionary<CodeGen, List<Slot>>();
            if (_concreteSlots.ContainsKey(cg)) return;

            Slot tmpLocal = PrepareSlotForEmit(cg);

            List<Slot> concreteSlots = new List<Slot>(_slots.Count);
            for (int i = 0; i < _slots.Count; i++ ) {
                concreteSlots.Add(CreateConcreteSlot(tmpLocal, i));
            }
            _concreteSlots[cg] = concreteSlots;
        }

        public Slot GetConcreteSlot(CodeGen cg, int data) {
            return _concreteSlots[cg][data];
        }

        protected override Slot CreateSlot(SymbolId name, Type type) {
            if (_tupleType != null) throw new InvalidOperationException("cannot add slots after tuple type has been determined");

            Slot res = new LazySlot<int>(this, type, _slots.Count);

            _slots.Add(res);
            _names.Add(name);

            return res;
        }

        internal Slot CreateConcreteSlot(Slot instance, int index) {
            Slot res = instance;
            foreach (PropertyInfo pi in Tuple.GetAccessPath(instance.Type, index)) {
                res = new PropertySlot(res, pi);
            }
            return res;
        }

        public Type TupleType {
            get {
                if (_tupleType != null) return _tupleType;

                _tupleType = Tuple.MakeTupleType(_slots.ConvertAll<Type>(delegate(Slot inp) { return inp.Type; }).ToArray());
                return _tupleType;
            }
        }

        public object CreateTupleInstance() {
            return Tuple.MakeTuple(TupleType, CompilerHelpers.MakeRepeatedArray<object>(null, _slots.Count));
        }

        public Type DictionaryType {
            get {
                return _dictType;
            }
        }

        public IList<SymbolId> Names {
            get {
                return _names.ToArray();
            }
        }
    }
}
