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
using System.Collections.Generic;

using Microsoft.Scripting;
using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Hosting;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting
{

#if FULL
    public interface IFunctionEnvironment {
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1819:PropertiesShouldNotReturnArrays")] // TODO: fix
        SymbolId[] Names {
            get;
        }
    } 
#endif


  /// <summary>
    /// Base class for FunctionEnvironment's which use a Tuple for the underlying storage.
    /// </summary>
    /// <typeparam name="TupleType"></typeparam>
  public sealed class FunctionEnvironmentDictionary<TupleType> : TupleDictionary<TupleType>
#if FULL
, IFunctionEnvironment 
#endif
 where TupleType : Tuple
  {
        private Dictionary<SymbolId, int> _slotDict;
        private int _size;

        public FunctionEnvironmentDictionary(TupleType data, SymbolId[] names) :
            base(data, names) {
        }

        protected internal override bool TrySetExtraValue(SymbolId key, object value) {
            if (_slotDict == null) MakeSlotDict();

            int val;
            if (_slotDict.TryGetValue(key, out val)) {
                if (_size < Tuple.MaxSize) {
                    // fast path
                    TupleData.SetValue(val, value);
                } else {
                    // slow path
                    object res = TupleData;
                    int lastAccess = -1;
                    foreach (int i in Tuple.GetAccessPath(_size, val)) {
                        if (lastAccess != -1) {
                            res = ((Tuple)res).GetValue(lastAccess);
                        }
                        lastAccess = i;
                    }
                    ((Tuple)res).SetValue(lastAccess, value);
                }
                return true;
            }

            return false;
        }

        protected internal override bool TryGetExtraValue(SymbolId key, out object value) {
            if (_slotDict == null) MakeSlotDict();

            int val;
            if (_slotDict.TryGetValue(key, out val)) {
                if (_size < Tuple.MaxSize) {
                    value = TupleData.GetValue(val);
                } else {
                    object res = TupleData;
                    foreach (int i in Tuple.GetAccessPath(_size, val)) {
                        res = ((Tuple)res).GetValue(i);
                    }
                    value = res;
                }                
                return true;
            }
            value = null;
            return false;
        }

        private void MakeSlotDict() {
            _size = Tuple.GetSize(TupleData.GetType());

            Dictionary<SymbolId, int> slotDict = new Dictionary<SymbolId, int>();
            for (int index = 0; index < Extra.Length; index++) {
                slotDict[Extra[index]] = index + 1;
            }
            _slotDict = slotDict;
        }


#if FULL
        SymbolId[] IFunctionEnvironment.Names {
            get {
                return Extra;
            }
        } 
#endif

  }
}
