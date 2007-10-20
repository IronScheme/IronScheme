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
using System.Text;
using Microsoft.Scripting;

namespace Microsoft.Scripting {
    /// <summary>
    /// A dicitonary backed by a generic tuple of type TupleType.  
    /// 
    /// Implements a type safe SymbolId dictionary that's backed by fields in a class.  Each name
    /// of the dictionary can be strongly typed to its own type.  When constructed with an array
    /// of SymbolId's the names have a 1 to 1 mapping with the Item's in the tuple (eg tuple.Item0
    /// maps to names[0]).
    /// 
    /// The TupleDictionary has no maximum size.  If more than NewTuple.MaxSize members are needed then nested 
    /// tuples will be used.  Nested tuples have their values stored in the leaf nodes of size NewTuple.MaxSize.
    /// The leaf nodes must all be at the same depth.  Only nodes which are outside the total size can be
    /// smaller than NewTuple.MaxSize.  When nesting more than 2 levels deep then the intermediary nodes have the
    /// same requirements as the leaf nodes (size 128 unless outside the size).
    /// </summary>
    public class TupleDictionary<TupleType> : CustomSymbolDictionary where TupleType : Tuple {
        private SymbolId[] _extra;       // extra keys
        private TupleType _data;

        /// <summary>
        /// Creates a new tuple dictionary with the provided TupleType data object providing the storage
        /// for names.  The indexes of the names correspond with the Item### fields of the tuple.
        /// 
        /// In the case of nested tuples the names correspond with the Item### in the order of the leaf nodes.
        /// See class information for details on nested tuples.
        /// </summary>
        public TupleDictionary(TupleType data, SymbolId[] names) {
            _data = data;
            _extra = names;
        }

        /// <summary>
        /// Creates a new tuple dictionary with the provided TupleType data object providing the storage.
        /// 
        /// The Extra property must be set later to set the names associated with the data.
        /// </summary>
        protected TupleDictionary(TupleType data) {
            _data = data;
        }

        public override SymbolId[] GetExtraKeys() {
            return _extra;
        }

        protected internal override bool TryGetExtraValue(SymbolId key, out object value) {
            for (int i = 0; i < _extra.Length; i++) {
                if (_extra[i] == key) {
                    value = GetValue(i);
                    return true;
                }
            }
            value = null;
            return false;
        }

        protected internal override bool TrySetExtraValue(SymbolId key, object value) {
            for (int i = 0; i < _extra.Length; i++) {
                if (_extra[i] == key) {
                    SetValue(i, value);
                    return true;
                }
            }
            return false;
        }

        private object GetValue(int index) {
            if (_extra.Length <= Tuple.MaxSize) return _data.GetValue(index);

            // nested tuples
            int depth = 0;
            int mask = Tuple.MaxSize - 1;
            int adjust = 1;
            int count = _extra.Length;
            while (count > Tuple.MaxSize) {
                depth++;
                count /= Tuple.MaxSize;
                mask *= Tuple.MaxSize;
                adjust *= Tuple.MaxSize;
            }

            object next = _data;
            while (depth-- >= 0) {
                int curIndex = (index & mask) / adjust;
                next = ((Tuple)next).GetValue(curIndex);

                mask /= Tuple.MaxSize;
                adjust /= Tuple.MaxSize;
            }

            return next;
        }

        private void SetValue(int index, object value) {
            if (_extra.Length <= Tuple.MaxSize) { 
                _data.SetValue(index, value); 
                return; 
            }

            // nested tuples
            int depth = 0;
            int mask = Tuple.MaxSize - 1;
            int adjust = 1;
            int count = _extra.Length;
            while (count > Tuple.MaxSize) {
                depth++;
                count /= Tuple.MaxSize;
                mask *= Tuple.MaxSize;
                adjust *= Tuple.MaxSize;
            }

            Tuple next = _data;
            while (depth-- >= 0) {
                int curIndex = (index & mask) / adjust;
                if (depth >= 0) {
                    next = (Tuple)next.GetValue(curIndex);
                } else {
                    next.SetValue(curIndex, value);
                }

                mask /= Tuple.MaxSize;
                adjust /= Tuple.MaxSize;
            }
        }

        /// <summary>
        /// Gets the Tuple data being used to back the dicionary.
        /// </summary>
        public TupleType TupleData {
            get {
                return _data;
            }
            set {
                _data = value;
            }
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1819:PropertiesShouldNotReturnArrays")] // TODO: fix
        protected SymbolId[] Extra {
            get { return _extra; }
            set { _extra = value; }
        }
    }


}
