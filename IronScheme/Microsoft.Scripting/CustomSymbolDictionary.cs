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
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using Microsoft.Scripting.Utils;


namespace Microsoft.Scripting {

    /// <summary>
    /// Abstract base class used for optimized thread-safe SymbolDictionaries. 
    /// 
    /// Implementers derive from this class and override the GetExtraKeys, TrySetExtraValue, 
    /// and TryGetExtraValue methods. When looking up a value first the extra keys will be 
    /// searched using the optimized Try*ExtraValue functions.  If the value isn't found there
    /// then the value is stored in the underlying .NET dictionary.
    /// 
    /// Implementors can optionally override the object key functionality to store object keys
    /// using their own mechanism.  By default object keys are stored in their own dictionary
    /// which is stored in the primary SymbolId dictionary under an invalid symbol id.
    /// </summary>
    public abstract class CustomSymbolDictionary : BaseSymbolDictionary, IAttributesCollection {
        private Dictionary<SymbolId, object> _data;

        protected CustomSymbolDictionary() {
        }

        private void InitializeData() {
            Debug.Assert(_data == null);

            _data = new Dictionary<SymbolId, object>();
        }

        #region IEnumerable<KeyValuePair<object,object>> Members

        IEnumerator<KeyValuePair<object, object>> IEnumerable<KeyValuePair<object, object>>.GetEnumerator() {
            if (_data != null) {
                foreach (KeyValuePair<SymbolId, object> o in _data) {
                    if (o.Key == SymbolId.Invalid) break;
                    yield return new KeyValuePair<object, object>(SymbolTable.IdToString(o.Key), o.Value);
                }
            }
        }

        #endregion

        #region IEnumerable Members

        public System.Collections.IEnumerator GetEnumerator() {
            return ((IEnumerable<KeyValuePair<object, object>>) this).GetEnumerator();
        }

        #endregion

        #region IAttributesDictionary Members


        public bool Remove(SymbolId name) {
            lock (this) return _data.Remove(name);
        }

        public bool TryGetValue(SymbolId name, out object value) {
          value = null;
            if (_data == null) return false;

            lock (this) return _data.TryGetValue(name, out value);
        }

        object IAttributesCollection.this[SymbolId name] {
            get {
                lock (this) {
                    if (_data == null) throw new KeyNotFoundException(SymbolTable.IdToString(name));
                    return _data[name];
                }
            }
            set {
                lock (this) {
                    if (_data == null) InitializeData();
                    _data[name] = value;
                }
            }
        }

        public IEnumerable<SymbolId> Keys { get { return _data.Keys; } }

        #endregion

        public override bool IsSynchronized {
            get {
                return true;
            }
        }

        public override object SyncRoot {
            get {
                // TODO: Sync root shouldn't be this, it should be data.
                return this;
            }
        }
    }
}
