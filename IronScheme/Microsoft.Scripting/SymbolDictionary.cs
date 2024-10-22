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

namespace Microsoft.Scripting
{
    /// <summary>
    /// Simple thread-safe SymbolDictionary used for storing collections of members.
    /// 
    /// Like all SymbolDictionaries this supports both indexing using SymbolId's (IAttributesCollection)
    /// and via object keys (IDictionary&lt;object, object&gt;).
    /// </summary>
    public sealed class SymbolDictionary : BaseSymbolDictionary, IAttributesCollection {
        private Dictionary<SymbolId, object> _data = new Dictionary<SymbolId, object>();

        public SymbolDictionary() {
        }

        #region IEnumerable<KeyValuePair<object,object>> Members

        IEnumerator<KeyValuePair<object, object>> IEnumerable<KeyValuePair<object, object>>.GetEnumerator() {
            lock (this) {
                foreach (KeyValuePair<SymbolId, object> o in _data) {
                    yield return new KeyValuePair<object, object>(SymbolTable.IdToString(o.Key), o.Value);
                }
            }
        }

        #endregion

        #region IEnumerable Members

        public System.Collections.IEnumerator GetEnumerator() {
            foreach (KeyValuePair<SymbolId, object> o in _data) {
                yield return SymbolTable.IdToString(o.Key);
            }
        }

        #endregion

        #region IAttributesDictionary Members

        public bool Remove(SymbolId key) {
            lock (this) return _data.Remove(key);
        }

        public bool TryGetValue(SymbolId key, out object value) {
            lock (this) return _data.TryGetValue(key, out value);
        }

        object IAttributesCollection.this[SymbolId key] {
            get {
                lock (this) return _data[key];
            }
            set {
                lock (this) _data[key] = value;
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
                // TODO: We should really lock on something else...
                return this;
            }
        }
    }

}
