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
    public sealed class SymbolDictionary : BaseSymbolDictionary, IDictionary, IDictionary<object, object>, IAttributesCollection {
        private Dictionary<SymbolId, object> _data = new Dictionary<SymbolId, object>();

        public SymbolDictionary() {
        }

        public SymbolDictionary(IAttributesCollection from) {
            // enumeration of a dictionary requires locking
            // the target dictionary.
            lock (from) {
                foreach (KeyValuePair<object, object> kvp in from) {
                    AsObjectKeyedDictionary().Add(kvp.Key, kvp.Value);
                }
            }

        }

        /// <summary>
        /// Symbol dictionaries are usually indexed using literal strings, which is handled using the Symbols.
        /// However, some languages allow non-string keys too. We handle this case by lazily creating an object-keyed dictionary,
        /// and keeping it in the symbol-indexed dictionary. Such access is slower, which is acceptable.
        /// </summary>
        [Obsolete]
        private Dictionary<object, object> GetObjectKeysDictionary() {
            Dictionary<object, object> objData = GetObjectKeysDictionaryIfExists();
            if (objData == null) {
                objData = new Dictionary<object, object>();
                _data.Add(BaseSymbolDictionary.ObjectKeys, objData);
            }
            return objData;
        }

        [Obsolete]
        private Dictionary<object, object> GetObjectKeysDictionaryIfExists() {
            object objData;
            if (_data.TryGetValue(BaseSymbolDictionary.ObjectKeys, out objData))
                return (Dictionary<object, object>)objData;
            return null;
        }

        #region IDictionary<object, object> Members

        void IDictionary<object, object>.Add(object key, object value) {
            Debug.Assert(!(key is SymbolId));

            string strKey = key as string;
            lock (this) {
               _data.Add(SymbolTable.StringToId(strKey), value);
            }
        }

        bool IDictionary<object, object>.ContainsKey(object key) {
            Debug.Assert(!(key is SymbolId));
            string strKey = key as string;
            lock (this) {
                if (!SymbolTable.StringHasId(strKey)) {
                    // Avoid creating a SymbolID if this string does not already have one
                    return false;
                }
                return _data.ContainsKey(SymbolTable.StringToId(strKey));
            }
        }

        ICollection<object> IDictionary<object, object>.Keys {
            get {
                // data.Keys is typed as ICollection<SymbolId>. Hence, we cannot return as a ICollection<object>.
                // Instead, we need to copy the data to a List<object>
                List<object> res = new List<object>();

                lock (this) {
                    foreach (SymbolId x in _data.Keys) {
                        res.Add(SymbolTable.IdToString(x));
                    }
                }

                return res;
            }
        }

        bool IDictionary<object, object>.Remove(object key) {
            Debug.Assert(!(key is SymbolId));
            string strKey = key as string;
            lock (this) {
               return _data.Remove(SymbolTable.StringToId(strKey));
            }
        }

        bool IDictionary<object, object>.TryGetValue(object key, out object value) {
            Debug.Assert(!(key is SymbolId));
            string strKey = key as string;
            lock (this) {
               return _data.TryGetValue(SymbolTable.StringToId(strKey), out value);
            }
        }

        ICollection<object> IDictionary<object, object>.Values {
            get {
                // Are there any object-keys? If not we can use a fast-path
                lock (this) {
                   return _data.Values;
                }
            }
        }

        public object this[object key] {
            get {
                Debug.Assert(!(key is SymbolId));
                string strKey = key as string;
                lock (this) {
                    object value;
                    if (_data.TryGetValue(SymbolTable.StringToId(strKey), out value))
                        return value;
                }
                throw new KeyNotFoundException(String.Format("'{0}'", key));
            }
            set {
                Debug.Assert(!(key is SymbolId));
                string strKey = key as string;
                lock (this) {
                    _data[SymbolTable.StringToId(strKey)] = value;
                }
            }
        }

        #endregion

        #region ICollection<KeyValuePair<object, object>> Members

        public void Add(KeyValuePair<object, object> item) {
            string strKey = item.Key as string;
            lock (this) {
               _data.Add(SymbolTable.StringToId(strKey), item.Value);
            }
        }

        public void Clear() {
            lock (this) _data.Clear();
        }

        public bool Contains(KeyValuePair<object, object> item) {
            object value;
            if (AsObjectKeyedDictionary().TryGetValue(item.Key, out value) && value == item.Value) return true;
            return false;
        }

        public void CopyTo(KeyValuePair<object, object>[] array, int arrayIndex) {
            // TODO:
            throw new NotImplementedException();
        }

        public int Count {
            get {
                lock (this) {
                    int count = _data.Count;
                    return count;
                }
            }
        }

        public bool IsReadOnly {
            get { return false; }
        }

        public bool Remove(KeyValuePair<object, object> item) {
            lock (this) {
                string strKey = item.Key as string;
                if (strKey != null) {
                    object value;
                    if (AsObjectKeyedDictionary().TryGetValue(strKey, out value) && value == item.Value) {
                        _data.Remove(SymbolTable.StringToId(strKey));
                        return true;
                    }
                }
            }
            return false;
        }

        #endregion

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

        public void Add(SymbolId key, object value) {
            lock (this) _data.Add(key, value);
        }

        public bool ContainsKey(SymbolId key) {
            lock (this) return _data.ContainsKey(key);
        }

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

        public IDictionary<SymbolId, object> SymbolAttributes {
            get {
                lock (this) {
                    return _data;
                }
            }
        }

        public void AddObjectKey(object key, object value) {
            AsObjectKeyedDictionary().Add(key, value);
        }

        public bool ContainsObjectKey(object key) {
            return AsObjectKeyedDictionary().ContainsKey(key);
        }

        public bool RemoveObjectKey(object key) {
            return AsObjectKeyedDictionary().Remove(key);
        }

        public bool TryGetObjectValue(object key, out object value) {
            return AsObjectKeyedDictionary().TryGetValue(key, out value);
        }

        public ICollection<object> Keys { get { return AsObjectKeyedDictionary().Keys; } }

        public override IDictionary<object, object> AsObjectKeyedDictionary() {
            return this;
        }

        #endregion

        #region IDictionary Members

        void IDictionary.Add(object key, object value) {
            AsObjectKeyedDictionary().Add(key, value);
        }

        public bool Contains(object key) {
            lock (this) return AsObjectKeyedDictionary().ContainsKey(key);
        }

        IDictionaryEnumerator IDictionary.GetEnumerator() {
            return new TransformDictionaryEnumerator(_data);
        }

        public bool IsFixedSize {
            get { return false; }
        }

        ICollection IDictionary.Keys {
            get {
                // data.Keys is typed as ICollection<SymbolId>. Hence, we cannot return as a ICollection.
                // Instead, we need to copy the data to a List.
                List<object> res = new List<object>();

                lock (this) {
                    foreach (SymbolId x in _data.Keys) {
                        res.Add(SymbolTable.IdToString(x));
                    }
                }

                return res;
            }
        }

        void IDictionary.Remove(object key) {
            Debug.Assert(!(key is SymbolId));
            string strKey = key as string;
            lock (this) {
                _data.Remove(SymbolTable.StringToId(strKey));
            }
        }

        ICollection IDictionary.Values {
            get {
                List<object> res = new List<object>();

                lock (this) {
                    foreach (KeyValuePair<SymbolId, object> x in _data) {
                        res.Add(x.Value);
                    }
                }

                return res;
            }
        }

        object IDictionary.this[object key] {
            get { return AsObjectKeyedDictionary()[key]; }
            set { AsObjectKeyedDictionary()[key] = value; }
        }

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
