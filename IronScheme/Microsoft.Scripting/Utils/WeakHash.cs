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
#if FULL
using System;
using System.Diagnostics;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;


namespace Microsoft.Scripting.Utils {
    /// <summary>
    /// Similar to Dictionary[TKey,TValue], but it also ensures that the keys will not be kept alive
    /// if the only reference is from this collection. The value will be kept alive as long as the key
    /// is alive.
    /// 
    /// This currently has a limitation that the caller is responsible for ensuring that an object used as 
    /// a key is not also used as a value in *any* instance of a WeakHash. Otherwise, it will result in the
    /// object being kept alive forever. This effectively means that the owner of the WeakHash should be the
    /// only one who has access to the object used as a value.
    /// 
    /// Currently, there is also no guarantee of how long the values will be kept alive even after the keys
    /// get collected. This could be fixed by triggerring CheckCleanup() to be called on every garbage-collection
    /// by having a dummy watch-dog object with a finalizer which calls CheckCleanup().
    /// </summary>
    public class WeakHash<TKey, TValue> : IDictionary<TKey, TValue> {
        // The one and only comparer instance.
        static readonly IEqualityComparer<object> comparer = new WeakComparer<object>();

        IDictionary<object, TValue> dict = new Dictionary<object, TValue>();
        int version, cleanupVersion;

#if SILVERLIGHT // GC
        WeakReference cleanupGC = new WeakReference(new object());
#else
        int cleanupGC = 0;
#endif

        public WeakHash() {
        }

        #region IDictionary<TKey,TValue> Members

        public void Add(TKey key, TValue value) {
            CheckCleanup();

            // If the WeakHash already holds this value as a key, it will lead to a circular-reference and result
            // in the objects being kept alive forever. The caller needs to ensure that this cannot happen.
            Debug.Assert(!dict.ContainsKey(value));

            dict.Add(new WeakObject<TKey>(key), value);
        }

        public bool ContainsKey(TKey key) {
            // We dont have to worry about creating "new WeakObject<TKey>(key)" since the comparer
            // can compare raw objects with WeakObject<T>.
            return dict.ContainsKey(key);
        }

        public ICollection<TKey> Keys {
            get { throw new NotImplementedException(); }
        }

        public bool Remove(TKey key) {
            return dict.Remove(key);
        }

        public bool TryGetValue(TKey key, out TValue value) {
            return dict.TryGetValue(key, out value);
        }

        public ICollection<TValue> Values {
            get { 
                // TODO:
                throw new NotImplementedException(); 
            }
        }

        public TValue this[TKey key] {
            get {
                return dict[key];
            }
            set {
                // If the WeakHash already holds this value as a key, it will lead to a circular-reference and result
                // in the objects being kept alive forever. The caller needs to ensure that this cannot happen.
                Debug.Assert(!dict.ContainsKey(value));

                dict[new WeakObject<TKey>(key)] = value;
            }
        }

        /// <summary>
        /// Check if any of the keys have gotten collected
        /// 
        /// Currently, there is also no guarantee of how long the values will be kept alive even after the keys
        /// get collected. This could be fixed by triggerring CheckCleanup() to be called on every garbage-collection
        /// by having a dummy watch-dog object with a finalizer which calls CheckCleanup().
        /// </summary>
        void CheckCleanup() {
            version++;

            long change = version - cleanupVersion;

            // Cleanup the table if it is a while since we have done it last time.
            // Take the size of the table into account.
            if (change > 1234 + dict.Count / 2) {
                // It makes sense to do the cleanup only if a GC has happened in the meantime.
                // WeakReferences can become zero only during the GC.

                bool garbage_collected;
#if SILVERLIGHT // GC.CollectionCount
                garbage_collected = !cleanupGC.IsAlive;
                if (garbage_collected) cleanupGC = new WeakReference(new object());
#else
                int currentGC = GC.CollectionCount(0);
                garbage_collected = currentGC != cleanupGC;
                if (garbage_collected) cleanupGC = currentGC;
#endif
                if (garbage_collected) {
                    Cleanup();
                    cleanupVersion = version;
                } else {
                    cleanupVersion += 1234;
                }
            }
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Reliability", "CA2004:RemoveCallsToGCKeepAlive")]
        private void Cleanup() {

            int liveCount = 0;
            int emptyCount = 0;

            foreach (WeakObject<TKey> w in dict.Keys) {
                if (w.Target != null)
                    liveCount++;
                else
                    emptyCount++;
            }

            // Rehash the table if there is a significant number of empty slots
            if (emptyCount > liveCount / 4) {
                Dictionary<object, TValue> newtable = new Dictionary<object, TValue>(liveCount + liveCount / 4, comparer);

                foreach (WeakObject<TKey> w in dict.Keys) {
                    object target = w.Target;

                    if (target != null)
                        newtable[w] = dict[w];

                    GC.KeepAlive(target);
                }

                dict = newtable;
            }
        }
        #endregion

        #region ICollection<KeyValuePair<TKey,TValue>> Members

        public void Add(KeyValuePair<TKey, TValue> item) {
            // TODO:
            throw new NotImplementedException();
        }

        public void Clear() {
            // TODO:
            throw new NotImplementedException();
        }

        public bool Contains(KeyValuePair<TKey, TValue> item) {
            // TODO:
            throw new NotImplementedException();
        }

        public void CopyTo(KeyValuePair<TKey, TValue>[] array, int arrayIndex) {
            // TODO:
            throw new NotImplementedException();
        }

        public int Count {
            get { 
                // TODO:
                throw new NotImplementedException(); 
            }
        }

        public bool IsReadOnly {
            get {
                // TODO:
                throw new NotImplementedException();
            }
        }

        public bool Remove(KeyValuePair<TKey, TValue> item) {
            // TODO:
            throw new NotImplementedException(); 
        }

        #endregion

        #region IEnumerable<KeyValuePair<TKey,TValue>> Members

        public IEnumerator<KeyValuePair<TKey, TValue>> GetEnumerator() {
            // TODO:
            throw new NotImplementedException(); 
        }

        #endregion

        #region IEnumerable Members

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator() {
            // TODO:
            throw new NotImplementedException(); 
        }

        #endregion
    }

    internal class WeakObject<T> {
        WeakReference weakReference;
        int hashCode;

        public WeakObject(T obj) {
            weakReference = new WeakReference(obj, true);
            hashCode = (obj == null) ? 0 : obj.GetHashCode();
        }

        public T Target {
            get {
                return (T)weakReference.Target;
            }
        }

        public override int GetHashCode() {
            return hashCode;
        }

        public override bool Equals(object obj) {
            return Target.Equals(obj);
        }
    }

    // WeakComparer treats WeakObject as transparent envelope
    sealed class WeakComparer<T> : IEqualityComparer<T> {
        bool IEqualityComparer<T>.Equals(T x, T y) {
            WeakObject<T> wx = x as WeakObject<T>;
            if (wx != null)
                x = wx.Target;

            WeakObject<T> wy = y as WeakObject<T>;
            if (wy != null)
                y = wy.Target;

            return Object.Equals(x, y);
        }

        int IEqualityComparer<T>.GetHashCode(T obj) {
            WeakObject<T> wobj = obj as WeakObject<T>;
            if (wobj != null)
                return wobj.GetHashCode();

            return (obj == null) ? 0 : obj.GetHashCode();
        }
    }

    public sealed class HybridMapping<T> {
        Dictionary<int, object> dict = new Dictionary<int, object>();
        readonly Object synchObject = new Object();
        readonly int SIZE = 4096;
        int current = 0;

        public int WeakAdd(T value) {
            lock (synchObject) {
                int saved = current;
                while (dict.ContainsKey(current)) {
                    current = (current + 1) % SIZE;
                    if (current == saved)
                        throw new InvalidOperationException("HybridMapping is full");
                }
                dict.Add(current, new WeakObject<T>(value));
                return current;
            }
        }

        public int StrongAdd(T value) {
            lock (synchObject) {
                int saved = current;
                while (dict.ContainsKey(current)) {
                    current = (current + 1) % SIZE;
                    if (current == saved)
                        throw new InvalidOperationException("HybridMapping is full");
                }
                dict.Add(current, value);
                return current;
            }
        }

        public T GetObjectFromId(int id) {
            object ret;
            if (dict.TryGetValue(id, out ret)) {
                WeakObject<T> weakObj = ret as WeakObject<T>;
                if (weakObj != null) {
                    return weakObj.Target;
                }
                if (ret is T) {
                    return (T)ret;
                }

                throw new InvalidOperationException("Unexpected dictionary content: type " + ret.GetType());
            } else
                return default(T);
        }

        public int GetIdFromObject(T value) {
            lock (synchObject) {
                foreach (KeyValuePair<int, object> kv in dict) {
                    if (kv.Value is WeakObject<T>) {
                        object target = ((WeakObject<T>)kv.Value).Target;
                        if (target != null && target.Equals(value))
                            return kv.Key;
                    } else if (kv.Value is T) {
                        object target = (T)(kv.Value);
                        if (target.Equals(value))
                            return kv.Key;
                    }
                }
            }
            return -1;
        }

        public void RemoveOnId(int id) {
            lock (synchObject) {
                dict.Remove(id);
            }
        }

        public void RemoveOnObject(T value) {
            try {
                int id = GetIdFromObject(value);
                RemoveOnId(id);
            } catch { }
        }
    }
}
#endif