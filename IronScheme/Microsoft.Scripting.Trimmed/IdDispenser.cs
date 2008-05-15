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
using System.Collections.Generic;
using SRC = System.Runtime.CompilerServices;

namespace Microsoft.Scripting {

    public static class IdDispenser {
        // The one and only comparer instance.
        private static readonly IEqualityComparer<object> comparer = new WrapperComparer();
        private static Dictionary<object, object> hashtable = new Dictionary<object, object>(comparer);
        private static readonly Object synchObject = new Object();  // The one and only global lock instance.
        // We do not need to worry about duplicates that to using long for unique Id.
        // It takes more than 100 years to overflow long on year 2005 hardware.
        private static long currentId = 42; // Last unique Id we have given out.

#if !SILVERLIGHT // GC.CollectionCount
        // cleanupId and cleanupGC are used for efficient scheduling of hashtable cleanups
        private static long cleanupId; // currentId at the time of last cleanup
        private static int cleanupGC; // GC.CollectionCount(0) at the time of last cleanup
#endif
        /// <summary>
        /// Given an ID returns the object associated with that ID.
        /// </summary>
        public static object GetObject(long id) {
            lock (synchObject) {
                foreach (Wrapper w in hashtable.Keys) {
                    if (w.Target != null) {
                        if (w.Id == id) return w.Target;
                    }
                }
                return null;
            }
        }

        /// <summary>
        /// Gets a unique ID for an object
        /// </summary>
        public static long GetId(Object o) {
            if (o == null)
                return 0;

            lock (synchObject) {
                // If the object exists then return it's existing ID.
                object res;
                if (hashtable.TryGetValue(o, out res)) {
                    return ((Wrapper)res).Id;
                }

                long uniqueId = checked(++currentId);

#if !SILVERLIGHT // GC.CollectionCount
                long change = uniqueId - cleanupId;

                // Cleanup the table if it is a while since we have done it last time.
                // Take the size of the table into account.
                if (change > 1234 + hashtable.Count / 2) {
                    // It makes sense to do the cleanup only if a GC has happened in the meantime.
                    // WeakReferences can become zero only during the GC.
                    int currentGC = GC.CollectionCount(0);
                    if (currentGC != cleanupGC) {
                        Cleanup();

                        cleanupId = uniqueId;
                        cleanupGC = currentGC;
                    } else {
                        cleanupId += 1234;
                    }
                }
#endif
                Wrapper w = new Wrapper(o, uniqueId);
                hashtable[w] = w;

                return uniqueId;
            }
        }

        /// <summary>
        /// Goes over the hashtable and removes empty entries 
        /// </summary>
        private static void Cleanup() {
            int liveCount = 0;
            int emptyCount = 0;

            foreach (Wrapper w in hashtable.Keys) {
                if (w.Target != null)
                    liveCount++;
                else
                    emptyCount++;
            }

            // Rehash the table if there is a significant number of empty slots
            if (emptyCount > liveCount / 4) {
                Dictionary<object, object> newtable = new Dictionary<object, object>(liveCount + liveCount / 4, comparer);

                foreach (Wrapper w in hashtable.Keys) {
                    if (w.Target != null)
                        newtable[w] = w;
                }

                hashtable = newtable;
            }
        }

        /// <summary>
        /// Weak-ref wrapper caches the weak reference, our hash code, and the object ID.
        /// </summary>
        private sealed class Wrapper {
            private WeakReference _weakReference;
            private int _hashCode;
            private long _id;

            public Wrapper(Object obj, long uniqueId) {
                _weakReference = new WeakReference(obj, true);

                _hashCode = (obj == null) ? 0 : SRC.RuntimeHelpers.GetHashCode(obj);
                _id = uniqueId;
            }

            public long Id {
                get {
                    return _id;
                }
            }

            public Object Target {
                get {
                    return _weakReference.Target;
                }
            }

            public override int GetHashCode() {
                return _hashCode;
            }
        }

        /// <summary>
        /// WrapperComparer treats Wrapper as transparent envelope 
        /// </summary>
        private sealed class WrapperComparer : IEqualityComparer<object> {            
            bool IEqualityComparer<object>.Equals(Object x, Object y) {

                Wrapper wx = x as Wrapper;
                if (wx != null)
                    x = wx.Target;

                Wrapper wy = y as Wrapper;
                if (wy != null)
                    y = wy.Target;

                return Object.ReferenceEquals(x, y);
            }

            int IEqualityComparer<object>.GetHashCode(Object obj) {

                Wrapper wobj = obj as Wrapper;
                if (wobj != null)
                    return wobj.GetHashCode();

                return GetHashCodeWorker(obj);
            }

            private static int GetHashCodeWorker(object o) {
                if (o == null) return 0;
                return SRC.RuntimeHelpers.GetHashCode(o);
            }
        }

    }
}
