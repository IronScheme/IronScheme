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
using System.Collections;

namespace Microsoft.Scripting.Utils {
    public static class CollectionUtils {

        public static void AddRange<T>(ICollection<T> collection, IEnumerable<T> items) {
            Contract.RequiresNotNull(collection, "collection");
            Contract.RequiresNotNull(items, "items");

            foreach (T item in items) {
                collection.Add(item);
            }
        }

        public static IEnumerator<S> ToCovariant<T, S>(IEnumerator<T> enumerator)
            where T : S {

            Contract.RequiresNotNull(enumerator, "enumerator");

            while (enumerator.MoveNext()) {
                yield return enumerator.Current;
            }
        }

        public static IEnumerable<S> ToCovariant<T, S>(IEnumerable<T> enumerable)
            where T : S {
            return new CovariantConvertor<T, S>(enumerable);
        }

        private class CovariantConvertor<T, S> : IEnumerable<S> where T : S {
            private IEnumerable<T> _enumerable;

            public CovariantConvertor(IEnumerable<T> enumerable) {
                Contract.RequiresNotNull(enumerable, "enumerable");
                _enumerable = enumerable;
            }

            public IEnumerator<S> GetEnumerator() {
                return CollectionUtils.ToCovariant<T, S>(_enumerable.GetEnumerator());
            }

            IEnumerator IEnumerable.GetEnumerator() {
                return GetEnumerator();
            }
        }

        public static List<T> MakeList<T>(T item) {
            List<T> result = new List<T>();
            result.Add(item);
            return result;
        }

        public static int CountOf<T>(IList<T> list, T item) where T : IEquatable<T> {
            if (list == null) return 0;

            int result = 0;
            for (int i = 0; i < list.Count; i++) {
                if (list[i].Equals(item)) {
                    result++;
                }
            }
            return result;
        }

        public static bool TrueForAll<T>(IList<T> collection, Predicate<T> predicate) {
            Contract.RequiresNotNull(collection, "collection");
            Contract.RequiresNotNull(predicate, "predicate");

            foreach (T item in collection) {
                if (!predicate(item)) return false;
            }

            return true;
        }
    }
}
