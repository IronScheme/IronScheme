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
using System.Collections.ObjectModel;

namespace Microsoft.Scripting.Utils {
    public static class CollectionUtils {

        public static IEnumerator<S> ToCovariant<T, S>(IEnumerator<T> enumerator)
            where T : S {

            Contract.RequiresNotNull(enumerator, "enumerator");

            while (enumerator.MoveNext()) {
                yield return enumerator.Current;
            }
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


        public static ReadOnlyCollection<T> ToReadOnlyCollection<T>(IList<T> list) {
            ReadOnlyCollection<T> roc;
            if (list == null) {
                return null;
            } else if ((roc = list as ReadOnlyCollection<T>) != null) {
                return roc;
            } else {
                return new ReadOnlyCollection<T>(list);
            }
        }
    }
}
