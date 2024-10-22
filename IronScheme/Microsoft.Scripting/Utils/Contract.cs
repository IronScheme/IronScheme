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
using System.Diagnostics;

namespace Microsoft.Scripting.Utils
{
    internal static class Contract {

        [Conditional("DEBUG")]
        public static void Requires(bool precondition) {
            if (!precondition) {
                throw new ArgumentException("Method precondition violated");
            }
        }

        [Conditional("DEBUG")]
        public static void Requires(bool precondition, string paramName) {
            Assert.NotEmpty(paramName);

            if (!precondition) {
                throw new ArgumentException("Invalid argument value", paramName);
            }
        }

        [Conditional("DEBUG")]
        public static void Requires(bool precondition, string paramName, string message) {
            Assert.NotEmpty(paramName);

            if (!precondition) {
                throw new ArgumentException(message, paramName);
            }
        }

        [Conditional("DEBUG")]
        public static void RequiresNotNull(object value, string paramName) {
            Assert.NotEmpty(paramName);

            if (value == null) {
                throw new ArgumentNullException(paramName);
            }
        }

        [Conditional("DEBUG")]
        public static void RequiresNotEmpty<T>(ICollection<T> collection, string paramName) {
            RequiresNotNull(collection, paramName);
            if (collection.Count == 0) {
                throw new ArgumentException("Non-empty collection required", paramName);
            }
        }

        /// <summary>
        /// Requires the specified index to point inside the array.
        /// </summary>
        /// <exception cref="ArgumentNullException">Array is <c>null</c>.</exception>
        /// <exception cref="ArgumentOutOfRangeException">Index is outside the array.</exception>
        [Conditional("DEBUG")]
        public static void RequiresArrayIndex<T>(IList<T> array, int index, string indexName) {
            Assert.NotEmpty(indexName);
            Assert.NotNull(array);

            if (index < 0 || index >= array.Count) throw new ArgumentOutOfRangeException(indexName);
        }

        /// <summary>
        /// Requires the array and all its items to be non-null.
        /// </summary>
        [Conditional("DEBUG")]
        public static void RequiresNotNullItems<T>(IList<T> array, string arrayName) {
            Assert.NotNull(arrayName);
            RequiresNotNull(array, arrayName);

            for (int i = 0; i < array.Count; i++) {
                if (array[i] == null) {
                    throw MakeArgumentItemNullException(i, arrayName);
                }
            }
        }

        private static ArgumentNullException MakeArgumentItemNullException(int index, string arrayName)
        {
            return new ArgumentNullException(String.Format("{0}[{1}]", arrayName, index));
        }
    }
}
