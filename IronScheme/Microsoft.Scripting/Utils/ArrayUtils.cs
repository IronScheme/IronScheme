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
using System.IO;
using System.Diagnostics;

namespace Microsoft.Scripting.Utils {
    public static class ArrayUtils {

#if SILVERLIGHT
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2105:ArrayFieldsShouldNotBeReadOnly")]
        public static readonly Type[] EmptyTypes = new Type[0];
#else
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2105:ArrayFieldsShouldNotBeReadOnly")]
        public static readonly Type[] EmptyTypes = Type.EmptyTypes;
#endif

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2105:ArrayFieldsShouldNotBeReadOnly")]
        public static readonly string[] EmptyStrings = new string[0];

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2105:ArrayFieldsShouldNotBeReadOnly")]
        public static readonly object[] EmptyObjects = new object[0];

        public static TOutput[] ConvertAll<TInput, TOutput>(TInput[] input, Converter<TInput, TOutput> conv) {
#if SILVERLIGHT
            Contract.RequiresNotNull(input, "input");
            Contract.RequiresNotNull(conv, "conv");

            TOutput[] res = new TOutput[input.Length];
            for (int i = 0; i < input.Length; i++) {
                res[i] = conv(input[i]);
            }
            return res;
#else
            return System.Array.ConvertAll<TInput, TOutput>(input, conv);
#endif
        }

        public static T[] FindAll<T>(T[] array, Predicate<T> match) {
#if SILVERLIGHT
            if (array == null) {
                throw new ArgumentNullException("array");
            }

            if (match == null) {
                throw new ArgumentNullException("match");
            }

            List<T> list = new List<T>();
            for (int i = 0; i < array.Length; i++) {
                if (match(array[i])) {
                    list.Add(array[i]);
                }
            }
            return list.ToArray();
#else
            return System.Array.FindAll(array, match);
#endif
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1814:PreferJaggedArraysOverMultidimensional", MessageId = "1#")] // TODO: fix
        public static void PrintTable(TextWriter output, string[,] table) {
            Contract.RequiresNotNull(output, "output");
            Contract.RequiresNotNull(table, "table");

            int max_width = 0;
            for (int i = 0; i < table.GetLength(0); i++) {
                if (table[i, 0].Length > max_width) {
                    max_width = table[i, 0].Length;
                }
            }

            for (int i = 0; i < table.GetLength(0); i++) {
                output.Write(" ");
                output.Write(table[i, 0]);

                for (int j = table[i, 0].Length; j < max_width + 1; j++) {
                    output.Write(' ');
                }

                output.WriteLine(table[i, 1]);
            }
        }

        /// <summary>
        /// Resizes an array to a speficied new size and copies a portion of the original array into its beginning.
        /// </summary>
        internal static void ResizeInternal(ref char[] array, int newSize, int start, int count) {
            Debug.Assert(array != null && newSize > 0 && count >= 0 && newSize >= count && start >= 0);

            char[] result = (newSize != array.Length) ? new char[newSize] : array;

            Buffer.BlockCopy(array, start * sizeof(char), result, 0, count * sizeof(char));

            array = result;
        }

        internal static T[] Copy<T>(T[] array) {
            return (array.Length > 0) ? (T[])array.Clone() : array;
        }

        public static T[] MakeArray<T>(IList<T> elements, int reservedSlotsBefore, int reservedSlotsAfter) {
            if (reservedSlotsAfter < 0) throw new ArgumentOutOfRangeException("reservedSlotsAfter");
            if (reservedSlotsBefore < 0) throw new ArgumentOutOfRangeException("reservedSlotsBefore");

            if (elements == null) {
                return new T[reservedSlotsBefore + reservedSlotsAfter];
            }

            T[] result = new T[reservedSlotsBefore + elements.Count + reservedSlotsAfter];
            elements.CopyTo(result, reservedSlotsBefore);
            return result;
        }

        public static T[] ShiftRight<T>(T[] array, int count) {
            Contract.RequiresNotNull(array, "array");
            if (count < 0) throw new ArgumentOutOfRangeException("count");

            T[] result = new T[array.Length + count];
            System.Array.Copy(array, 0, result, count, array.Length);
            return result;
        }

        public static T[] ShiftLeft<T>(T[] array, int count) {
            Contract.RequiresNotNull(array, "array");
            if (count < 0) throw new ArgumentOutOfRangeException("count");

            T[] result = new T[array.Length - count];
            System.Array.Copy(array, count, result, 0, result.Length);
            return result;
        }

        public static T[] Insert<T>(T item, T[] array) {
            T[] result = ShiftRight(array, 1);
            result[0] = item;
            return result;
        }

        public static T[] Insert<T>(T item1, T item2, T[] array) {
            T[] result = ShiftRight(array, 2);
            result[0] = item1;
            result[1] = item2;
            return result;
        }

        public static T[] Append<T>(T[] array, T item) {
            Contract.RequiresNotNull(array, "array");

            System.Array.Resize<T>(ref array, array.Length + 1);
            array[array.Length - 1] = item;
            return array;
        }

        public static T[] AppendRange<T>(T[] array, IList<T> items, int additionalItemCount) {
            Contract.RequiresNotNull(array, "array");
            if (additionalItemCount < 0) throw new ArgumentOutOfRangeException("additionalItemCount");

            int j = array.Length;

            System.Array.Resize<T>(ref array, array.Length + items.Count + additionalItemCount);

            for (int i = 0; i < items.Count; i++, j++) {
                array[j] = items[i];
            }

            return array;
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1814:PreferJaggedArraysOverMultidimensional")] // TODO: fix
        public static T[,] Concatenate<T>(T[,] array1, T[,] array2) {
            int columnsCount = array1.GetLength(1);
            Debug.Assert(array2.GetLength(1) == columnsCount);

            int row1Count = array1.GetLength(0);
            int row2Count = array2.GetLength(0);
            int totalRowsCount = row1Count + row2Count;
            T[,] result = new T[totalRowsCount, columnsCount];

            for (int i = 0; i < row1Count; i++) {
                for (int j = 0; j < columnsCount; j++) {
                    result[i, j] = array1[i, j];
                }
            }

            for (int i = 0; i < row2Count; i++) {
                for (int j = 0; j < columnsCount; j++) {
                    result[(i + row1Count), j] = array2[i, j];
                }
            }

            return result;
        }

        public static void SwapLastTwo<T>(T[] array) {
            Debug.Assert(array != null && array.Length >= 2);

            T temp = array[array.Length - 1];
            array[array.Length - 1] = array[array.Length - 2];
            array[array.Length - 2] = temp;
        }

        public static T[] RemoveFirst<T>(T[] array) {
            return ShiftLeft(array, 1);
        }

        public static T[] RemoveLast<T>(T[] array) {
            Contract.RequiresNotNull(array, "array");

            System.Array.Resize(ref array, array.Length - 1);
            return array;
        }

        public static T[] RemoveAt<T>(T[] array, int indexToRemove) {
            Contract.RequiresNotNull(array, "array");
            Contract.Requires(indexToRemove >= 0 && indexToRemove < array.Length, "index");

            T[] result = new T[array.Length - 1];
            if (indexToRemove > 0) {
                Array.Copy(array, 0, result, 0, indexToRemove);
            }
            int remaining = array.Length - indexToRemove - 1;
            if (remaining > 0) {
                Array.Copy(array, array.Length - remaining, result, result.Length - remaining, remaining);
            }
            return result;
        }

        public static T[] InsertAt<T>(T[] array, int index, params T[] items) {
            Contract.RequiresNotNull(array, "array");
            Contract.RequiresNotNull(items, "items");
            Contract.Requires(index >= 0 && index <= array.Length, "index");

            if (items.Length == 0) {
                return Copy(array);
            }

            T[] result = new T[array.Length + items.Length];
            if (index > 0) {
                Array.Copy(array, 0, result, 0, index);
            }
            Array.Copy(items, 0, result, index, items.Length);

            int remaining = array.Length - index;
            if (remaining > 0) {
                Array.Copy(array, array.Length - remaining, result, result.Length - remaining, remaining);
            }
            return result;
        }
    }
}
