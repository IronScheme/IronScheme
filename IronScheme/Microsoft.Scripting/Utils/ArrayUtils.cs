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

        internal static T[] Copy<T>(T[] array) {
            return (array.Length > 0) ? (T[])array.Clone() : array;
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

        public static T[] RemoveLast<T>(T[] array) {
            Contract.RequiresNotNull(array, "array");

            System.Array.Resize(ref array, array.Length - 1);
            return array;
        }
      
    }
}
