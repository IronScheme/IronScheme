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
    internal static class IOUtils {
        /// <summary>
        /// Seeks the first character of a specified line in the text stream.
        /// Assumes the reader is currently positioned just before the first character of the first line.
        /// Line numbers are counted starting from 1.
        /// Returns <c>true</c> if the line is found (the current position of the reader will be  
        /// character read from the reader will be the first one of the line - if there is any), <b>false</b> otherwise.
        /// </summary>
        public static bool SeekLine(TextReader reader, int line) {
            Contract.RequiresNotNull(reader, "reader");
            if (line < 1) throw new ArgumentOutOfRangeException("line");
            if (line == 1) return true;

            int current_line = 1;

            for (; ; ) {
                int c = reader.Read();

                if (c == '\r') {
                    if (reader.Peek() == '\n') {
                        reader.Read();
                    }

                    current_line++;
                    if (current_line == line) return true;

                } else if (c == '\n') {
                    current_line++;
                    if (current_line == line) return true;
                } else if (c == -1) {
                    return false;
                }
            }
        }

        /// <summary>
        /// Reads characters to a string until end position or a terminator is reached. 
        /// Doesn't include the terminator into the resulting string.
        /// Returns <c>null</c>, if the reader is at the end position.
        /// </summary>
        public static string ReadTo(TextReader reader, char terminator) {
            Contract.RequiresNotNull(reader, "reader");

            StringBuilder result = new StringBuilder();
            int ch;
            for (; ; ) {
                ch = reader.Read();

                if (ch == -1) break;
                if (ch == terminator) return result.ToString();

                result.Append((char)ch);
            }
            return (result.Length > 0) ? result.ToString() : null;
        }

        /// <summary>
        /// Reads characters until end position or a terminator is reached.
        /// Returns <c>true</c> if the character has been found (the reader is positioned right behind the character), 
        /// <c>false</c> otherwise.
        /// </summary>
        public static bool SeekTo(TextReader reader, char c) {
            Contract.RequiresNotNull(reader, "reader");

            for (; ; ) {
                int ch = reader.Read();
                if (ch == -1) return false;
                if (ch == c) return true;
            }
        }

        public static string ToValidPath(string path) {
            return ToValidPath(path, false, true);
        }


        private static string ToValidPath(string path, bool isMask, bool isPath) {
            Debug.Assert(!isMask || isPath);

            if (String.IsNullOrEmpty(path)) {
                return "_";
            }

            StringBuilder sb = new StringBuilder(path);

            if (isPath) {
                foreach (char c in Path.GetInvalidPathChars()) {
                    sb.Replace(c, '_');
                }
            } else {
                foreach (char c in Path.GetInvalidFileNameChars()) {
                    sb.Replace(c, '_');
                }
            }

            if (!isMask) {
                sb.Replace('*', '_').Replace('?', '_');
            }

            return sb.ToString();
        }
    }
}
