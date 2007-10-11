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
using System.Runtime.Serialization;
using System.Text;
using System.Diagnostics;

namespace Microsoft.Scripting {
    /// <summary>
    /// Stores the location of a span of text in a source file.
    /// </summary>
    [Serializable]
    public struct SourceSpan {
        private readonly SourceLocation _start;
        private readonly SourceLocation _end;

        /// <summary>
        /// Constructs a new span with a specific start and end location.
        /// </summary>
        /// <param name="start">The beginning of the span.</param>
        /// <param name="end">The end of the span.</param>
        public SourceSpan(SourceLocation start, SourceLocation end) {
            ValidateLocations(start, end);
            this._start = start;
            this._end = end;
        }

        [Conditional("DEBUG")]
        private static void ValidateLocations(SourceLocation start, SourceLocation end) {
            if (start.IsValid && end.IsValid) {
                // both spans are valid, they should be well ordered
                Debug.Assert(start.Index <= end.Index && start.Line <= end.Line && (start.Line != end.Line || start.Column <= end.Column));
            } else {
                // both should be invalid
                Debug.Assert(!start.IsValid && !end.IsValid);
            }
        }

        /// <summary>
        /// The start location of the span.
        /// </summary>
        public SourceLocation Start {
            get { return _start; }
        }

        /// <summary>
        /// The end location of the span. Location of the first character behind the span.
        /// </summary>
        public SourceLocation End {
            get { return _end; }
        }

        /// <summary>
        /// Length of the span (number of characters inside the span).
        /// </summary>
        public int Length {
            get { return _end.Index - _start.Index; } 
        }

        /// <summary>
        /// A valid span that represents no location.
        /// </summary>
        public static readonly SourceSpan None = new SourceSpan(SourceLocation.None, SourceLocation.None);

        /// <summary>
        /// An invalid span.
        /// </summary>
        public static readonly SourceSpan Invalid = new SourceSpan(SourceLocation.Invalid, SourceLocation.Invalid);

        /// <summary>
        /// Whether the locations in the span are valid.
        /// </summary>
        public bool IsValid {
            get { return _start.IsValid && _end.IsValid; }
        }

        /// <summary>
        /// Compares two specified Span values to see if they are equal.
        /// </summary>
        /// <param name="left">One span to compare.</param>
        /// <param name="right">The other span to compare.</param>
        /// <returns>True if the spans are the same, False otherwise.</returns>
        public static bool operator ==(SourceSpan left, SourceSpan right) {
            return left._start == right._start && left._end == right._end;
        }

        /// <summary>
        /// Compares two specified Span values to see if they are not equal.
        /// </summary>
        /// <param name="left">One span to compare.</param>
        /// <param name="right">The other span to compare.</param>
        /// <returns>True if the spans are not the same, False otherwise.</returns>
        public static bool operator !=(SourceSpan left, SourceSpan right) {
            return left._start != right._start || left._end != right._end;
        }

        public override bool Equals(object obj) {
            if (!(obj is SourceSpan)) return false;

            SourceSpan other = (SourceSpan)obj;
            return _start == other._start && _end == other._end;
        }

        public override string ToString() {
            return _start.ToString() + " - " + _end.ToString();
        }

        public override int GetHashCode() {
            // 7 bits for each column (0-128), 9 bits for each row (0-512), xor helps if
            // we have a bigger file.
            return (_start.Column) ^ (_end.Column << 7) ^ (_start.Line << 14) ^ (_end.Line << 23);
        }

#if DEBUG
        public string ToDebugString() {
            return String.Format("{0}-{1}", _start.ToDebugString(), _end.ToDebugString());
        }
#endif
    }
}
