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
using System.Runtime.Serialization;
using System.Text;

using Microsoft.Scripting.Hosting;
using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting {
   
    [Serializable]
    public class SyntaxErrorException : Exception {
        private SourceSpan _span;
        private SourceUnit _sourceUnit;
        private Severity _severity;
        private int _mappedLine;
        private int _errorCode;

        public SyntaxErrorException() : base() { }

        public SyntaxErrorException(string message) : base(message) { }

        public SyntaxErrorException(string message, Exception innerException)
            : base(message, innerException) {
        }

        public SyntaxErrorException(string message, SourceUnit sourceUnit, SourceSpan span, int errorCode, Severity severity)
            : base(message) {
            Contract.RequiresNotNull(message, "message");

            _span = span;
            _sourceUnit = sourceUnit;
            _severity = severity;
            _errorCode = errorCode;
            _mappedLine = -1; // lazy
        }

#if !SILVERLIGHT
        protected SyntaxErrorException(SerializationInfo info, StreamingContext context) 
            : base(info, context) { }
#endif

        /// <summary>
        /// Unmapped span.
        /// </summary>
        public SourceSpan RawSpan {
            get { return _span; }
        }

        public SourceUnit SourceUnit {
            get { return _sourceUnit; }
        }

        public Severity Severity {
            get { return _severity; }
        }

        public int Line {
            get {
                if (_mappedLine == -1) {
                    _mappedLine = (_sourceUnit != null) ? _sourceUnit.MapLine(_span.Start.Line) : _span.Start.Line;
                }
                return _mappedLine;
            }
        }

        public int Column {
            get { return _span.Start.Column; }
        }

        public int ErrorCode {
            get { return _errorCode; }
        }

        public string GetSymbolDocumentName() {
            return (_sourceUnit != null) ? _sourceUnit.GetSymbolDocument(_span.Start.Line) : null;
        }

        public string GetCodeLine() {
            return (_sourceUnit != null) ? _sourceUnit.GetCodeLine(Line) : null;
        }
    }
}