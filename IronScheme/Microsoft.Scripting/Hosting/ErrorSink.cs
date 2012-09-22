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
using Microsoft.Scripting.Generation;

namespace Microsoft.Scripting.Hosting {

    public enum Severity {
        Ignore,
        Warning,
        Error,
        FatalError,
    }

    public class ErrorSink 
    {
        private int _fatalErrorCount;
        private int _errorCount;
        private int _warningCount;

        public int FatalErrorCount {
            get { return _fatalErrorCount; }
        }
        
        public int ErrorCount {
            get { return _errorCount; }
        }

        public int WarningCount {
            get { return _warningCount; }
        }

        public bool AnyError {
            get {
                return _errorCount > 0 || _fatalErrorCount > 0;
            }
        }

        public ErrorSink() {
        }

        protected void CountError(Severity severity) {
            if (severity == Severity.FatalError) _fatalErrorCount++;
            else if (severity == Severity.Error) _errorCount++;
            else if (severity == Severity.Warning) _warningCount++;
        }

        public void ClearCounters() {
            _warningCount = _errorCount = _fatalErrorCount = 0;
        }

        public virtual void Add(SourceUnit sourceUnit, string message, SourceSpan span, int errorCode, Severity severity) {
            CountError(severity);
        }

        public SyntaxErrorException Add(SyntaxErrorException exception) {
            Add(exception.SourceUnit, exception.Message, exception.RawSpan, exception.ErrorCode, exception.Severity);
            return exception;
        }
    }

}
