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
using Microsoft.Scripting.Ast;
using System.Globalization;

using System.Collections.Generic;
using System.Diagnostics;
using Microsoft.Scripting.Hosting;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting {

    /// <summary>
    /// Represents the context that is flowed for doing CodeGen.  Languages can derive
    /// from this class to provide additional contextual information.
    /// </summary>
    public sealed class CompilerContext {

        /// <summary>
        /// Source unit currently being compiled in the CompilerContext
        /// </summary>
        private readonly SourceUnit _sourceUnit;

        /// <summary>
        /// Current error sink.
        /// </summary>
        private readonly ErrorSink _errors;

        /// <summary>
        /// Sink for parser callbacks (e.g. brace matching, etc.).
        /// </summary>
        private readonly ParserSink _parserSink;

        /// <summary>
        /// Compiler specific options.
        /// </summary>
        private readonly CompilerOptions _options;

        public int DefaultErrorCode { get { return -1; } }
        public Severity DefaultSeverity { get { return Severity.Error; } }

        public SourceUnit SourceUnit {
            get {
                return _sourceUnit;
            }
        }

        public ParserSink ParserSink {
            get {
                return _parserSink;
            }
        }

        public ErrorSink Errors {
            get { return _errors; }
        }

        public CompilerOptions Options {
            get { return _options; }
        }

        public CompilerContext(SourceUnit sourceUnit)
            : this(sourceUnit, null, null, null) {
        }

        public CompilerContext(SourceUnit sourceUnit, CompilerOptions options, ErrorSink errorSink)
            : this(sourceUnit, options, errorSink, null) {
        }

        public CompilerContext(SourceUnit sourceUnit, CompilerOptions options, ErrorSink errorSink, ParserSink parserSink) {
            Contract.RequiresNotNull(sourceUnit, "sourceUnit");

            _sourceUnit = sourceUnit;
            _options = options ?? sourceUnit.Engine.GetDefaultCompilerOptions();
            _errors = errorSink ?? sourceUnit.Engine.GetCompilerErrorSink();
            _parserSink = parserSink ?? ParserSink.Null;
        }

        public CompilerContext CopyWithNewSourceUnit(SourceUnit sourceUnit) {
            return new CompilerContext(sourceUnit, (CompilerOptions)_options.Clone(), _errors);
        }

        #region Error Reporting

        public void AddError(string message, SourceLocation start, SourceLocation end) {
            AddError(message, start, end, DefaultSeverity, DefaultErrorCode);
        }

        public void AddError(string message, SourceLocation start, SourceLocation end, Severity severity) {
            AddError(message, start, end, severity, DefaultErrorCode);
        }

        public void AddError(string message, SourceLocation start, SourceLocation end, Severity severity, int errorCode) {
            _errors.Add(SourceUnit, message, new SourceSpan(start, end), errorCode, severity);
        }

        #endregion
    }
}
