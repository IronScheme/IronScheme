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

using Microsoft.Scripting.Hosting;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting
{

    /// <summary>
    /// Represents the context that is flowed for doing CodeGen.  Languages can derive
    /// from this class to provide additional contextual information.
    /// </summary>
    public sealed class CompilerContext
    {

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

        public SourceUnit SourceUnit
        {
            get
            {
                return _sourceUnit;
            }
        }

        public ParserSink ParserSink
        {
            get
            {
                return _parserSink;
            }
        }

        public ErrorSink Errors
        {
            get { return _errors; }
        }

        public CompilerContext(SourceUnit sourceUnit)
            : this(sourceUnit, null, null)
        {
        }

        public CompilerContext(SourceUnit sourceUnit, ErrorSink errorSink)
            : this(sourceUnit, errorSink, null)
        {
        }

        public CompilerContext(SourceUnit sourceUnit, ErrorSink errorSink, ParserSink parserSink)
        {
            Contract.RequiresNotNull(sourceUnit, "sourceUnit");

            _sourceUnit = sourceUnit;
            _errors = errorSink ?? sourceUnit.Engine.GetCompilerErrorSink();
            _parserSink = parserSink ?? ParserSink.Null;
        }
    }
}
