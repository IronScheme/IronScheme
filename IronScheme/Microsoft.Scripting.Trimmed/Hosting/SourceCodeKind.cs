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

namespace Microsoft.Scripting.Hosting {

    /// <summary>
    /// Defines a kind of the source code. The parser sets its initial state accordingly.
    /// </summary>
    public enum SourceCodeKind {
        /// <summary>
        /// The kind of code is unknown. Parser should choose the least restrictive initial state (i.e. initial non-terminal).
        /// </summary>
        Default,

        /// <summary>
        /// The code is an expression.
        /// </summary>
        Expression,

        /// <summary>
        /// The code is a sequence of statements.
        /// </summary>
        Statements,

        /// <summary>
        /// The code is a single statement.
        /// </summary>
        SingleStatement,

        /// <summary>
        /// The code is a content of a file.
        /// </summary>
        File,

        /// <summary>
        /// The code is an interactive command.
        /// </summary>
        InteractiveCode,
    }
}
