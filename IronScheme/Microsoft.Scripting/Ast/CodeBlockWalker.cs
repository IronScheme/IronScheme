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

namespace Microsoft.Scripting.Ast {
    /// <summary>
    /// The walker will recurse into all statements/expressions,
    /// except for nested code blocks.
    /// </summary>
    class CodeBlockWalker : Walker {
        protected internal override bool Walk(CodeBlock node) {
            // Do not recurse into nested code block
            return false;
        }
        protected internal override bool Walk(CodeBlockExpression node) {
            // Do not recurse into nested code block expression
            return false;
        }
        protected internal override bool Walk(GeneratorCodeBlock node) {
            // Do not recurse into nested generator code block
            return false;
        }
    }
}
