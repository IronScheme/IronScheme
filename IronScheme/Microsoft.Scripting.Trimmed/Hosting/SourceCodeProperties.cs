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
    public enum SourceCodeProperties {
        None,

        /// <summary>
        /// Source code is already invalid and no suffix can make it syntactically correct.
        /// </summary>
        IsInvalid,

        /// <summary>
        /// Last token is incomplete. Source code can still be completed correctly.
        /// </summary>
        IsIncompleteToken,

        /// <summary>
        /// Last statement is incomplete. Source code can still be completed correctly.
        /// </summary>
        IsIncompleteStatement,

        /// <summary>
        /// String represents an empty statement/expression.
        /// </summary>
        IsEmpty
    }

    public static class SourceCodePropertiesUtils {

        // TODO: extension method
        public static bool IsCompleteOrInvalid(SourceCodeProperties props, bool allowIncompleteStatement) {
            return
                props == SourceCodeProperties.IsInvalid ||
                props != SourceCodeProperties.IsIncompleteToken &&
                (allowIncompleteStatement || props != SourceCodeProperties.IsIncompleteStatement);
        }
    }
}
