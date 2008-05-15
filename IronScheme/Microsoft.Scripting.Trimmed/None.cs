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

namespace Microsoft.Scripting {
    // 
    /// <summary>
    /// Gives us a concrete Type object to represent null's to allow for more uniform
    /// handling of methods that expect a Type or Type[].  This type can also be used by
    /// languages that support methods on null objects by adding extension methods with a
    /// Null/None this.
    /// 
    /// TODO Should be renamed to Null to reflect the standard .NET name rather than Python name
    /// </summary>
    public class None {
        public static readonly Type Type = typeof(None);

        /// <summary>
        /// Private constructor is never called since 'null' is the only valid instance.
        /// </summary>
        private None() { }
    }
}
