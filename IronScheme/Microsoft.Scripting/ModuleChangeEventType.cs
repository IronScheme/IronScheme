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
using System.Text;

namespace Microsoft.Scripting {
    /// <summary>
    /// The way in which a module has changed : Set or Delete
    /// </summary>
    public enum ModuleChangeType {
        /// <summary>
        /// A new value has been set in the module (or a previous value has changed).
        /// </summary>
        Set,
        /// <summary>
        /// A value has been removed from the module.
        /// </summary>
        Delete,
    }
}
