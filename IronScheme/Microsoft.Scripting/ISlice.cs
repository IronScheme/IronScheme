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

namespace Microsoft.Scripting {
    /// <summary>
    /// A useful interface for taking slices of numeric arrays, inspired by Python's Slice objects.
    /// </summary>
    public interface ISlice {
        /// <summary>
        /// The starting index of the slice or null if no first index defined
        /// </summary>
        object Start { get; }

        /// <summary>
        /// The ending index of the slice or null if no ending index defined
        /// </summary>
        object Stop { get; }

        /// <summary>
        /// The length of step to take
        /// </summary>
        object Step { get; }
    }
}