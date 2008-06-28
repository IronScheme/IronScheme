
#if FULL
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
    /// <summary>
    /// Provides hashing and equality based upon the value of the object instead of the reference.
    /// </summary>

    public interface IValueEquality {
        /// <summary>
        /// Gets the hash code for the value of the instance.
        /// </summary>
        /// <returns>A hash code</returns>
        /// <exception cref="Microsoft.Scripting.ArgumentTypeException">The type is mutable and cannot be hashed by value</exception>
        int GetValueHashCode();
        /// <summary>
        /// Determines if two values are equal
        /// </summary>
        /// <param name="other">The object to compare the current object against.</param>
        /// <returns>Returns true if the objects are equal, false if they are not.</returns>        
        bool ValueEquals(object other);
        /// <summary>
        /// Determines if two values are not equal
        /// </summary>
        /// <param name="other">The object to compare the current object against.</param>
        /// <returns>Returns true if the objects are not equal, false if they are.</returns>
        bool ValueNotEquals(object other);
    }
}

#endif	
