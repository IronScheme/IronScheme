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
    /// Represents a set of attributes that different functions can have.
    /// </summary>
    [Flags]
    public enum FunctionType {
        /// <summary>No flags have been set </summary>
        None = 0x0000,
        /// <summary>This is a function w/ no instance pointer </summary>
        Function = 0x0001,
        /// <summary>This is a method that requires an instance</summary>
        Method = 0x0002,
        /// <summary>Built-in functions can encapsulate both methods and functions, in which case both bits are set</summary>
        FunctionMethodMask = 0x0003,
        /// <summary>True is the function/method should be visible from pure-Python code</summary>
        AlwaysVisible = 0x0004,
        /// <summary>we should skip the type check for the this pointer (due to base type, or an InstanceOps method).</summary>
        SkipThisCheck = 0x0008,
        /// <summary>True if this is a function/method declared on an Ops type (StringOps, IntOps, etc...)</summary>
        OpsFunction = 0x0010,
        /// <summary>True if this is a __r*__ method for a CLS overloaded operator method</summary>
        ReversedOperator = 0x0020,
        /// <summary>This method represents a binary operator method for a CLS overloaded operator method</summary>
        BinaryOperator = 0x0040,
    }
}
