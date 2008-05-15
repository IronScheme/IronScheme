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

namespace Microsoft.Scripting {
    /// <summary>
    /// Implemented by objects that can be called in a "new" expression
    /// Ops.Construct is the helper that is called at runtime that handles construction
    /// </summary>
    public interface IConstructorWithCodeContext {
        object Construct(CodeContext context, params object[] args);
    }
}