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

using Microsoft.Scripting.Actions;

namespace Microsoft.Scripting {
    public interface IDynamicObject {
        /// <summary>
        /// The language the object is defined in.
        /// Do not use this property! It will be gone when ICallable interfaces are removed and we figure out how to do GetDelegate via dynamic sites.
        /// </summary>
        LanguageContext LanguageContext { get; }

        StandardRule<T> GetRule<T>(DynamicAction action, CodeContext context, object[] args);
    }
}
