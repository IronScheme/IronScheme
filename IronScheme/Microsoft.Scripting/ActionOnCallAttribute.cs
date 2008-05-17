
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
using Microsoft.Scripting.Actions;

namespace Microsoft.Scripting {
    /// <summary>
    /// Provides a mechanism to enable a calls body to be hoisted into a dynamic site.
    /// 
    /// When a call site is being produced to call a method decorated with the subtype GetRule will be called
    /// to produce the rule.  Return a new rule if the call can be optimized or null if the default behavior
    /// should be applied.
    /// </summary>
    [AttributeUsage(AttributeTargets.Method)]
    public abstract class ActionOnCallAttribute : Attribute {
        public ActionOnCallAttribute() {
        }

        public abstract StandardRule<T> GetRule<T>(CodeContext callerContext, object[] args);
    }
}

#endif	
