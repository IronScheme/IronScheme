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
    /// ScriptNameAttribute is used to decorate methods with an alternate name that should
    /// be exposed to ScriptCode.  Languages can derive from ScriptName and override the
    /// Name and Context properties to provide custom behavior.
    /// </summary>
    [AttributeUsage(AttributeTargets.Method | AttributeTargets.Field | AttributeTargets.Property, AllowMultiple = false, Inherited = false)]
    public class ScriptNameAttribute : Attribute {
        private string _name;

        public ScriptNameAttribute() {            
        }

        public ScriptNameAttribute(string name) {
            _name = name;
        }

        public virtual string Name {
            get {
                return _name;
            }
        }

        public virtual ContextId Context {
            get {
                return ContextId.Empty;
            }
        }
    }
}
