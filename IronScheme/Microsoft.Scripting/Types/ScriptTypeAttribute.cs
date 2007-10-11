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

namespace Microsoft.Scripting.Types {
    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Interface | AttributeTargets.Struct, Inherited = false)]
    public class ScriptTypeAttribute : Attribute {
        private readonly string _name;
        private readonly Type _impersonateType;

        /// <summary>
        /// Provides the Python name for a type.  
        /// </summary>
        /// <param name="name"></param>
        public ScriptTypeAttribute(string name) {
            _name = name;
        }

        protected ScriptTypeAttribute(string name, Type type) {
            _name = name;
            _impersonateType = type;
        }
        /// <summary>
        /// Marks a type as impersonating another type.  The type's implementation will come from
        /// the type this is added on, but this type will logically appear to be a subclass of the
        /// impersonated type and will have its repr.
        /// 
        /// TODO: Remove impersonation and this overload
        /// </summary>
        public ScriptTypeAttribute(Type impersonateType) {
            // The impersonating type should have a name for the Python type it represents.
            object[] attrs = impersonateType.GetCustomAttributes(typeof(ScriptTypeAttribute), false);

            this._impersonateType = impersonateType;

            if (attrs.Length > 0)
                this._name = ((ScriptTypeAttribute)attrs[0]).Name;
            else
                this._name = impersonateType.Name;
        }

        public string Name {
            get {
                return _name;
            }
        }

        /// <summary>
        /// Returns the type the decorated type should impersonate.
        /// </summary>
        public Type ImpersonateType {
            get { return _impersonateType; }
        }

        public virtual ContextId Context {
            get {
                return ContextId.Empty;
            }
        }

        public virtual ExtensionNameTransformer GetTransformer(DynamicType type) {
            return null;
        }
    }
}
