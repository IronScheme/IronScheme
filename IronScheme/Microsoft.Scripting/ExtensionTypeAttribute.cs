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
using System.Text;
using System.Collections.Generic;
using System.Diagnostics;

namespace Microsoft.Scripting {
    /// <summary>
    /// Marks a class in the assembly as being an extension type for another type.
    /// </summary>
    [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1813:AvoidUnsealedAttributes")] // TODO: fix
    [AttributeUsage(AttributeTargets.Assembly, Inherited = false, AllowMultiple = true)]
    public class ExtensionTypeAttribute : Attribute {
        private readonly Type _extensionType;
        private readonly Type _extends;
        private static Dictionary<Type, Type> ExtensionTypeToType = new Dictionary<Type, Type>(10);

        public ExtensionTypeAttribute(Type extends, Type extensionType) {
            if (extends == null) {
                throw new ArgumentNullException("extends");
            }
            if (extensionType != null && !extensionType.IsPublic && !extensionType.IsNestedPublic) {
                throw new ArgumentException(String.Format("Extension type {0} must be public", extensionType.FullName), "extensionType");
            }

            _extends = extends;
            _extensionType = extensionType;
        }

        public Type Type {
            get {
                return _extensionType;
            }
        }

        public Type Extends {
            get {
                return _extends;
            }
        }

        public static bool IsExtensionType(Type t) {
            lock (ExtensionTypeToType) {
                return ExtensionTypeToType.ContainsKey(t);
            }
        }

        public static Type GetExtendedTypeFromExtension(Type t) {
            lock (ExtensionTypeToType) {
                return ExtensionTypeToType[t];
            }
        }

        public static void RegisterType(Type extendedType, Type extensionType) {
            if (extensionType == null) return;
            Debug.Assert(extensionType != null);

            lock (ExtensionTypeToType) {
                if (extendedType != null && extendedType.IsArray) {
                    if (extendedType == typeof(Array)) {
                        ExtensionTypeToType[extensionType] = extendedType;
                    }
                } else {
                    Type curType = extensionType;
                    do {
                        Debug.Assert(!ExtensionTypeToType.ContainsKey(curType));

                        ExtensionTypeToType[curType] = extendedType;
                        curType = curType.BaseType;
                    } while (curType != typeof(object) && curType != null && !ExtensionTypeToType.ContainsKey(curType));
                }
            }
        }        
    }

}
