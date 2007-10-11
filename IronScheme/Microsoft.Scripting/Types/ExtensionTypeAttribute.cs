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

namespace Microsoft.Scripting.Types {
    /// <summary>
    /// Marks a class in the assembly as being an extension type for another type.
    /// </summary>
    [AttributeUsage(AttributeTargets.Assembly, Inherited = false, AllowMultiple=true)]
    public class ExtensionTypeAttribute : Attribute {
        private Type _extensionType;
        private Type _extends;
        private bool _enableDerivation;
        private Type _derivationType;
        internal static Dictionary<Type, DynamicType> ExtensionTypeToType = new Dictionary<Type, DynamicType>(10);

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

        public virtual ExtensionNameTransformer Transformer {
            get {
                return null;
            }
        }

        public bool EnableDerivation {
            get {
                return _enableDerivation;
            }
            set {
                _enableDerivation = value;
            }
        }

        /// <summary>
        ///  TODO: Remove me and need to have custom derivation types.
        /// </summary>
        public Type DerivationType {
            get {
                return _derivationType;
            }
            set {
                _derivationType = value;
            }
        }

        public static bool IsExtensionType(Type t) {
            lock (ExtensionTypeToType) {
                return ExtensionTypeToType.ContainsKey(t);
            }
        }

        public static DynamicType GetExtendedTypeFromExtension(Type t) {
            lock (ExtensionTypeToType) {
                return ExtensionTypeToType[t];
            }
        }

        public static void RegisterType(Type extendedType, Type extensionType) {
            RegisterType(extendedType, extensionType, DynamicHelpers.GetDynamicTypeFromType(extendedType));
        }

        public static void RegisterType(Type extendedType, Type extensionType, DynamicType dt) {
            if (extensionType == null) return;
            Debug.Assert(extensionType != null);

            lock (ExtensionTypeToType) {
                if (extendedType != null && extendedType.IsArray) {
                    if (extendedType == typeof(Array)) ExtensionTypeToType[extensionType] = dt;
                } else {
                    Type curType = extensionType;
                    do {
                        Debug.Assert(!ExtensionTypeToType.ContainsKey(curType));

                        ExtensionTypeToType[curType] = dt;
                        curType = curType.BaseType;
                    } while (curType != typeof(object) && curType != null && !ExtensionTypeToType.ContainsKey(curType));
                }
            }
        }        
    }

}
