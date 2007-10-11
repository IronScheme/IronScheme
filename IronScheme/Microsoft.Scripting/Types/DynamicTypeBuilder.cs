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
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Types {
    public class DynamicTypeBuilder : DynamicMixinBuilder {
        private new DynamicType _building;

        /// <summary>
        /// Creates a new DynamicTypeBuilder for a DynamicMixin with the specified name
        /// </summary>
        public DynamicTypeBuilder(string name, Type underlyingSystemType) {
            Contract.RequiresNotNull(name, "name");
            Contract.RequiresNotNull(underlyingSystemType, "underlyingSystemType");

            _building = new DynamicType(underlyingSystemType);
            _building.Name = name;
            _building.Builder = this;
            base._building = _building;
        }

        public DynamicTypeBuilder(string name, Type underlyingSystemType, Type extensionType)
            : this(name, underlyingSystemType) {
            _building.ExtensionType = extensionType;
        }

        internal DynamicTypeBuilder(DynamicType type) : base(type) {
            _building = type;
        }

        /// <summary>
        /// Gets the DynamicTypeBuilder for a pre-existing DynamicType.
        /// </summary>
        public static DynamicTypeBuilder GetBuilder(DynamicType type) {
            Contract.RequiresNotNull(type, "type");

            lock (type.SyncRoot) {
                if (type.Builder == null) {
                    type.Builder = new DynamicTypeBuilder(type);
                    return type.Builder;
                }

                return type.Builder;
            }
        }

        /// <summary>
        /// Returns the DynamicMixin that is being built by this DynamicMixinBuilder.
        /// </summary>
        public new DynamicType UnfinishedType {
            get {
                return _building;
            }
        }

        /// <summary>
        /// True if the type is a system type, false if the type is a user type.
        /// </summary>
        public bool IsSystemType {
            get {
                return _building.IsSystemType;
            }
            set {
                _building.IsSystemType = value;
            }
        }

        /// <summary>
        /// Adds a conversion from one type to another.
        /// </summary>
        public void AddConversion(Type from, Type to, CallTarget1 converter) {
            _building.AddConversion(from, to, converter);
        }
        
        /// <summary>
        /// Sets the type the dynamic tyep should impersonate
        /// </summary>
        public void SetImpersonationType(Type impersonate) {
            _building.ImpersonationType = impersonate;
        }

        /// <summary>
        /// Adds a new base type to the type being constructed
        /// </summary>
        public void AddBaseType(DynamicType baseType) {
            Contract.RequiresNotNull(baseType, "baseType");

            _building.AddBaseType(baseType);
        }

        public void SetBases(IList<DynamicType> bases) {
            _building.BaseTypes = new List<DynamicType>(bases);
        }

        /// <summary>
        /// Sets the interface which can be used for constructing instances of this object
        /// </summary>
        public void SetConstructor(ICallableWithCodeContext callable) {
            _building.SetConstructor(callable);
        }


        public void SetIsExtensible() {
            SetExtensionType(typeof(Extensible<>).MakeGenericType(_building.UnderlyingSystemType));
        }

        public void SetExtensionType(Type type) {
            _building.ExtensionType = type;
        }
    }
}
