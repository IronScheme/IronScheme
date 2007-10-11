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
using System.Reflection;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;

using Microsoft.Scripting;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Types {
    /// <summary>
    /// Base class for properties backed by methods.  These include our slot properties,
    /// indexers, and normal properties.  This class provides the storage of these as well
    /// as the storage of our optimized getter/setter methods, documentation for the property,
    /// etc...
    /// </summary>
    public abstract class ReflectedGetterSetter : DynamicTypeSlot, IContextAwareMember {
        private readonly MethodInfo _getter, _setter;
        private readonly PropertyInfo _info;
        private NameType _nameType;

        public ReflectedGetterSetter(PropertyInfo info, MethodInfo getter, MethodInfo setter, NameType nt) {
            _info = info;
            _getter = getter;
            _setter = setter;
            _nameType = nt;
        }

        protected ReflectedGetterSetter(ReflectedGetterSetter from) {
            _info = from._info;
            _getter = from._getter;
            _setter = from._setter;
            _nameType = from._nameType;
        }

        public virtual string Name {
            get {
                if (_info != null) {
                    return _info.Name;
                }

                return null;
            }
        }

        public MethodInfo Getter {
            get {
                return _getter;
            }
        }

        public MethodInfo Setter {
            get {
                return _setter;
            }
        }

        public PropertyInfo Info {
            get {
                return _info;
            }
        }

        public virtual Type DeclaringType {
            get {
                return _info.DeclaringType;
            }
        }

        protected NameType NameType {
            get {
                return _nameType;
            }
        }

        public object CallGetter(CodeContext context, object instance, object[] args) {
            if (instance == null && (Getter == null || !Getter.IsStatic)) return this;

            if (Getter == null) {
                throw new MissingMemberException("unreadable property");
            }

            MethodBinder binder = MethodBinder.MakeBinder(context.LanguageContext.Binder, Name, new MethodInfo[] { Getter }, BinderType.Normal);

            if (instance != null) {                
                return binder.CallInstanceReflected(context, instance, args);
            }

            return binder.CallReflected(context, CallType.None, args);
        }

        public bool CallSetter(CodeContext context, object instance, object[] args, object value) {
            if (instance == null && (Setter == null || !Setter.IsStatic)) return false;

            MethodBinder binder = MethodBinder.MakeBinder(context.LanguageContext.Binder, Name, new MethodInfo[] { Setter }, BinderType.Normal);

            if (args.Length == 0) {
                if (instance != null) {
                    binder.CallInstanceReflected(context, instance, value);
                } else {
                    binder.CallReflected(context, CallType.None, value);
                }
            } else {
                args = ArrayUtils.Append(args, value); 

                if (instance != null) {
                    binder.CallInstanceReflected(context, instance, args);
                } else {
                    binder.CallReflected(context, CallType.None, args);
                }
            }

            return true;
        }

        #region IContextAwareMember Members

        public override bool IsVisible(CodeContext context, DynamicMixin owner) {
            return _nameType == NameType.PythonProperty || context.ModuleContext.ShowCls;
        }

        #endregion
    }
}
