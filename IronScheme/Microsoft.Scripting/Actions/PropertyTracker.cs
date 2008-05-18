
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
using System.Reflection;

using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Generation;

namespace Microsoft.Scripting.Actions {
    using Ast = Microsoft.Scripting.Ast.Ast;
    using System.Diagnostics;

    /// <summary>
    /// Represents a logical Property as a member of a Type.  This Property can either be a real 
    /// concrete Property on a type (implemented with a ReflectedPropertyTracker) or an extension
    /// property (implemented with an ExtensionPropertyTracker).
    /// </summary>
    public abstract class PropertyTracker : MemberTracker {
        public override TrackerTypes MemberType {
            get { return TrackerTypes.Property; }
        }

        public abstract MethodInfo GetGetMethod();
        public abstract MethodInfo GetSetMethod();
        public abstract MethodInfo GetGetMethod(bool privateMembers);
        public abstract MethodInfo GetSetMethod(bool privateMembers);

        public virtual MethodInfo GetDeleteMethod() {
            return null;
        }

        public virtual MethodInfo GetDeleteMethod(bool privateMembers) {
            return null;
        }

        public abstract ParameterInfo[] GetIndexParameters();

        public abstract bool IsStatic {
            get;
        }

        public abstract Type PropertyType {
            get;
        }

        #region Public expression builders

        public override Expression GetValue(ActionBinder binder, Type type) {
            if (!IsStatic || GetIndexParameters().Length > 0) {
                // need to bind to a value or parameters to get the value.
                return binder.ReturnMemberTracker(type, this);
            }

            MethodInfo getter = ResolveGetter();
            if (getter == null || getter.ContainsGenericParameters) {
                // no usable getter
                return null;
            }

            if (getter.IsPublic && getter.DeclaringType.IsPublic) {
                return binder.MakeCallExpression(getter);
            }

            // private binding is just a call to the getter method...
            return MemberTracker.FromMemberInfo(getter).Call(binder);
        }

        public override ErrorInfo GetError(ActionBinder binder) {
            MethodInfo getter = ResolveGetter();

            if(getter == null) {
                return binder.MakeMissingMemberErrorInfo(DeclaringType, Name);
            }
            
            if(getter.ContainsGenericParameters) {
                return binder.MakeGenericAccessError(this);
            }

            throw new InvalidOperationException();
        }
        
        #endregion

        #region Internal expression builders

        internal override Expression GetBoundValue(ActionBinder binder, Type type, Expression instance) {
            if (instance == null) {
                return null;
            }

            if (GetIndexParameters().Length > 0) {
                // need to bind to a value or parameters to get the value.
                return binder.ReturnMemberTracker(type, BindToInstance(instance));
            }

            MethodInfo getter = ResolveGetter();
            if (getter == null || getter.ContainsGenericParameters) {
                // no usable getter
                return null;
            }

            if (getter.IsPublic && getter.DeclaringType.IsPublic) {                
                return binder.MakeCallExpression(getter, instance);;
            }

            // private binding is just a call to the getter method...
            return MemberTracker.FromMemberInfo(getter).Call(binder, instance);
        }

        internal override ErrorInfo GetBoundError(ActionBinder binder, Expression instance) {
            MethodInfo getter = ResolveGetter();

            if (getter == null) {
                return binder.MakeMissingMemberErrorInfo(DeclaringType, Name);
            }

            if (getter.ContainsGenericParameters) {
                return binder.MakeGenericAccessError(this);
            }

            if (instance == null) {
                return binder.MakeInvalidParametersError(Name, 0);
            }

            throw new InvalidOperationException();
        }

        internal override MemberTracker BindToInstance(Expression instance) {
            if (IsStatic) {
                return new BoundMemberTracker(this, null);
            }

            return new BoundMemberTracker(this, instance);
        }

        #endregion

        #region Private expression builder helpers

        private MethodInfo ResolveGetter() {
            MethodInfo getter = GetGetMethod(true);

            if (getter == null) return null;

            // Allow access to protected getters TODO: this should go, it supports IronPython semantics.
            if (!getter.IsPublic && !(getter.IsFamily || getter.IsFamilyOrAssembly)) {
                if (!ScriptDomainManager.Options.PrivateBinding) {
                    getter = null;
                }
            }
            return CompilerHelpers.GetCallableMethod(getter);
        }

        #endregion
    }
}

#endif	
