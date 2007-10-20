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
using System.Reflection;
using System.Runtime.CompilerServices;

using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Actions {
    /// <summary>
    /// Represents a logical member of a type.  The member could either be real concrete member on a type or
    /// an extension member.
    /// 
    /// This seperates the "physical" members that .NET knows exist on types from the members that
    /// logically exist on a type.  It also provides other abstractions above the level of .NET reflection
    /// such as MemberGroups and NamespaceTracker's.
    /// 
    /// It also provides a wrapper around the reflection APIs which cannot be extended from partial trust.
    /// </summary>
    public abstract class MemberTracker {
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2105:ArrayFieldsShouldNotBeReadOnly")]
        public static readonly MemberTracker[] EmptyTrackers = new MemberTracker[0];

        private static Dictionary<MemberInfo, MemberTracker> _trackers = new Dictionary<MemberInfo, MemberTracker>();

        internal MemberTracker() {
        }

        /// <summary>
        /// The type of member tracker.
        /// </summary>
        public abstract TrackerTypes MemberType {
            get;
        }

        /// <summary>
        /// The logical declaring type of the member.
        /// </summary>
        public abstract Type DeclaringType {
            get;
        }

        /// <summary>
        /// The name of the member.
        /// </summary>
        public abstract string Name {
            get;
        }

        public static MemberTracker FromMemberInfo(MemberInfo member) {
            Contract.RequiresNotNull(member, "member");

            lock (_trackers) {
                MemberTracker res;
                if (_trackers.TryGetValue(member, out res)) return res;

                switch (member.MemberType) {
                    case MemberTypes.Constructor: res = new ConstructorTracker((ConstructorInfo)member); break;
                    case MemberTypes.Event: res = new EventTracker((EventInfo)member); break;
                    case MemberTypes.Field: res = new FieldTracker((FieldInfo)member); break;
                    case MemberTypes.Method: res = new MethodTracker((MethodInfo)member); break;
                    case MemberTypes.TypeInfo:
                    case MemberTypes.NestedType: res = new NestedTypeTracker((Type)member); break;
                    case MemberTypes.Property: res = new ReflectedPropertyTracker((PropertyInfo)member); break;
                    default: throw new InvalidOperationException("unknown type: " + member.MemberType);
                }

                _trackers[member] = res;
                return res;
            }
        }

        /// <summary>
        /// Returns the error associated with getting the value.  A null return value indicates that the default error message
        /// should be provided by the caller.
        /// </summary>
        public virtual Expression GetError(ActionBinder binder) {
            return null;
        }

        /// <summary>
        /// Gets the expression that creates the value.  Returns null if it's an error to get the value.  The caller can then
        /// call GetError to get the correct error Expression (or null if they should provide a default).
        /// </summary>
        public virtual Expression GetValue(ActionBinder binder) {
            return binder.ReturnMemberTracker(this);
        }

        /// <summary>
        /// Internal helper for getting values that have been bound.  Called from BoundMemberTracker.
        /// </summary>
        internal virtual Expression GetBoundValue(ActionBinder binder, Expression instance) {
            return GetValue(binder);
        }

        internal virtual MemberTracker BindToInstance(Expression instance) {
            return this;
        }
    }
}
