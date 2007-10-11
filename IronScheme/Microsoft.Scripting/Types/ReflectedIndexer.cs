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
using System.Reflection;

namespace Microsoft.Scripting.Types {
    /// <summary>
    /// Provides access to non-default .NET indexers (aka properties w/ parameters).
    /// 
    /// C# doesn't support these, but both COM and VB.NET do.  The types dictionary
    /// gets populated w/a ReflectedGetterSetter indexer which is a descriptor.  Getting
    /// the descriptor returns a bound indexer.  The bound indexer supports indexing.
    /// We support multiple indexer parameters via expandable tuples.
    /// </summary>
    public sealed class ReflectedIndexer : ReflectedGetterSetter {
        private object _instance;

        public ReflectedIndexer(PropertyInfo info, NameType nt)
            : base(info, info.GetGetMethod(), info.GetSetMethod(), nt) {
        }

        public ReflectedIndexer(ReflectedIndexer from, object instance)
            : base(from) {
            this._instance = instance;
        }

        public override bool TryGetValue(CodeContext context, object instance, DynamicMixin owner, out object value) {
            value = new ReflectedIndexer(this, instance);
            return true;
        }

        public bool SetValue(CodeContext context, object [] keys, object value) {
            return CallSetter(context, _instance, keys, value);
        }

        public object GetValue(CodeContext context, object[] keys) {
            return CallGetter(context, _instance, keys);
        }
    }

}
