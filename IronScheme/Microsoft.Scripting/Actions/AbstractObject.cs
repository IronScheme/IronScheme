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
using Microsoft.Scripting.Types;

namespace Microsoft.Scripting.Actions {
    public class AbstractObject {
        private readonly Type _type;
        private readonly bool _exact;
        private readonly object _value;

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly AbstractObject NullObject = new AbstractObject(null, true, null);

        public static AbstractObject[] MakeAll(object[] args) {
            AbstractObject[] ret = new AbstractObject[args.Length];
            for (int i = 0; i < args.Length; i++) {
                ret[i] = Make(args[i]);
            }
            return ret;
        }

        public static AbstractObject Make(object o) {
            if (o == null) return NullObject;
            ISuperDynamicObject dynObj = o as ISuperDynamicObject;
            if (dynObj != null) {
                return new AbstractObject(o.GetType(), true, o);
            }
            return new AbstractObject(o.GetType(), true, null); //caching?
        }

        public static AbstractObject MakeType(Type type) {
            return new AbstractObject(type, type.IsValueType || type.IsSealed, null);
        }

        public AbstractObject(Type type, bool exact, object value) {
            _type = type;
            _exact = exact;
            _value = value;
        }

        public bool IsNull {
            get { return _type == null; }
        }

        public bool IsExact {
            get { return _exact; }
        }

        public Type Type {
            get { return _type; }
        }

        public bool IsDynamic {
            get { return _value != null; }
        }

        public object Value {
            get { return _value; }
        }
    }
}
