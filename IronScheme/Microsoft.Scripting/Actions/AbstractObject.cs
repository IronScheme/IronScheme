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
        private Type _type;
        private bool _exact;
        private object _value;

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
            this._type = type;
            this._exact = exact;
            this._value = value;
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
