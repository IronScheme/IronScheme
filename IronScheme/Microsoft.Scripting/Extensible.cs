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
#if FULL
using System;
using System.Collections.Generic;
using System.Text;

namespace Microsoft.Scripting {
    public class Extensible<T> {
        private T _value;

        public Extensible() { }
        public Extensible(T value) { this._value = value; }

        public T Value {
            get { return _value; }
        }

        public override bool Equals(object obj) {
            return _value.Equals(obj);
        }

        public override int GetHashCode() {
            return _value.GetHashCode();
        }

        public override string ToString() {
            return _value.ToString();
        }
    }

}
#endif