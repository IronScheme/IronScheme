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
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting {
    public struct ScriptModuleKind : IEquatable<ScriptModuleKind> {
        public static readonly ScriptModuleKind Default = new ScriptModuleKind("Default");
        public static readonly ScriptModuleKind Console = new ScriptModuleKind("System.Console");
        public static readonly ScriptModuleKind XamlPage = new ScriptModuleKind("System.Windows.XamlPage");

        private string _value;

        public string Value {
            get { return _value; }
        }

        public ScriptModuleKind(string value) {
            Contract.RequiresNotNull(value, "value");
            _value = value;
        }

        public static bool operator ==(ScriptModuleKind left, ScriptModuleKind right) {
            return left._value == right._value;
        }

        public static bool operator !=(ScriptModuleKind left, ScriptModuleKind right) {
            return left._value != right._value;
        }

        public bool Equals(ScriptModuleKind other) {
            return _value == other._value;
        }

        public override bool Equals(object obj) {
            return obj is ScriptModuleKind && Equals((ScriptModuleKind)obj);
        }

        public override int GetHashCode() {
            return _value.GetHashCode();
        }
    }
}
