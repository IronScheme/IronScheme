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

namespace Microsoft.Scripting.Ast {
    public class SwitchCase {
        private readonly SourceLocation _header;
        private readonly Expression _value;
        private readonly Statement _body;

        // A default case is represented with a null value.

        public SwitchCase(Expression value, Statement body)
            : this(value, body, SourceLocation.None) {
        }

        public SwitchCase(Expression value, Statement body, SourceLocation header) {
            _value = value;
            _body = body;
            _header = header;
        }

        public bool IsDefault {
            get { return _value == null; }
        }

        public Expression Value {
            get { return _value; }
        }

        public Statement Body {
            get { return _body; }
        }

        public SourceLocation Header {
            get { return _header; }
        }
    }
}
