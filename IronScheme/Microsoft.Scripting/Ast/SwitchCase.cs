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

using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Ast {
    public class SwitchCase : Node {
        private readonly SourceLocation _header;
        private readonly bool _default;
        private readonly int _value;
        private readonly Statement _body;

        internal SwitchCase(SourceLocation header, bool @default, int value, Statement body)
            : base(AstNodeType.SwitchCase) {
            _header = header;
            _default = @default;
            _value = value;
            _body = body;
        }

        public bool IsDefault {
            get { return _default; }
        }

        public int Value {
            get { return _value; }
        }

        public Statement Body {
            get { return _body; }
        }

        public SourceLocation Header {
            get { return _header; }
        }
    }

    public static partial class Ast {
        public static SwitchCase DefaultCase(Statement body) {
            return DefaultCase(SourceLocation.None, body);
        }

        public static SwitchCase DefaultCase(SourceLocation header, Statement body) {
            Contract.RequiresNotNull(body, "body");
            return new SwitchCase(header, true, 0, body);
        }

        public static SwitchCase SwitchCase(int value, Statement body) {
            return SwitchCase(SourceLocation.None, value, body);
        }

        public static SwitchCase SwitchCase(SourceLocation header, int value, Statement body) {
            Contract.RequiresNotNull(body, "body");
            return new SwitchCase(header, false, value, body);
        }
    }
}
