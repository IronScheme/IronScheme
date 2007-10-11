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
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Generation {
    public sealed class RuntimeConstant : CompilerConstant {
        private object/*!*/ _value;

        internal RuntimeConstant(object/*!*/ value) {
            Contract.RequiresNotNull(value, "value");
            _value = value;
        }

        public override Type Type {
            get { return CompilerHelpers.GetVisibleType(_value); }
        }

        public override void EmitCreation(CodeGen cg) {
            throw new InvalidOperationException();
        }

        public override object Create() {
            return _value;
        }
    }
}
