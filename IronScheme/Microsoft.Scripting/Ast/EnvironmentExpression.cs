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
using Microsoft.Scripting.Utils;
using Microsoft.Scripting.Generation;

namespace Microsoft.Scripting.Ast {
    public class EnvironmentExpression : Expression {
        private readonly Type /*!*/ _expressionType;

        internal EnvironmentExpression(Type /*!*/ expressionType)
            : base(AstNodeType.EnvironmentExpression) {
            _expressionType = expressionType;
        }

        public override Type Type {
            get { 
                return _expressionType;
            }
        }
        
        public override void Emit(CodeGen cg) {
            cg.EmitEnvironmentOrNull();
        }
    }

    public static partial class Ast {
        public static EnvironmentExpression Environment(Type type) {
            Contract.RequiresNotNull(type, "type");
            return new EnvironmentExpression(type);
        }
    }
}
