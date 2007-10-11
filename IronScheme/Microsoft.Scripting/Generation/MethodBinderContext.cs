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
using Microsoft.Scripting.Actions;
using Microsoft.Scripting.Ast;

namespace Microsoft.Scripting.Generation {
    /// <summary>
    /// Helper class for emitting calls via the MethodBinder.
    /// </summary>
    class MethodBinderContext {
        private ActionBinder _actionBinder;
        private StandardRule _rule;

        public MethodBinderContext(ActionBinder actionBinder, StandardRule rule) {
            _actionBinder = actionBinder;
            _rule = rule;
        }

        public Expression ConvertExpression(Expression expr, Type type) {
            return _actionBinder.ConvertExpression(expr, type);
        }

        public Expression CheckExpression(Expression expr, Type type) {
            return _actionBinder.CheckExpression(expr, type);
        }

        public Variable GetTemporary(Type type, string name) {
            return _rule.GetTemporary(type, name);
        }
    }
}
