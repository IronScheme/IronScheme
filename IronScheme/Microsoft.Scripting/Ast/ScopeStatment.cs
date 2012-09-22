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
using Microsoft.Scripting.Utils;
using Microsoft.Scripting.Generation;

namespace Microsoft.Scripting.Ast {
    public class ScopeStatement : Statement {
        private readonly Expression/*!*/ _scope;
        private readonly Statement/*!*/ _body;

        public Expression Scope {
            get {
                return _scope;
            }
        }

        public Statement Body {
            get {
                return _body;
            }
        }

        internal ScopeStatement(SourceSpan span, Expression/*!*/ scope, Statement/*!*/ body)
          : base(AstNodeType.ScopeStatement, span)
        {
          _scope = scope;
          _body = body;
        }

        public override void Emit(CodeGen cg) {
            Slot tempContext = cg.ContextSlot;
            Slot newContext = cg.GetLocalTmp(typeof(CodeContext));

            _scope.Emit(cg);            //Locals dictionary
            cg.EmitCodeContext();       //CodeContext
            //cg.EmitBoolean(true);       //Visible = true
            cg.EmitCall(typeof(RuntimeHelpers), "CreateNestedCodeContext");

            newContext.EmitSet(cg);

            cg.ContextSlot = newContext;
            _body.Emit(cg);
            cg.ContextSlot = tempContext;
        }
    }

    /// <summary>
    /// Factory methods.
    /// </summary>
    public static partial class Ast {
        public static ScopeStatement Scope(Expression scope, Statement body) {
            return Scope(SourceSpan.None, scope, body);
        }

        public static ScopeStatement Scope(SourceSpan span, Expression scope, Statement body) {
            Contract.RequiresNotNull(scope, "scope");
            Contract.RequiresNotNull(body, "body");
            Contract.Requires(TypeUtils.CanAssign(typeof(IAttributesCollection), scope.Type), "scope", "Scope must be IAttributesCollection");

            return new ScopeStatement(span, scope, body);
        }
    }
}
