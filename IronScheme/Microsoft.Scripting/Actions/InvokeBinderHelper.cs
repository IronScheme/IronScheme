
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
using System.Diagnostics;
using System.Reflection;
using System.Collections;

using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Actions;
using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Actions {
    using Ast = Microsoft.Scripting.Ast.Ast;
    
    public class InvokeMemberBinderHelper<T> : BinderHelper<T, InvokeMemberAction> {
        public InvokeMemberBinderHelper(CodeContext context, InvokeMemberAction action, object[] args)
            : base(context, action) {
            Contract.RequiresNotNull(args, "args");
            if (args.Length < 1) throw new ArgumentException("Must receive at least one argument, the target to call", "args");
        }

        public virtual StandardRule<T> MakeRule() {
            CallAction callAction = CallAction.Make(Action.Signature);

            // TODO: First try to make a rule for get-member and see if we get back a constant method to call
            //GetMemberAction getAction = GetMemberAction.Make(Action.Name);
            //StandardRule<T> getRule = Binder.GetRule<T>(Context, getAction, new object[] { _args[0] });
            
            // otherwise, make a generic rule with embedded dynamic sites
            StandardRule<T> rule = new StandardRule<T>();
            rule.SetTest(Ast.True());
            Expression getExpr = Ast.Action.GetMember(Action.Name, typeof(object), rule.Parameters[0]);

            Expression[] callArgs = new Expression[rule.ParameterCount];
            callArgs[0] = getExpr;
            for (int i=1; i < callArgs.Length; i++) {
                callArgs[i] = rule.Parameters[i];
            }

            //TODO support non-object return types
            Expression callExpr = Ast.Action.Call(callAction, typeof(object), callArgs);

            rule.SetTarget(Ast.Return(callExpr));

            return rule;
        }
    }
}
