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
using System.Diagnostics;
using System.Reflection;

using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Actions {
    using Ast = Microsoft.Scripting.Ast.Ast;

    public class CreateInstanceBinderHelper<T> : CallBinderHelper<T, CreateInstanceAction> {
        public CreateInstanceBinderHelper(CodeContext context, CreateInstanceAction action, object []args)
            : base(context, action, args) {
        }

        public override StandardRule<T> MakeRule() {
            Type t = CompilerHelpers.GetType(Arguments[0]);
            if (typeof(IConstructorWithCodeContext).IsAssignableFrom(t)) {
                // TODO: This should go away when IConstructorWCC goes away.
                Debug.Assert(!Action.Signature.HasKeywordArgument());

                Expression call = Ast.SimpleCallHelper(
                    Rule.Parameters[0],
                    typeof(IConstructorWithCodeContext).GetMethod("Construct"),
                    GetICallableParameters(t, Rule)
                );

                Rule.SetTarget(Rule.MakeReturn(Binder, call));
                Rule.MakeTest(t);

                return Rule;
            }
            
            return base.MakeRule();
        }

        protected override MethodBase[] GetTargetMethods() {
            object target = Arguments[0];
            Type t = GetTargetType(target);

            if (t != null) {
                Test = Ast.AndAlso(Test, Ast.Equal(Rule.Parameters[0], Ast.RuntimeConstant(target)));

                return CompilerHelpers.GetConstructors(t);
            }

            return null;
        }
        
        private static Type GetTargetType(object target) {
            TypeTracker tt = target as TypeTracker;
            if (tt != null) {
                return tt.Type;
            }
            return target as Type;
        }
       
        protected override void MakeCannotCallRule(Type type) {
            string name = type.Name;
            Type t = Arguments[0] as Type;
            if (t != null) name = t.Name;

            Rule.SetTarget(
                Rule.MakeError(
                    Ast.New(
                        typeof(ArgumentTypeException).GetConstructor(new Type[] { typeof(string) }),
                        Ast.Constant("Cannot create instances of " + name)
                    )
                )
            );
        }
    }
}
