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
using System.Diagnostics;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;

using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Actions;
using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Actions {
    using Ast = Microsoft.Scripting.Ast.Ast;

    public class BinderHelper<T, ActionType> where ActionType : DynamicAction {
        private CodeContext _context;
        private ActionType _action;
        
        public BinderHelper(CodeContext context, ActionType action) {
            Contract.RequiresNotNull(context, "context");
            Contract.RequiresNotNull(action, "action");

            _context = context;
            _action = action;
        }        

        protected CodeContext Context {
            get {
                return _context;
            }
        }

        protected ActionType Action {
            get {
                return _action;
            }
        }

        protected ActionBinder Binder {
            get {
                return _context.LanguageContext.Binder;
            }
        }        
               
        public static bool IsStrongBox(object target) {
            Type t = CompilerHelpers.GetType(target);

            return IsStrongBox(t);
        }

        public static bool IsStrongBox(Type t) {
            return t.IsGenericType && t.GetGenericTypeDefinition() == typeof(StrongBox<>);
        }

        public static UnaryExpression GetParamsList(StandardRule<T> rule) {
            return Ast.Convert(
                rule.Parameters[rule.ParameterCount - 1],
                typeof(IList<object>)
            );
        }
      
        public static Expression MakeParamsTest(StandardRule<T> rule, object paramArg, Expression listArg) {
            return Ast.AndAlso(
                Ast.TypeIs(listArg, typeof(ICollection<object>)),
                Ast.Equal(
                    Ast.ReadProperty(
                        Ast.Convert(listArg, typeof(ICollection<object>)),
                        typeof(ICollection<object>).GetProperty("Count")
                    ),
                    rule.AddTemplatedConstant(typeof(int), ((IList<object>)paramArg).Count)
                )
            );
        }

        public static Type[] GetArgumentTypes(CallAction action, object[] args) {
            List<Type> res = new List<Type>();
            for (int i = 1; i < args.Length; i++) {
                switch (action.Signature.GetArgumentKind(i - 1)) {
                    case ArgumentKind.Simple:
                    case ArgumentKind.Instance:
                    case ArgumentKind.Named:
                        res.Add(CompilerHelpers.GetType(args[i]));
                        continue;

                    case ArgumentKind.List:
                        IList<object> list = args[i] as IList<object>;
                        if (list == null) return null;

                        for (int j = 0; j < list.Count; j++) {
                            res.Add(CompilerHelpers.GetType(list[j]));
                        }
                        break;

                    case ArgumentKind.Dictionary: 
                        // caller needs to process these...
                        break;

                    default:
                        throw new NotImplementedException();
                }
            }
            return res.ToArray();
        }
        
        internal MethodInfo GetMethod(Type type, string name) {
            // declaring type takes precedence
            MethodInfo mi = type.GetMethod(name);
            if(mi != null) {
                return mi;
            }

            // then search extension types.
            Type curType = type;
            do {
                IList<Type> extTypes = Binder.GetExtensionTypes(curType);
                foreach (Type t in extTypes) {
                    MethodInfo next = t.GetMethod(name);
                    if (next != null) {
                        if (mi != null) {
                            throw new AmbiguousMatchException(String.Format("Found multiple members for {0} on type {1}", name, curType));
                        }

                        mi = next;
                    }
                }

                if (mi != null) {
                    return mi;
                }

                curType = curType.BaseType;
            } while (curType != null);

            return null;
        }

        /// <summary>
        /// Emits a call to the provided method using the given expressions.  If the
        /// method is not static the first parameter is used for the instance.
        /// </summary>
        public Expression MakeCallExpression(MethodInfo method, params Expression[] parameters) {
            ParameterInfo[] infos = method.GetParameters();
            Expression callInst = null;
            int parameter = 0, startArg = 0;
            Expression[] callArgs = new Expression[infos.Length];
            
            if (!method.IsStatic) {
                callInst = parameters[0];
                parameter = 1;
            }
            if (infos.Length > 0 && infos[0].ParameterType == typeof(CodeContext)) {
                startArg = 1;
                callArgs[0] = Ast.CodeContext();
            }

            for (int arg = startArg; arg < infos.Length; arg++) {
                if (parameter < parameters.Length) {
                    callArgs[arg] = Binder.ConvertExpression(
                        parameters[parameter++],
                        infos[arg].ParameterType);
                } else {
                    return null;
                }
            }

            // check that we used all parameters
            if (parameter != parameters.Length) {
                return null;
            }

            return Ast.Call(callInst, method, callArgs);
        }

        public Statement MakeCallStatement(MethodInfo method, params Expression[] parameters) {
            // TODO: Ast.Return not right, we need to go through the binder
            Expression call = MakeCallExpression(method, parameters);
            if (call != null) {
                return Ast.Return(call);
            }
            return null;
        }
       
        public static Expression MakeNecessaryTests(StandardRule<T> rule, IList<Type[]> necessaryTests, Expression [] arguments) {            
            Expression typeTest = Ast.Constant(true);
            if (necessaryTests.Count > 0) {
                Type[] testTypes = null;

                for (int i = 0; i < necessaryTests.Count; i++) {
                    if (necessaryTests[i] == null) continue;
                    if (testTypes == null) testTypes = new Type[necessaryTests[i].Length];

                    for (int j = 0; j < necessaryTests[i].Length; j++) {
                        if (testTypes[j] == null || testTypes[j].IsAssignableFrom(necessaryTests[i][j])) {
                            // no test yet or more specific test
                            testTypes[j] = necessaryTests[i][j];
                        }
                    }
                }

                if (testTypes != null) {
                    for (int i = 0; i < testTypes.Length; i++) {
                        if (testTypes[i] != null) {
                            Debug.Assert(i < arguments.Length);
                            typeTest = Ast.AndAlso(typeTest, rule.MakeTypeTest(testTypes[i], arguments[i]));
                        }
                    }
                }
            }
            return typeTest;
        }
    }
}
