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
using System.Diagnostics;
using System.Reflection;
using System.Text;
using System.Collections;

using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Actions {
    /// <summary>
    /// Provides binding semantics for a language.  This include conversions as well as support
    /// for producing rules for actions.  These optimized rules are used for calling methods, 
    /// performing operators, and getting members using the ActionBinder's conversion semantics.
    /// </summary>
    public abstract class ActionBinder {
        private CodeContext _context;

        protected ActionBinder(CodeContext context) {
            _context = context;
        }

        /// <summary>
        /// Deprecated - only used by DelegateSignatureInfo.GenerateDelegateStub.  Use CodeContext
        /// passed in at rule creation time instead.
        /// </summary>
        internal CodeContext Context {
            get {
                return _context;
            }
        }

        /// <summary>
        /// Emits the code to convert an arbitrary object to the specified type.
        /// </summary>
        public virtual void EmitConvertFromObject(CodeGen cg, Type paramType) {
            cg.EmitCast(typeof(object), paramType);
        }

        /// <summary>
        /// Converts an object at runtime into the specified type.
        /// </summary>
        public virtual object Convert(object obj, Type toType) {
            if (obj == null) {
                if (!toType.IsValueType) {
                    return null;
                }
            } else {
                if (toType.IsValueType) {
                    if (toType == obj.GetType()) {
                        return obj;
                    }
                } else {
                    if (toType.IsInstanceOfType(obj)) {
                        return obj;
                    }
                }
            }
            throw new InvalidCastException(String.Format("Cannot convert {0} to {1}", obj != null ? obj.GetType().Name : "(null)", toType.Name));
        }

        /// <summary>
        /// Determines if a conversion exists from fromType to toType at the specified narrowing level.
        /// </summary>
        public abstract bool CanConvertFrom(Type fromType, Type toType, NarrowingLevel level);

        /// <summary>
        /// Selects the best (of two) candidates for conversion from actualType
        /// </summary>
        public virtual Type SelectBestConversionFor(Type actualType, Type candidateOne, Type candidateTwo, NarrowingLevel level) {
            return null;
        }

        /// <summary>
        /// Provides ordering for two parameter types if there is no conversion between the two parameter types.
        /// 
        /// Returns true to select t1, false to select t2.
        /// </summary>
        public abstract bool PreferConvert(Type t1, Type t2);


        /// <summary>
        /// Converts the provided expression to the given type.  The expression is safe to evaluate multiple times.
        /// </summary>
        public abstract Expression ConvertExpression(Expression expr, Type toType);

        /// <summary>
        /// Returns an expression which checks to see if the provided expression can be converted to the provided type.
        /// 
        /// TODO: Remove me when operator method binding disappears from the MethodBinder.
        /// </summary>
        public abstract Expression CheckExpression(Expression expr, Type toType);

        /// <summary>
        /// Gets the return value when an object contains out / by-ref parameters.  
        /// </summary>
        /// <param name="args">The values of by-ref and out parameters that the called method produced.  This includes the normal return
        /// value if the method does not return void.</param>
        public virtual object GetByRefArray(object[] args) {
            return args;
        }

        /// <summary>
        /// Non-public types can have public members that we find when calling type.GetMember(...).  This
        /// filters out the non-visible members by attempting to resolve them to the correct visible type.
        /// 
        /// If no correct visible type can be found then the member is not visible and we won't call it.
        /// </summary>
        private static MemberInfo[] FilterNonVisibleMembers(Type type, MemberInfo[] foundMembers) {
            if (!type.IsVisible && foundMembers.Length > 0 && !ScriptDomainManager.Options.PrivateBinding) {
                // need to remove any members that we can't get through other means
                List<MemberInfo> foundVisible = null;
                MemberInfo visible;
                MethodInfo mi;
                for (int i = 0; i < foundMembers.Length; i++) {
                    visible = null;
                    switch(foundMembers[i].MemberType) {
                        case MemberTypes.Method:
                            visible = CompilerHelpers.TryGetCallableMethod((MethodInfo)foundMembers[i]);
                            break;
                        case MemberTypes.Property:
                            PropertyInfo pi = (PropertyInfo)foundMembers[i];
                            mi = pi.GetGetMethod() ?? pi.GetSetMethod();
                            visible = CompilerHelpers.TryGetCallableMethod(mi);
                            if (visible != null) {
                                visible = visible.DeclaringType.GetProperty(pi.Name);
                            }
                            break;
                        case MemberTypes.Event:
                            EventInfo ei = (EventInfo)foundMembers[i];
                            mi = ei.GetAddMethod() ?? ei.GetRemoveMethod() ?? ei.GetRaiseMethod();
                            visible = CompilerHelpers.TryGetCallableMethod(mi);
                            if (visible != null) {
                                visible = visible.DeclaringType.GetEvent(ei.Name);
                            }
                            break;
                        // all others can't be exposed out this way
                    }
                    if (visible != null) {
                        if (foundVisible == null) foundVisible = new List<MemberInfo>();
                        foundVisible.Add(visible);
                    }
                }

                if (foundVisible != null) {
                    foundMembers = foundVisible.ToArray();
                } else {
                    foundMembers = new MemberInfo[0];
                }
            }
            return foundMembers;
        }

        #region Error Production

        public virtual ErrorInfo MakeMissingMemberErrorInfo(Type type, string name) {
            return ErrorInfo.FromException(
                Ast.Ast.New(
                    typeof(MissingMemberException).GetConstructor(new Type[] { typeof(string) }),
                    Ast.Ast.Constant(name)
                )
            );
        }


#if FULL
        public virtual ErrorInfo MakeGenericAccessError(MemberTracker info) {
            return ErrorInfo.FromException(
                Ast.Ast.New(
                    typeof(MemberAccessException).GetConstructor(new Type[] { typeof(string) }),
                    Ast.Ast.Constant(info.Name)
                )
            );
        } 
#endif


        public virtual ErrorInfo MakeInvalidParametersError(string name, int expectedParams, params Expression[] args) {            
            return ErrorInfo.FromException(
                Ast.Ast.Call(
                    typeof(RuntimeHelpers).GetMethod("TypeErrorForIncorrectArgumentCount", new Type[] { typeof(string), typeof(int), typeof(int) }),
                    Ast.Ast.Constant(name),
                    Ast.Ast.Constant(args.Length),
                    Ast.Ast.Constant(expectedParams)
                )
            );
        }

        public virtual ErrorInfo MakeConversionError(Type toType, Expression value) {            
            return ErrorInfo.FromException(
                Ast.Ast.Call(
                    typeof(RuntimeHelpers).GetMethod("CannotConvertError"),
                    Ast.Ast.RuntimeConstant(toType),
                    value
               )
            );
        }

        #endregion

        #region Deprecated Error production


#if FULL
        /// <summary>
        /// Provides a way for the binder to provide a custom error message when lookup fails.  Just
        /// doing this for the time being until we get a more robust error return mechanism.
        /// 
        /// Deprecated, use the non-generic version instead
        /// </summary>
        public virtual Statement MakeMissingMemberError<T>(StandardRule<T> rule, Type type, string name) {
            return rule.MakeError(
                Ast.Ast.New(
                    typeof(MissingMemberException).GetConstructor(new Type[] { typeof(string) }),
                    Ast.Ast.Constant(name)
                )
            );
        } 
#endif


        /// <summary>
        /// Provides a way for the binder to provide a custom error message when lookup fails.  Just
        /// doing this for the time being until we get a more robust error return mechanism.
        /// </summary>
        public virtual Expression MakeMissingMemberError(Type type, string name, Type returnType) {
            return Ast.Ast.New(
                typeof(MissingMemberException).GetConstructor(new Type[] { typeof(string) }),
                Ast.Ast.Constant(name)
            );
        }


#if FULL
        /// <summary>
        /// Provides a way for the binder to provide a custom error message when lookup fails.  Just
        /// doing this for the time being until we get a more robust error return mechanism.
        /// </summary>
        public virtual Statement MakeReadOnlyMemberError<T>(StandardRule<T> rule, Type type, string name) {
            return rule.MakeError(
                Ast.Ast.New(
                    typeof(MissingMemberException).GetConstructor(new Type[] { typeof(string) }),
                    Ast.Ast.Constant(name)
                )
            );
        } 

        /// <summary>
        /// Provides a way for the binder to provide a custom error message when lookup fails.  Just
        /// doing this for the time being until we get a more robust error return mechanism.
        /// </summary>
        public virtual Statement MakeUndeletableMemberError<T>(StandardRule<T> rule, Type type, string name) {
            return MakeReadOnlyMemberError<T>(rule, type, name);
        }
#endif



        #endregion

        /// <summary>
        /// Builds an expressoin for a call to the provided method using the given expressions.  If the
        /// method is not static the first parameter is used for the instance.
        /// 
        /// Parameters are converted using the binder's conversion rules.
        /// 
        /// If an incorrect number of parameters is provided MakeCallExpression returns null.
        /// </summary>
        public Expression MakeCallExpression(MethodInfo method, params Expression[] parameters) {
            ParameterInfo[] infos = method.GetParameters();
            Expression callInst = null;
            int parameter = 0, startArg = 0;
            Expression[] callArgs = new Expression[infos.Length];

            if (!method.IsStatic) {
                callInst = Ast.Ast.ConvertHelper(parameters[0], method.DeclaringType);
                parameter = 1;
            }
            if (infos.Length > 0 && infos[0].ParameterType == typeof(CodeContext)) {
                startArg = 1;
                callArgs[0] = Ast.Ast.CodeContext();
            }

            for (int arg = startArg; arg < infos.Length; arg++) {
                if (parameter < parameters.Length) {
                    callArgs[arg] = ConvertExpression(
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

            return Ast.Ast.SimpleCallHelper(callInst, method, callArgs);
        }

    }
}

