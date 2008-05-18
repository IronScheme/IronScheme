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

#if FULL
private readonly RuleCache _ruleCache = new RuleCache(); 
#endif


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



#if FULL 
      
        // TODO: internal and friendly UnitTests
        public void ClearRuleCache() {
            _ruleCache.Clear();
        }

        public StandardRule<T> GetRule<T>(CodeContext callerContext, DynamicAction action, object[] args) {
            Contract.RequiresNotNull(action, "action");
            //Debug.Assert(action.Kind != ActionKind.GetMember || ((GetMemberAction)action).Name != SymbolTable.StringToId("x"));

            StandardRule<T> rule = _ruleCache.FindRule<T>(callerContext, action, args);
            if (rule != null) {
                return rule;
            }

            NoteRuleCreation(action, args);

            IDynamicObject ndo = args[0] as IDynamicObject;
            if (ndo != null) {
                rule = ndo.GetRule<T>(action, callerContext, args);
                Debug.Assert(rule == null || rule.Target != null && rule.Test != null);
            }

            rule = rule ?? MakeRule<T>(callerContext, action, args);
            Debug.Assert(rule != null && rule.Target != null && rule.Test != null);
#if DEBUG
            AstWriter.Dump(rule);
#endif        
            return rule;
        }

        internal object ExecuteRule<T>(CodeContext callerContext, DynamicAction action, object[] args) {
            T target = default(T);
            RuleSet<T> rules = null;
            
            return UpdateSiteAndExecute<T>(callerContext, action, args, null, ref target, ref rules);
        }

        /// <summary>
        /// Gets a rule, updates the site that called, and then returns the result of executing the rule.
        /// </summary>
        /// <typeparam name="T">The type of the DynamicSite the rule is being produced for.</typeparam>
        /// <param name="action">The Action the rule is being produced for.</param>
        /// <param name="args">The arguments to the rule as provided from the call site at runtime.</param>
        /// <param name="callerContext">The CodeContext that is requesting the rule and that should be used for conversions.</param>
        /// <param name="rules"></param>
        /// <param name="site"></param>
        /// <param name="target"></param>
        /// <returns>The result of executing the rule.</returns>
        internal object UpdateSiteAndExecute<T>(CodeContext callerContext, DynamicAction action, object[] args, object site, ref T target, ref RuleSet<T> rules) {
            Contract.RequiresNotNull(action, "action");
            //Debug.Assert(action.Kind != ActionKind.GetMember || ((GetMemberAction)action).Name != SymbolTable.StringToId("x"));
            object result;

            StandardRule<T> rule = _ruleCache.ExecuteRuleAndUpdateSite<T>(callerContext, action, args, site, ref target, ref rules, out result);
            if (rule != null) {
                return result;
            }

            NoteRuleCreation(action, args);

            for (; ; ) {
                IDynamicObject ndo = args[0] as IDynamicObject;
                if (ndo != null) {
                    rule = ndo.GetRule<T>(action, callerContext, args);
                    Debug.Assert(rule == null || rule.Target != null && rule.Test != null);
                }

                rule = rule ?? MakeRule<T>(callerContext, action, args);
                Debug.Assert(rule != null && rule.Target != null && rule.Test != null);
#if DEBUG
                AstWriter.Dump(rule);
#endif
                object[] callArgs = args;
                if (args.Length > 6) {
                    // BigDynamicSite
                    callArgs = new object[] { Tuple.MakeTuple(rule.ParamVariables[0].Type, args) };
                }

                CodeContext tmpCtx = callerContext.Scope.GetTemporaryVariableContext(callerContext, rule.ParamVariables, callArgs);
                try {
                    if ((bool)rule.Test.Evaluate(tmpCtx)) {
                        if (site != null) {
                            DynamicSiteHelpers.UpdateSite<T>(callerContext, site, ref target, ref rules, rule);
                        }

                        _ruleCache.AddRule(action, args, rule);

                        return rule.Target.Execute(tmpCtx);
                    }
                } finally {
                    tmpCtx.Scope.TemporaryStorage.Clear();
                }
            }
        }

        [Conditional("DEBUG")]
        private static void NoteRuleCreation(DynamicAction action, object[] args) {
            PerfTrack.NoteEvent(PerfTrack.Categories.Rules, "MakeRule " + action.ToString() + " " + CompilerHelpers.GetType(args[0]).Name);
        }
        
        /// <summary>
        /// Gets a rule for the provided action and arguments and executes it without compiling.
        /// </summary>
        public object Execute(CodeContext cc, DynamicAction action, object[] args) {
            return DynamicSiteHelpers.Execute(cc, this, action, args);
        }

        public virtual AbstractValue AbstractExecute(DynamicAction action, IList<AbstractValue> args) {
            throw new NotImplementedException();
        }

        /// <summary>
        /// Produces a rule for the specified Action for the given arguments.
        /// 
        /// The default implementation can produce rules for standard .NET types.  Languages should
        /// override this and provide any custom behavior they need and fallback to the default
        /// implementation if no custom behavior is required.
        /// </summary>
        /// <typeparam name="T">The type of the DynamicSite the rule is being produced for.</typeparam>
        /// <param name="action">The Action that is being performed.</param>
        /// <param name="args">The arguments to the action as provided from the call site at runtime.</param>
        /// <param name="callerContext">The CodeContext that is requesting the rule and should be use</param>
        /// <returns></returns>
        protected virtual StandardRule<T> MakeRule<T>(CodeContext/*!*/ callerContext, DynamicAction/*!*/ action, object[]/*!*/ args) {
            Contract.RequiresNotNull(callerContext, "callerContext");
            Contract.RequiresNotNull(action, "action");
            Contract.RequiresNotNull(args, "args");

            switch (action.Kind) {
                case DynamicActionKind.Call:
                    return new CallBinderHelper<T, CallAction>(callerContext, (CallAction)action, args).MakeRule();
                case DynamicActionKind.GetMember:
                    return new GetMemberBinderHelper<T>(callerContext, (GetMemberAction)action, args).MakeNewRule();
                case DynamicActionKind.SetMember:
                    return new SetMemberBinderHelper<T>(callerContext, (SetMemberAction)action, args).MakeNewRule();
                case DynamicActionKind.CreateInstance:
                    return new CreateInstanceBinderHelper<T>(callerContext, (CreateInstanceAction)action, args).MakeRule();
                case DynamicActionKind.DoOperation:
                    return new DoOperationBinderHelper<T>(callerContext, (DoOperationAction)action, args).MakeRule();
                case DynamicActionKind.DeleteMember:
                    return new DeleteMemberBinderHelper<T>(callerContext, (DeleteMemberAction)action, args).MakeRule();
                case DynamicActionKind.InvokeMember:
                    return new InvokeMemberBinderHelper<T>(callerContext, (InvokeMemberAction)action, args).MakeRule();
                case DynamicActionKind.ConvertTo:
                    return new ConvertToBinderHelper<T>(callerContext, (ConvertToAction)action, args).MakeRule();
                default:
                    throw new NotImplementedException(action.ToString());
            }
        } 
#endif


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
                    if (toType.IsAssignableFrom(obj.GetType())) {
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

#if FULL

        /// <summary>
        /// Gets the members that are visible from the provided type of the specified name.
        /// 
        /// The default implemetnation first searches the type, then the flattened heirachy of the type, and then
        /// registered extension methods.
        /// </summary>
        public virtual MemberGroup GetMember(DynamicAction action, Type type, string name) {
            MemberInfo[] foundMembers = type.GetMember(name);

            MemberGroup members = new MemberGroup(FilterNonVisibleMembers(type, foundMembers));

            // check for generic types w/ arity...
            Type[] types = type.GetNestedTypes(BindingFlags.Public);
            string genName = name + ReflectionUtils.GenericArityDelimiter;
            List<Type> genTypes = null;
            foreach (Type t in types) {
                if (t.Name.StartsWith(genName)) {
                    if (genTypes == null) genTypes = new List<Type>();
                    genTypes.Add(t);
                }
            }

            if (genTypes != null) {
                List<MemberTracker> mt = new List<MemberTracker>(members);
                foreach (Type t in genTypes) {
                    mt.Add(MemberTracker.FromMemberInfo(t));
                }
                return MemberGroup.CreateInternal(mt.ToArray());
            }
            
            if (members.Count == 0) {
                members = new MemberGroup(type.GetMember(name, BindingFlags.FlattenHierarchy | BindingFlags.Public | BindingFlags.Static | BindingFlags.Instance));
                if (members.Count == 0) {
                    members = GetExtensionMembers(type, name);
                }
            }
            
            return members;
        } 
#endif


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

#if FULL

        public virtual Statement MakeInvalidParametersError(MethodBinder binder, DynamicAction action, CallType callType, IList<MethodBase> targets, StandardRule rule, object []args) {
            int minArgs = Int32.MaxValue;
            int maxArgs = Int32.MinValue;
            int maxDflt = Int32.MinValue;
            int argsProvided = args.Length - 1; // -1 to remove the object we're calling
            bool hasArgList = false, hasNamedArgument = false;
            Dictionary<string, bool> namedArgs = new Dictionary<string, bool>();

            CallAction ca = action as CallAction;
            if (ca != null) {
                hasNamedArgument = ca.Signature.HasNamedArgument();
                int dictArgIndex = ca.Signature.IndexOf(ArgumentKind.Dictionary);

                if (dictArgIndex > -1) {
                    argsProvided--;
                    IAttributesCollection iac = args[dictArgIndex + 1] as IAttributesCollection;
                    if (iac != null) {
                        foreach (KeyValuePair<object, object> kvp in iac) {
                            namedArgs[(string)kvp.Key] = false;
                        }
                    }
                }

                argsProvided += GetParamsArgumentCountAdjust(ca, args);
                foreach (SymbolId si in ca.Signature.GetArgumentNames()) {
                    namedArgs[SymbolTable.IdToString(si)] = false;
                }
            } else {
                maxArgs = minArgs = rule.Parameters.Length;
                maxDflt = 0;
            }

            foreach (MethodBase mb in targets) {
                if (callType == CallType.ImplicitInstance && CompilerHelpers.IsStatic(mb)) continue;

                ParameterInfo[] pis = mb.GetParameters();
                int cnt = pis.Length;
                int dflt = 0;

                if (!CompilerHelpers.IsStatic(mb) && callType == CallType.None) {
                    cnt++;
                }

                foreach (ParameterInfo pi in pis) {
                    if (pi.ParameterType == typeof(CodeContext)) {
                        cnt--;
                    } else if (CompilerHelpers.IsParamArray(pi)) {
                        cnt--;
                        hasArgList = true;
                    } else if (CompilerHelpers.IsParamDictionary(pi)) {
                        cnt--;
                    } else if (!CompilerHelpers.IsMandatoryParameter(pi)) {
                        dflt++;
                        cnt--;
                    }

                    namedArgs[pi.Name] = true;
                }

                minArgs = System.Math.Min(cnt, minArgs);
                maxArgs = System.Math.Max(cnt, maxArgs);
                maxDflt = System.Math.Max(dflt, maxDflt);
            }

            foreach (KeyValuePair<string, bool> kvp in namedArgs) {
                if (kvp.Value == false) {
                    // unbound named argument.
                    return rule.MakeError(
                        Ast.Ast.Call(
                            typeof(RuntimeHelpers).GetMethod("TypeErrorForExtraKeywordArgument"),
                            Ast.Ast.Constant(binder._name, typeof(string)),
                            Ast.Ast.Constant(kvp.Key, typeof(string))
                        )
                    );
                }
            }

            return rule.MakeError(
                    Ast.Ast.Call(
                        typeof(RuntimeHelpers).GetMethod("TypeErrorForIncorrectArgumentCount", new Type[] {
                            typeof(string), typeof(int), typeof(int), typeof(int), typeof(int), typeof(bool), typeof(bool)
                        }),
                        Ast.Ast.Constant(binder._name, typeof(string)), // name
                        Ast.Ast.Constant(minArgs),                      // min formal normal arg cnt
                        Ast.Ast.Constant(maxArgs),                      // max formal normal arg cnt
                        Ast.Ast.Constant(maxDflt),                      // default cnt
                        Ast.Ast.Constant(argsProvided),                 // args provided
                        Ast.Ast.Constant(hasArgList),                   // hasArgList
                        Ast.Ast.Constant(hasNamedArgument)              // kwargs provided
                    )
                );
        }

        /// <summary>
        /// Given a CallAction and its arguments gets the number by which the parameter count should
        /// be adjusted due to the params array to get the logical number of parameters.
        /// </summary>
        protected static int GetParamsArgumentCountAdjust(CallAction action, object[] args) {
            int paramsCount = 0;
            int listArgIndex = action.Signature.IndexOf(ArgumentKind.List);

            if (listArgIndex > -1) {
                paramsCount--;
                IList<object> paramArgs = args[listArgIndex + 1] as IList<object>;
                if (paramArgs != null) {
                    paramsCount += paramArgs.Count;
                }
            }
            return paramsCount;
        } 
#endif



#if FULL
        public MemberGroup GetExtensionMembers(Type type, string name) {
            Type curType = type;
            do {
                IList<Type> extTypes = GetExtensionTypes(curType);
                List<MemberTracker> members = new List<MemberTracker>();

                foreach (Type ext in extTypes) {
                    foreach (MemberInfo mi in ext.GetMember(name)) {
                        members.Add(MemberTracker.FromMemberInfo(mi, true));
                    }

                    // TODO: Support indexed getters/setters w/ multiple methods
                    MethodInfo getter = null, setter = null, deleter = null;
                    foreach (MemberInfo mi in ext.GetMember("Get" + name)) {
                        if (!mi.IsDefined(typeof(PropertyMethodAttribute), false)) continue;
                        
                        Debug.Assert(getter == null);
                        getter = (MethodInfo)mi;
                    }

                    foreach (MemberInfo mi in ext.GetMember("Set" + name)) {
                        if (!mi.IsDefined(typeof(PropertyMethodAttribute), false)) continue;
                        Debug.Assert(setter == null);
                        setter = (MethodInfo)mi;
                    }

                    foreach (MemberInfo mi in ext.GetMember("Delete" + name)) {
                        if (!mi.IsDefined(typeof(PropertyMethodAttribute), false)) continue;
                        Debug.Assert(deleter == null);
                        deleter = (MethodInfo)mi;
                    }


#if FULL
                    if (getter != null || setter != null || deleter != null) {
                        members.Add(new ExtensionPropertyTracker(name, getter, setter, deleter, curType));
                    } 
#endif

                }

                if (members.Count != 0) {
                    return MemberGroup.CreateInternal(members.ToArray());
                }

                curType = curType.BaseType;
            } while (curType != null);

            return MemberGroup.EmptyGroup;
        } 
#endif


        protected internal virtual IList<Type> GetExtensionTypes(Type t) {
            // consult globally registered types
            return RuntimeHelpers.GetExtensionTypes(t);
        }

#if FULL

        /// <summary>
        /// Provides an opportunity for languages to replace all MemberInfo's with their own type.
        /// 
        /// Alternatlely a language can expose MemberInfo's directly.
        /// </summary>
        /// <param name="memberTracker">The member which is being returned to the user.</param>
        /// <param name="type">Tthe type which the memberTrack was accessed from</param>
        /// <returns></returns>
        protected internal virtual Expression ReturnMemberTracker(Type type, MemberTracker memberTracker) {
            if (memberTracker.MemberType == TrackerTypes.Bound) {
                BoundMemberTracker bmt = (BoundMemberTracker)memberTracker;
                return Ast.Ast.New(
                    typeof(BoundMemberTracker).GetConstructor(new Type[] { typeof(MemberTracker), typeof(object) }),
                    Ast.Ast.RuntimeConstant(bmt.BoundTo),
                    bmt.Instance);
            }

            return Ast.Ast.RuntimeConstant(memberTracker);
        } 
#endif


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

