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
using System.Collections;

using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Actions;
using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Actions {
    using Ast = Microsoft.Scripting.Ast.Ast;
    
    /// <summary>
    /// Creates rules for performing method calls.  Currently supports calling built-in functions, built-in method descriptors (w/o 
    /// a bound value) and bound built-in method descriptors (w/ a bound value), delegates, types defining a "Call" method marked
    /// with SpecialName, ICallableWithContext, IFancyCallable, and ICallableWithContextAndThis.
    /// </summary>
    /// <typeparam name="T">The type of the dynamic site</typeparam>
    /// <typeparam name="ActionType">The specific type of CallAction</typeparam>
    public class CallBinderHelper<T, ActionType> : BinderHelper<T, ActionType> where ActionType : CallAction {
        private object[] _args;                                     // the arguments the binder is binding to - args[0] is the target, args[1..n] are args to the target
        private Expression _instance;                               // the instance or null if this is a non-instance call
        private Expression _test;                                   // the test expression, built up and assigned at the end
        private StandardRule<T> _rule = new StandardRule<T>();      // the rule we end up producing
        private bool _binaryOperator, _reversedOperator;            // if we're producing a binary operator or a reversed operator (should go away, Python specific).
        private MethodBase[] _targets;

        public CallBinderHelper(CodeContext context, ActionType action, object[] args)
            : base(context, action) {
            Contract.RequiresNotNull(args, "args");
            if (args.Length < 1) throw new ArgumentException("Must receive at least one argument, the target to call", "args");

            _args = args;
            _test = _rule.MakeTypeTest(CompilerHelpers.GetType(_args[0]), 0);
        }

        public CallBinderHelper(CodeContext context, ActionType action, object[] args, IList<MethodBase> targets)
            : this(context, action, args) {
            _targets = ArrayUtils.ToArray(targets);
        }

        public CallBinderHelper(CodeContext context, ActionType action, object[] args, IList<MethodBase> targets, bool isBinaryOperator, bool isReversedOperator)
            : this(context, action, args) {
            _targets = ArrayUtils.ToArray(targets);
            _binaryOperator = isBinaryOperator;
            _reversedOperator = isReversedOperator;
        }

        public virtual StandardRule<T> MakeRule() {
            Type t = CompilerHelpers.GetType(_args[0]);

            MethodBase[] targets = GetTargetMethods();
            if (targets != null && targets.Length > 0) {
                // we're calling a well-known MethodBase
                MakeMethodBaseRule(targets);
            } else if (typeof(ICallableWithCodeContext).IsAssignableFrom(t) || typeof(ICallableWithThis).IsAssignableFrom(t)) {
                // Old paths: these go away when everyone implements IDynamicObject.
                MakeICallableRule(t);
            } else {
                // we can't call this object
                MakeCannotCallRule(t);
            }

            // if we produced an ActionOnCall rule we don't replace the test w/ our own.
            if (_rule.Test == null) _rule.SetTest(_test);
            return _rule;
        }

        #region Method Call Rule

        private void MakeMethodBaseRule(MethodBase[] targets) {
            Type[] testTypes, argTypes;
            SymbolId[] argNames;

            //If an instance is explicitly passed in as an argument, ignore it.
            //Calls that need an instance will pick it up from the bound objects 
            //passed in or the rule. CallType can differentiate between the type 
            //of call during method binding.
            int instanceIndex = Action.Signature.IndexOf(ArgumentKind.Instance);
            if (instanceIndex > -1) {
                _args = ArrayUtils.RemoveAt(_args, instanceIndex + 1);
            }

            GetArgumentNamesAndTypes(out argNames, out argTypes);

            Type[] bindingArgs = argTypes;
            CallType callType = CallType.None;
            if (_instance != null) {
                bindingArgs = ArrayUtils.Insert(_instance.Type, argTypes);
                callType = CallType.ImplicitInstance;
            }

            if (_reversedOperator && bindingArgs.Length >= 2) {
                // we swap the arguments before binding, and swap back before calling.
                ArrayUtils.SwapLastTwo(bindingArgs);
                if (argNames.Length >= 2) {
                    ArrayUtils.SwapLastTwo(argNames);
                }
            }

            // attempt to bind to an individual method
            MethodBinder binder = MethodBinder.MakeBinder(Binder, GetTargetName(targets), targets, GetBinderType(targets), argNames);
            MethodCandidate cand = binder.MakeBindingTarget(callType, bindingArgs, out testTypes);

            if (cand != null) {
                // if we succeed make the target for the rule
                MethodBase target = cand.Target.Method;
                MethodInfo targetMethod = target as MethodInfo;

                if (targetMethod != null) {
                    target = CompilerHelpers.GetCallableMethod(targetMethod);
                }

                if (!MakeActionOnCallRule(target)) {
                    Expression[] exprargs = FinishTestForCandidate(testTypes, argTypes);

                    _rule.SetTarget(_rule.MakeReturn(
                        Binder,
                        cand.Target.MakeExpression(Binder, _rule, exprargs, testTypes)));
                }
            } else {
                // make an error rule
                MakeInvalidParametersRule(binder, callType, targets);
            }
        }

        private static string GetTargetName(MethodBase[] targets) {
            return targets[0].IsConstructor ? targets[0].DeclaringType.Name : targets[0].Name;
        }

        private Expression[] FinishTestForCandidate(Type[] testTypes, Type[] argTypes) {
            Expression[] exprargs = MakeArgumentExpressions();

            MakeSplatTests();

            if (_reversedOperator) {
                ArrayUtils.SwapLastTwo(exprargs);
            }

            if (argTypes.Length > 0 && testTypes != null) {
                // we've already tested the instance, no need to test it again...
                Expression[] testArgs = exprargs;
                Type[] types = testTypes;
                for (int i = 0; i < testArgs.Length; i++) {
                    if (testArgs[i] == _instance) {
                        testArgs = ArrayUtils.RemoveAt(testArgs, i);
                        types = ArrayUtils.RemoveAt(types, i);
                        break;
                    }
                }
                
                _test = Ast.AndAlso(_test, MakeNecessaryTests(_rule, new Type[][] { types }, testArgs));
            }

            return exprargs;
        }

        protected Expression[] MakeArgumentExpressions() {
            List<Expression> exprargs = new List<Expression>();
            if (_instance != null) exprargs.Add(_instance);
            for (int i = 0; i < Action.Signature.ArgumentCount; i++) { // ArgumentCount(Action, _rule)
                switch (Action.Signature.GetArgumentKind(i)) {
                    case ArgumentKind.Simple:
                    case ArgumentKind.Named:
                        exprargs.Add(_rule.Parameters[i + 1]);
                        break;

                    case ArgumentKind.List:
                        IList<object> list = (IList<object>)_args[i + 1];
                        for (int j = 0; j < list.Count; j++) {
                            exprargs.Add(
                                Ast.Call(
                                    Ast.Convert(
                                        _rule.Parameters[i + 1],
                                        typeof(IList<object>)
                                    ),
                                    typeof(IList<object>).GetMethod("get_Item"),
                                    Ast.Constant(j)
                                )
                            );
                        }
                        break;

                    case ArgumentKind.Dictionary:
                        IDictionary dict = (IDictionary)_args[i + 1];

                        IDictionaryEnumerator dictEnum = dict.GetEnumerator();
                        while (dictEnum.MoveNext()) {
                            DictionaryEntry de = dictEnum.Entry;

                            string strKey = de.Key as string;
                            if (strKey == null) continue;

                            Expression dictExpr = _rule.Parameters[_rule.Parameters.Length - 1];
                            exprargs.Add(
                                Ast.Call(
                                    Ast.ConvertHelper(dictExpr, typeof(IDictionary)),
                                    typeof(IDictionary).GetMethod("get_Item"),
                                    Ast.Constant(strKey)
                                )
                            );
                        }
                        break;
                }
            }
            return exprargs.ToArray();
        }

        #endregion

        #region Inline Action Rule

        /// <summary>
        /// Sees if the target is implemented with ActionOnCallAttribute and if so attempts to get a rule from the attribute. 
        /// </summary>
        /// <returns>True if the method implements ActionOnCall, false if not.</returns>
        private bool MakeActionOnCallRule(MethodBase target) {
            // see if the method provides a custom inline action
            object[] attrs = target.GetCustomAttributes(typeof(ActionOnCallAttribute), false);
            if (attrs.Length > 0) {
                StandardRule<T> rule = ((ActionOnCallAttribute)attrs[0]).GetRule<T>(Context, _args);
                if (rule != null) {
                    _rule = rule;
                    return true;
                }
            }
            return false;
        }

        #endregion

        #region ICallable Rule

        private void MakeICallableRule(Type t) {
            Expression call = null;
            if (!Action.Signature.HasKeywordArgument()) {
                if (Action.Signature.HasInstanceArgument() && typeof(ICallableWithThis).IsAssignableFrom(t)) {
                    call = Ast.SimpleCallHelper(
                        _rule.Parameters[0],
                        typeof(ICallableWithThis).GetMethod("Call"),
                        GetICallableParameters(t, _rule)
                    );
                } else {
                    call = Ast.SimpleCallHelper(
                        _rule.Parameters[0],
                        typeof(ICallableWithCodeContext).GetMethod("Call"),
                        GetICallableParameters(t, _rule)
                    );
                }
            } else if (typeof(IFancyCallable).IsAssignableFrom(t)) {
                call = Ast.SimpleCallHelper(
                    _rule.Parameters[0],
                    typeof(IFancyCallable).GetMethod("Call"),
                    GetICallableParameters(t, _rule)
                );
            } else {
                _rule.SetTarget(_rule.MakeError(MakeICallableError(t)));
                return;
            }

            _rule.SetTarget(_rule.MakeReturn(Binder, call));
        }

        private Expression MakeICallableError(Type t) {
            return Ast.New(
                typeof(ArgumentTypeException).GetConstructor(new Type[] { typeof(string) }),
                Ast.Constant(t.Name + " is not callable with keyword arguments")
            );
        }

        protected Expression[] GetICallableParameters(Type t, StandardRule<T> rule) {
            List<Expression> plainArgs = new List<Expression>();
            List<KeyValuePair<SymbolId, Expression>> named = new List<KeyValuePair<SymbolId, Expression>>();
            Expression splat = null, kwSplat = null;
            Expression instance = null;

            for (int i = 1; i < rule.Parameters.Length; i++) {
                switch (Action.Signature.GetArgumentKind(i - 1)) {
                    case ArgumentKind.Simple: plainArgs.Add(rule.Parameters[i]); break;
                    case ArgumentKind.List: splat = rule.Parameters[i]; break;
                    case ArgumentKind.Dictionary: kwSplat = rule.Parameters[i]; break;
                    case ArgumentKind.Named: named.Add(new KeyValuePair<SymbolId, Expression>(Action.Signature.GetArgumentName(i - 1), rule.Parameters[i])); break;
                    case ArgumentKind.Instance: instance = rule.Parameters[i]; break;
                    case ArgumentKind.Block: 
                    default:
                        throw new NotImplementedException();
                }
            }

            Expression argsArray = Ast.NewArrayHelper(typeof(object[]), plainArgs.ToArray());
            if (splat != null) {
                argsArray = Ast.Call(
                    typeof(BinderOps).GetMethod("GetCombinedParameters"),
                    argsArray,
                    Ast.ConvertHelper(splat, typeof(object))
                );
            }

            if (kwSplat != null || named.Count > 0) {
                // IFancyCallable.Call(context, args, names)
                Debug.Assert(instance == null); // not supported, no IFancyCallableWithInstance
                Expression names;

                if (named.Count > 0) {
                    List<Expression> constNames = new List<Expression>();
                    List<Expression> namedValues = new List<Expression>();
                    foreach (KeyValuePair<SymbolId, Expression> kvp in named) {
                        constNames.Add(Ast.Constant(SymbolTable.IdToString(kvp.Key)));
                        namedValues.Add(kvp.Value);
                    }

                    argsArray = Ast.Call(
                        typeof(BinderOps).GetMethod("GetCombinedParameters"),
                        argsArray,
                        Ast.NewArrayHelper(typeof(object[]), namedValues.ToArray())
                    );

                    names = Ast.NewArrayHelper(typeof(string[]), constNames.ToArray());
                } else {
                    names = Ast.Null(typeof(string[]));
                }

                if (kwSplat != null) {
                    Variable namesVar = rule.GetTemporary(typeof(string[]), "names");
                    argsArray = Ast.Comma(
                        Ast.Assign(namesVar, names),
                        Ast.Call(
                            typeof(BinderOps).GetMethod("GetCombinedKeywordParameters"),
                            argsArray,
                            Ast.ConvertHelper(kwSplat, typeof(IAttributesCollection)),
                            Ast.Read(namesVar)
                        )
                    );

                    return new Expression[] { Ast.CodeContext(), argsArray, Ast.Read(namesVar) };
                }
                return new Expression[] { Ast.CodeContext(), argsArray, names };
            }

            // ICallable.Call(context, args)
            if (instance != null && typeof(ICallableWithThis).IsAssignableFrom(t)) {
                return new Expression[] { Ast.CodeContext(), instance, argsArray };
            }

            return new Expression[] { Ast.CodeContext(), argsArray };
        }

        #endregion

        #region Target acquisition

        protected virtual MethodBase[] GetTargetMethods() {
            if (_targets != null) return _targets;

            object target = _args[0];
            MethodBase[] targets;
            Delegate d;
            MemberGroup mg;
            MethodGroup mthgrp;
            BoundMemberTracker bmt;

            if ((d = target as Delegate) != null) {
                targets = GetDelegateTargets(d);
            } else if ((mg = target as MemberGroup) != null) {
                List<MethodInfo> foundTargets = new List<MethodInfo>();
                foreach (MemberTracker mt in mg) {
                    if (mt.MemberType == TrackerTypes.Method) {
                        foundTargets.Add(((MethodTracker)mt).Method);
                    }
                }
                targets = foundTargets.ToArray();
            } else if ((mthgrp = target as MethodGroup) != null) {
                _test = Ast.AndAlso(_test, Ast.Equal(Ast.Convert(Rule.Parameters[0], typeof(object)), Ast.RuntimeConstant(target)));

                List<MethodBase> foundTargets = new List<MethodBase>();
                foreach (MethodTracker mt in mthgrp.Methods) {
                    foundTargets.Add(mt.Method);
                }

                targets = foundTargets.ToArray();
            } else if ((bmt = target as BoundMemberTracker) != null) {
                targets = GetBoundMemberTargets(bmt);
            } else {
                targets = GetOperatorTargets(target);
            }

            return targets;
        }

        private MethodBase[] GetBoundMemberTargets(BoundMemberTracker bmt) {
            Debug.Assert(bmt.Instance == null); // should be null for trackers that leak to user code

            MethodBase[] targets;            
            _instance = Ast.Convert(
                Ast.ReadProperty(
                    Ast.Convert(Rule.Parameters[0], typeof(BoundMemberTracker)), 
                    typeof(BoundMemberTracker).GetProperty("ObjectInstance")
                ),
                CompilerHelpers.GetVisibleType(CompilerHelpers.GetType(bmt.ObjectInstance))
            );
            _test = Ast.AndAlso(
                _test, 
                Ast.Equal(
                    Ast.ReadProperty(
                        Ast.Convert(Rule.Parameters[0], typeof(BoundMemberTracker)), 
                        typeof(BoundMemberTracker).GetProperty("BoundTo")
                    ),
                    Ast.RuntimeConstant(bmt.BoundTo)
                )
            );
            _test = Ast.AndAlso(
                _test,
                Rule.MakeTypeTest(
                    CompilerHelpers.GetType(bmt.ObjectInstance),
                    Ast.ReadProperty(
                        Ast.Convert(Rule.Parameters[0], typeof(BoundMemberTracker)),
                        typeof(BoundMemberTracker).GetProperty("ObjectInstance")
                    )
                )
            );                    
            switch (bmt.BoundTo.MemberType) {
                case TrackerTypes.MethodGroup:
                    targets = ((MethodGroup)bmt.BoundTo).GetMethodBases();
                    break;
                case TrackerTypes.Method:
                    targets = new MethodBase[] { ((MethodTracker)bmt.BoundTo).Method };
                    break;
                default:
                    throw new InvalidOperationException(); // nothing else binds yet
            }
            return targets;
        }

        private MethodBase[] GetDelegateTargets(Delegate d) {
            _instance = _rule.Parameters[0];
            return new MethodBase[] { d.GetType().GetMethod("Invoke") };
        }

        private MethodBase[] GetOperatorTargets(object target) {
            MethodBase[] targets = null;

            // see if the type defines a well known Call method
            Type targetType = CompilerHelpers.GetType(target);

            // some of these define SpecialName, work around that until the interfaces go away entirely...
            if (!typeof(ICallableWithCodeContext).IsAssignableFrom(targetType) &&
                !typeof(IFancyCallable).IsAssignableFrom(targetType)) {

                MemberGroup callMembers = Binder.GetMember(Action, targetType, "Call");
                List<MethodBase> callTargets = new List<MethodBase>();
                foreach (MemberTracker mi in callMembers) {
                    if (mi.MemberType == TrackerTypes.Method) {
                        MethodInfo method = ((MethodTracker)mi).Method;
                        if (method.IsSpecialName) {
                            callTargets.Add(method);
                        }
                    }
                }
                if (callTargets.Count > 0) {
                    targets = callTargets.ToArray();
                    _instance = Ast.Convert(_rule.Parameters[0], CompilerHelpers.GetType(_args[0]));
                }
            }
            return targets;
        }

        #endregion

        #region Test support

        /// <summary>
        /// Makes test for param arrays and param dictionary parameters.
        /// </summary>
        protected void MakeSplatTests() {
            if (Action.Signature.HasListArgument()) {
                MakeParamsArrayTest();
            }

            if (Action.Signature.HasDictionaryArgument()) {
                MakeParamsDictionaryTest();
            }
        }

        private void MakeParamsArrayTest() {
            int listIndex = Action.Signature.IndexOf(ArgumentKind.List);
            Debug.Assert(listIndex != -1);
            _test = Ast.AndAlso(_test, MakeParamsTest(_rule, _args[listIndex + 1], _rule.Parameters[listIndex + 1]));
        }

        private void MakeParamsDictionaryTest() {
            IDictionary dict = (IDictionary)_args[_args.Length - 1];
            IDictionaryEnumerator dictEnum = dict.GetEnumerator();

            // verify the dictionary has the same count and arguments.

            string[] names = new string[dict.Count];
            int index = 0;
            while (dictEnum.MoveNext()) {
                names[index++] = (string)dictEnum.Entry.Key;
            }

            _test = Ast.AndAlso(
                _test,
                Ast.AndAlso(
                    Ast.TypeIs(_rule.Parameters[_rule.Parameters.Length - 1], typeof(IDictionary)),
                    Ast.Call(
                        typeof(BinderOps).GetMethod("CheckDictionaryMembers"),
                        Ast.Convert(_rule.Parameters[_rule.Parameters.Length - 1], typeof(IDictionary)),
                        _rule.AddTemplatedConstant(typeof(string[]), names)
                    )
                )
            );
        }

        #endregion

        #region Error support

        protected virtual void MakeCannotCallRule(Type type) {
            _rule.SetTarget(
                _rule.MakeError(
                    Ast.New(
                        typeof(ArgumentTypeException).GetConstructor(new Type[] { typeof(string) }),
                        Ast.Constant(type.Name + " is not callable")
                    )
                )
            );
        }

        private void MakeInvalidParametersRule(MethodBinder binder, CallType callType, MethodBase[] targets) {
            MakeSplatTests();

            if (_args.Length > 1) {
                // we do an exact type check on all of the arguments types for a failed call.
                Expression[] argExpr = MakeArgumentExpressions();
                SymbolId[] names;
                Type[] vals;
                GetArgumentNamesAndTypes(out names, out vals);
                if (_instance != null) {
                    // target type was added to test already
                    argExpr = ArrayUtils.RemoveFirst(argExpr);
                }

                _test = Ast.AndAlso(_test, MakeNecessaryTests(_rule, new Type[][] { vals }, argExpr));
            }

            _rule.SetTarget(Binder.MakeInvalidParametersError(binder, Action, callType, targets, _rule, _args));
        }

        #endregion

        #region Misc. Helpers

        /// <summary>
        /// Gets all of the argument names and types.  Non named arguments are returned at the beginning of the argTypes array
        /// and named arguments line up w/ argTypes.
        /// </summary>
        protected void GetArgumentNamesAndTypes(out SymbolId[] argNames, out Type[] argTypes) {
            argNames = Action.Signature.GetArgumentNames();
            argTypes = GetArgumentTypes(Action, _args);
            if (Action.Signature.HasDictionaryArgument()) {
                // need to get names from dictionary argument...
                GetDictionaryNamesAndTypes(ref argNames, ref argTypes);
            }
        }

        private void GetDictionaryNamesAndTypes(ref SymbolId[] argNames, ref Type[] argTypes) {
            Debug.Assert(Action.Signature.GetArgumentKind(Action.Signature.ArgumentCount - 1) == ArgumentKind.Dictionary);

            List<SymbolId> names = new List<SymbolId>(argNames);
            List<Type> types = new List<Type>(argTypes);

            IDictionary dict = (IDictionary)_args[_args.Length - 1];
            IDictionaryEnumerator dictEnum = dict.GetEnumerator();
            while (dictEnum.MoveNext()) {
                DictionaryEntry de = dictEnum.Entry;

                if (de.Key is string) {
                    names.Add(SymbolTable.StringToId((string)de.Key));
                    types.Add(CompilerHelpers.GetType(de.Value));
                }
            }

            argNames = names.ToArray();
            argTypes = types.ToArray();
        }

        private BinderType GetBinderType(MethodBase[] targets) {
            if (_binaryOperator) return BinderType.BinaryOperator;

            foreach (MethodBase mb in targets) {
                if (mb.IsConstructor) {
                    return BinderType.Constructor;
                }
            }
            return BinderType.Normal;
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1819:PropertiesShouldNotReturnArrays")] // TODO: fix
        protected object[] Arguments {
            get {
                return _args;
            }
        }

        public StandardRule<T> Rule {
            get {
                return _rule;
            }
        }

        public Expression Instance {
            get {
                return _instance;
            }
            set {
                _instance = value;
            }
        }

        protected Expression Test {
            get {
                return _test;
            }
            set {
                _test = value;
            }
        }        
        #endregion          
    }
}
