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
using System.Reflection;
using System.Diagnostics;
using System.Text;
using Microsoft.Scripting.Actions;
using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Generation.Builders;

namespace Microsoft.Scripting
{
    public class MethodBinder {
        internal string _name;
        private BinderType _binderType;
        private Dictionary<int, TargetSet> _targetSets = new Dictionary<int, TargetSet>();
        private List<MethodCandidate> _paramsCandidates;
        private SymbolId[] _kwArgs;
        internal ActionBinder _binder;

        /// <summary>
        /// TODO: remove. If true checks for signature even though there is no other overload.
        /// This is right behavior. We need to fix all langauges that doesn't use it.
        /// </summary>
        private bool _strictParameterCheck;

        public bool StrictParameterCheck {
            get { return _strictParameterCheck; }
            set { _strictParameterCheck = value; }
        }

        private static bool IsUnsupported(MethodBase method) {
            return (method.CallingConvention & CallingConventions.VarArgs) != 0 || method.ContainsGenericParameters
                || Array.Exists(method.GetParameters(), p => p.ParameterType.Name.EndsWith("Span`1"));
        }

        public static MethodBinder MakeBinder(ActionBinder binder, string name, IList<MethodBase> mis, BinderType binderType) {
            return new MethodBinder(binder, name, mis, binderType, SymbolId.EmptySymbols);
        }

        public MethodCandidate MakeBindingTarget(CallType callType, Type[] types) {
            Type[] dummy;
            return MakeBindingTarget(callType, types, out dummy);
        }

        public MethodCandidate MakeBindingTarget(CallType callType, Type[] types, out Type[] argumentTests) {
            TargetSet ts = GetTargetSet(types.Length);
            if (ts != null) {
                return ts.MakeBindingTarget(callType, types, _kwArgs, out argumentTests);
            }
            argumentTests = null;
            return null;
        }

        private MethodBinder(ActionBinder binder, string name, IList<MethodBase> methods, BinderType binderType, SymbolId[] kwArgs) {
            _binder = binder;
            _name = name;
            _binderType = binderType;
            _kwArgs = kwArgs;
            foreach (MethodBase method in methods) {
                if (IsUnsupported(method)) continue;

                AddBasicMethodTargets(method);
            }

            if (_paramsCandidates != null) {
                foreach (MethodCandidate maker in _paramsCandidates) {
                    foreach (int count in _targetSets.Keys) {
                        MethodCandidate target = maker.MakeParamsExtended(binder, count, _kwArgs);
                        if (target != null) AddTarget(target);
                    }
                }
            }
        }

        public bool IsBinaryOperator {
            get {
                return _binderType == BinderType.BinaryOperator || IsComparison;
            }
        }

        private bool IsComparison {
            get {
                return _binderType == BinderType.ComparisonOperator;
            }
        }

        private void GetMinAndMaxArgs(out int minArgs, out int maxArgs) {
            List<int> argCounts = new List<int>(_targetSets.Keys);
            argCounts.Sort();
            minArgs = argCounts[0];
            maxArgs = argCounts[argCounts.Count - 1];
        }

        private Exception BadArgumentCount(CallType callType, int argCount) {
            if (_targetSets.Count == 0) return new ArgumentTypeException("no callable targets, if this is a generic method make sure to specify the type parameters");
            int minArgs, maxArgs;
            GetMinAndMaxArgs(out minArgs, out maxArgs);

            if (callType == CallType.ImplicitInstance) {
                argCount -= 1;
                if (maxArgs > 0) {
                    minArgs -= 1;
                    maxArgs -= 1;
                }
            }

            // This generates Python style error messages assuming that all arg counts in between min and max are allowed
            //It's possible that discontinuous sets of arg counts will produce a weird error message
            return RuntimeHelpers.TypeErrorForIncorrectArgumentCount(_name, maxArgs, maxArgs - minArgs, argCount);
        }

        private TargetSet BuildTargetSet(int count) {
            TargetSet ts = new TargetSet(this, count);
            if (_paramsCandidates != null) {
                foreach (MethodCandidate maker in _paramsCandidates) {
                    MethodCandidate target = maker.MakeParamsExtended(_binder, count, _kwArgs);
                    if (target != null) ts.Add(target);
                }
            } 
            
            return ts;
        }

        private TargetSet GetTargetSet(int nargs) {
            TargetSet ts;
            
            if (_targetSets.TryGetValue(nargs, out ts)) {
                return ts;
            } else if (_paramsCandidates != null) {
                ts = BuildTargetSet(nargs);
                if (ts._targets.Count > 0) {
                    return ts;
                }
            }
            
            return null;
        }

        public object CallReflected(CodeContext context, CallType callType, params object[] args) {
            TargetSet ts = GetTargetSet(args.Length);
            if (ts != null) return ts.CallReflected(context, callType, args, _kwArgs);
            throw BadArgumentCount(callType, args.Length);
        }

        private void AddTarget(MethodCandidate target) {
            int count = target.Target.ParameterCount;
            TargetSet set;
            if (!_targetSets.TryGetValue(count, out set)) {
                set = new TargetSet(this, count);
                _targetSets[count] = set;
            }
            set.Add(target);
        }

        private void AddSimpleTarget(MethodCandidate target) {
            AddTarget(target);
            if (CompilerHelpers.IsParamsMethod(target.Target.Method)) {
                if (_paramsCandidates == null) _paramsCandidates = new List<MethodCandidate>();
                _paramsCandidates.Add(target);
            }            
        }

        private void AddBasicMethodTargets(MethodBase method) {
            List<ParameterWrapper> parameters = new List<ParameterWrapper>();
            int argIndex = 0;
            ArgBuilder instanceBuilder;
            
            if (!CompilerHelpers.IsStatic(method)) {
                parameters.Add(new ParameterWrapper(_binder, method.DeclaringType, true));
                instanceBuilder = new SimpleArgBuilder(argIndex++, parameters[0].Type);
            } else {
                instanceBuilder = new NullArgBuilder();
            }

            ParameterInfo[] methodParams = method.GetParameters();
            List<ArgBuilder> argBuilders = new List<ArgBuilder>(methodParams.Length);
            List<ArgBuilder> defaultBuilders = new List<ArgBuilder>();
            bool hasByRefOrOut = false;

            foreach (ParameterInfo pi in methodParams) {
                if (pi.ParameterType == typeof(CodeContext) && argBuilders.Count == 0) {
                    argBuilders.Add(new ContextArgBuilder());
                    continue;
                }

                int newIndex= argIndex++;

                ArgBuilder ab;
                if (pi.ParameterType.IsByRef) {
                    hasByRefOrOut|= true;
                    ParameterWrapper param = new ParameterWrapper(_binder, pi);
                    parameters.Add(param);
                    ab = new SimpleArgBuilder(newIndex, param.Type, pi);
                }
                else {
                    hasByRefOrOut |= CompilerHelpers.IsOutParameter(pi);
                    ParameterWrapper param = new ParameterWrapper(_binder, pi);
                    parameters.Add(param);
                    ab = new SimpleArgBuilder(newIndex, param.Type, pi);
                }

                    argBuilders.Add(ab);
            }

            ReturnBuilder returnBuilder = new ReturnBuilder(CompilerHelpers.GetReturnType(method));
            if (hasByRefOrOut) AddSimpleTarget(MakeByRefReducedMethodTarget(method));
            AddSimpleTarget(MakeMethodCandidate(method, parameters, instanceBuilder, argBuilders, returnBuilder));
        }

        private MethodCandidate MakeMethodCandidate(MethodBase method, List<ParameterWrapper> parameters, ArgBuilder instanceBuilder, List<ArgBuilder> argBuilders, ReturnBuilder returnBuilder) {
            return new MethodCandidate(
                new MethodTarget(this, method, parameters.Count, instanceBuilder, argBuilders, returnBuilder),
                parameters);
        }

        private MethodCandidate MakeByRefReducedMethodTarget(MethodBase method) {
            List<ParameterWrapper> parameters = new List<ParameterWrapper>();
            int argIndex = 0;
            ArgBuilder instanceBuilder;
            if (!CompilerHelpers.IsStatic(method)) {
                parameters.Add(new ParameterWrapper(_binder, method.DeclaringType, true));
                instanceBuilder = new SimpleArgBuilder(argIndex++, parameters[0].Type);
            } else {
                instanceBuilder = new NullArgBuilder();
            }

            List<ArgBuilder> argBuilders = new List<ArgBuilder>();

            List<int> returnArgs = new List<int>();
            if (CompilerHelpers.GetReturnType(method) != typeof(void)) {
                returnArgs.Add(-1);
            }

            int paramCount = 0;
            foreach (ParameterInfo pi in method.GetParameters()) {
                if (pi.ParameterType == typeof(CodeContext) && paramCount == 0) {
                    argBuilders.Add(new ContextArgBuilder());
                    continue;
                }
                paramCount++;

                int newIndex = argIndex++;

                ArgBuilder ab;

                ParameterWrapper param = new ParameterWrapper(_binder, pi);
                parameters.Add(param);
                ab = new SimpleArgBuilder(newIndex, param.Type, pi);

                argBuilders.Add(ab);

            }

            ReturnBuilder returnBuilder = new ReturnBuilder(CompilerHelpers.GetReturnType(method));
            return MakeMethodCandidate(method, parameters, instanceBuilder, argBuilders, returnBuilder);
        }


        public override string ToString()
        {
          List<string> res = new List<string>();
          foreach (int key in _targetSets.Keys)
          {
            res.Add(_targetSets[key].ToString());
          }
          return string.Join(Environment.NewLine, res.ToArray());
        }
    }

    public class TargetSet {
        private MethodBinder _binder;
        internal int _count;
        internal List<MethodCandidate> _targets;

        public TargetSet(MethodBinder binder, int count) {
            this._binder = binder;
            this._count = count;
            _targets = new List<MethodCandidate>();
        }
       
        public MethodCandidate MakeBindingTarget(CallType callType, Type[] types, SymbolId[] names, out Type[] argTests) {
            List<MethodCandidate> targets = SelectTargets(callType, types, names);

            if (targets.Count == 1) {                
                argTests = GetTypesForTest(types, _targets);
                return targets[0];
            }
            argTests = null;
            return null;
        }

        public object CallReflected(CodeContext context, CallType callType, object[] args, SymbolId[] names) {
            List<MethodCandidate> targets = FindTarget(callType, args, names);

            if (targets.Count == 1) {
                if (_binder.IsBinaryOperator) {
                    if (!targets[0].CheckArgs(context, args, names)) {
                        return context.LanguageContext.GetNotImplemented(targets[0]);
                    }
                }
                return targets[0].Target.CallReflected(context, args);
            }

            return CallFailed(context, targets, callType, args);
        }

        private object CallFailed(CodeContext context, List<MethodCandidate> targets, CallType callType, object[] args) {
            if (_binder.IsBinaryOperator) {
                return context.LanguageContext.GetNotImplemented(targets.ToArray());
            }

            if (targets.Count == 0) {
                throw NoApplicableTarget(callType, CompilerHelpers.GetTypes(args));
            } else {
                throw MultipleTargets(targets, callType, CompilerHelpers.GetTypes(args));
            }
        }

        private List<MethodCandidate> FindTarget(CallType callType, object[] args, SymbolId[] names) {
            return SelectTargets(callType, CompilerHelpers.GetTypes(args), names);
        }

        private List<MethodCandidate> SelectTargets(CallType callType, Type[] types, SymbolId[] names) {
            // obsolete: this should be removed entirely:
            if (!_binder.StrictParameterCheck) {
                if (_targets.Count == 1 && !_binder.IsBinaryOperator && names.Length == 0) return _targets;
            }

            List<MethodCandidate> applicableTargets = new List<MethodCandidate>();
            foreach (MethodCandidate target in _targets) {
                if (target.IsApplicable(types, names, NarrowingLevel.None)) {
                    applicableTargets.Add(target);
                }
            }

            List<MethodCandidate> result = null;
            if (TryGetApplicableTarget(callType, applicableTargets, types, out result)) {
                return result;
            }

            //no targets are applicable without narrowing conversions, so try those
            foreach (MethodCandidate target in _targets) {
                if (target.IsApplicable(types, names, NarrowingLevel.Preferred)) {
                    applicableTargets.Add(new MethodCandidate(target, NarrowingLevel.Preferred));
                }
            }

            if (TryGetApplicableTarget(callType, applicableTargets, types, out result)) {
                return result;
            }
       
            foreach (MethodCandidate target in _targets) {
                NarrowingLevel nl = _binder.IsBinaryOperator ? NarrowingLevel.Operator : NarrowingLevel.All;
                if (target.IsApplicable(types, names, nl)) {
                    applicableTargets.Add(new MethodCandidate(target, nl));
                }
            }            

            return applicableTargets;
        }

        private bool TryGetApplicableTarget(CallType callType, List<MethodCandidate> applicableTargets, Type[] actualTypes, out List<MethodCandidate> result) {
            result = null;
            if (applicableTargets.Count == 1) {
                result = applicableTargets;
                return true;
            }
            if (applicableTargets.Count > 1) {
                MethodCandidate target = FindBest(callType, applicableTargets, actualTypes);
                if (target != null) {
                    result = new List<MethodCandidate>(new MethodCandidate[] { target });
                    return true;
                } else {
                    result = applicableTargets;
                    return true;
                }
            }
            return false;
        }

        private Type[] GetTypesForTest(Type[] types, IList<MethodCandidate> candidates) {
            // if we have a single target we need no tests.
            // if we have a binary operator we have to test to return NotImplemented
            if (_targets.Count == 1 && !_binder.IsBinaryOperator) return null;

            Type[] tests = new Type[types.Length];
            for (int i = 0; i < types.Length; i++) {
                if (_binder.IsBinaryOperator || AreArgumentTypesOverloaded(types, i, candidates)) {
                    tests[i] = types[i];
                }                
            }
                                  
            return tests;
        }

        private static bool AreArgumentTypesOverloaded(Type[] types, int index, IList<MethodCandidate> methods) {
            Type argType = null;
            for (int i = 0; i < methods.Count; i++) {
                IList<ParameterWrapper> pis = methods[i].Parameters;
                if (pis.Count == 0) continue;

                int readIndex = index;
                if (pis[0].Type == typeof(CodeContext)) {
                    readIndex++;
                }
                
                Type curType;
                if (readIndex < pis.Count) {
                    if (readIndex == -1) {
                        curType = methods[i].Target.Method.DeclaringType;
                    } else if (pis[readIndex].IsParamsArray) {
                        if (index == types.Length - 1) {
                            return true;    // TODO: Optimize this case
                        }
                        curType = pis[pis.Count - 1].Type.GetElementType();
                    } else {
                        curType = pis[readIndex].Type;
                    }
                } else if (pis[pis.Count - 1].IsParamsArray) {
                    curType = pis[pis.Count - 1].Type.GetElementType();
                } else {
                    continue;
                }

                if (argType == null) {
                    argType = curType;
                } else if (argType != curType) {
                    return true;
                }
            }
            return false;
        }

        private static bool IsBest(MethodCandidate candidate, List<MethodCandidate> applicableTargets, CallType callType, Type[] actualTypes) {
            foreach (MethodCandidate target in applicableTargets) {
                if (candidate == target) continue;
                if (candidate.CompareTo(target, callType, actualTypes) != +1) return false;
            }
            return true;
        }

        private static MethodCandidate FindBest(CallType callType, List<MethodCandidate> applicableTargets, Type[] actualTypes) {
            foreach (MethodCandidate candidate in applicableTargets) {
                if (IsBest(candidate, applicableTargets, callType, actualTypes)) return candidate;
            }

            applicableTargets = applicableTargets.FindAll(x => !x.Target.HasParams);

            foreach (MethodCandidate candidate in applicableTargets)
            {
                if (IsBest(candidate, applicableTargets, callType, actualTypes)) return candidate;
            }
            return null;
        }

        private Exception NoApplicableTarget(CallType callType, Type[] types) {
            return new ArgumentTypeException(NoApplicableTargetMessage(callType, types));
        }

        private Exception MultipleTargets(List<MethodCandidate> applicableTargets, CallType callType, Type[] types) {
            return new ArgumentTypeException(MultipleTargetsMessage(applicableTargets, callType, types));
        }

        private string NoApplicableTargetMessage(CallType callType, Type[] types) {
            return TypeErrorForOverloads("no overloads of {0} could match {1}", _targets, callType, types);
        }

        private string MultipleTargetsMessage(List<MethodCandidate> applicableTargets, CallType callType, Type[] types) {
            return TypeErrorForOverloads("multiple overloads of {0} could match {1}", applicableTargets, callType, types);
        }

        private static string GetArgTypeNames(Type[] types, CallType callType) {
            StringBuilder buf = new StringBuilder();
            buf.Append("(");
            bool isFirstArg = true;
            int i = 0;
            if (callType == CallType.ImplicitInstance) i = 1;
            for (; i < types.Length; i++) {
                if (isFirstArg) isFirstArg = false;
                else buf.Append(", ");
                buf.Append(types[i].Name);
            }
            buf.Append(")");
            return buf.ToString();
        }

        private string TypeErrorForOverloads(string message, List<MethodCandidate> targets, CallType callType, Type[] types) {
            StringBuilder buf = new StringBuilder();
            buf.AppendFormat(message, _binder._name, GetArgTypeNames(types, callType));
            buf.AppendLine();
            foreach (MethodCandidate target in targets) {
                buf.Append("  ");
                buf.AppendLine(target.ToSignatureString(_binder._name, callType));
            }
            return buf.ToString();
        }

        public void Add(MethodCandidate target) {
            Debug.Assert(target.Parameters.Count == _count);

            _targets.Add(target);
        }

        public override string ToString() {
            return string.Format("TargetSet({0} on {1}, nargs={2})", _targets[0].Target.Method.Name, _targets[0].Target.Method.DeclaringType.FullName, _count);
        }
    }
}
