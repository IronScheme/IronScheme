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
using System.Text;
using System.Collections.Generic;

using Microsoft.Scripting.Actions;
using Microsoft.Scripting.Generation;
using System.Diagnostics;

namespace Microsoft.Scripting {
    public class MethodCandidate {
        private MethodTarget _target;
        private List<ParameterWrapper> _parameters;
        private NarrowingLevel _narrowingLevel;

        public MethodCandidate(MethodCandidate previous, NarrowingLevel narrowingLevel) {
            this._target = previous.Target;
            this._parameters = previous._parameters;
            _narrowingLevel = narrowingLevel;
        }

        internal MethodCandidate(MethodTarget target, List<ParameterWrapper> parameters) {
            Debug.Assert(target != null);

            _target = target;
            _parameters = parameters;
            _narrowingLevel = NarrowingLevel.None;
            parameters.TrimExcess();
        }

        public MethodTarget Target {
            get { return _target; }
        }

        public bool IsApplicable(Type[] types, SymbolId[] names, NarrowingLevel allowNarrowing)
        {
          if (!TryGetNormalizedArguments(types, names, out types)) {
                return false;
            }

            return IsApplicable(types, allowNarrowing);
        }

        public bool IsApplicable(Type[] types, NarrowingLevel allowNarrowing) {
            Debug.Assert(types.Length == _parameters.Count);

            for (int i = 0; i < types.Length; i++) {
                if (!_parameters[i].HasConversionFrom(types[i], allowNarrowing)) {
                    return false;
                }
            }            

            return true;
        }

        public bool CheckArgs(CodeContext context, object[] args, SymbolId[] names) {
            Type [] newArgs;
            if (!TryGetNormalizedArguments(CompilerHelpers.GetTypes(args), names, out newArgs)) {
                return false;
            }

            if (IsApplicable(newArgs, NarrowingLevel.None)) {
                return true;
            }

            object []objArgs;
            TryGetNormalizedArguments(args, names, out objArgs);

            return Target.CheckArgs(context, objArgs);
        }

        private bool TryGetNormalizedArguments<T>(T[] argTypes, SymbolId[] names, out T[] args) {
            if (names.Length == 0) {
                args = argTypes;
                return true;
            }

            T[] res = new T[argTypes.Length];
            Array.Copy(argTypes, res, argTypes.Length - names.Length);

            for (int i = 0; i < names.Length; i++) {
                bool found = false;
                for (int j = 0; j < _parameters.Count; j++) {
                    if (_parameters[j].Name == names[i]) {
                        if (res[j] != null) {
                            args = null;
                            return false;
                        }

                        res[j] = argTypes[i + argTypes.Length - names.Length];

                        found = true;
                        break;
                    }
                }

                if (!found) {
                    args = null;
                    return false;
                }
            }
            
            args = res;
            return true;
        }

        public int? CompareParameters(MethodCandidate other, Type[] actualTypes) {
            return ParameterWrapper.CompareParameters(this._parameters, other._parameters, actualTypes);
        }

        public int CompareTo(MethodCandidate other, CallType callType, Type[] actualTypes) {
            int? cmpParams = CompareParameters(other, actualTypes);
            if (cmpParams == +1 || cmpParams == -1) return (int)cmpParams;

            int ret = Target.CompareEqualParameters(other.Target);
            if (ret != 0) return ret;

            if (CompilerHelpers.IsStatic(Target.Method) && !CompilerHelpers.IsStatic(other.Target.Method)) {
                return callType == CallType.ImplicitInstance ? -1 : +1;
            } else if (!CompilerHelpers.IsStatic(Target.Method) && CompilerHelpers.IsStatic(other.Target.Method)) {
                return callType == CallType.ImplicitInstance ? +1 : -1;
            }

            return 0;
        }

        /// <summary>
        /// Builds a new MethodCandidate which takes count arguments and the provided list of keyword arguments.
        /// 
        /// The basic idea here is to figure out which parameters map to params or a dictionary params and
        /// fill in those spots w/ extra ParameterWrapper's.  
        /// </summary>
        public MethodCandidate MakeParamsExtended(ActionBinder binder, int count, SymbolId[] names) {
            List<ParameterWrapper> newParameters = new List<ParameterWrapper>(count);
            // if we don't have a param array we'll have a param dict which is type object
            Type elementType = null;  
            int index = -1, kwIndex = -1;

            // keep track of which kw args map to a real argument, and which ones
            // map to the params dictionary.
            List<SymbolId> unusedNames = new List<SymbolId>(names);
            List<int> unusedNameIndexes = new List<int>();
            for (int i = 0; i < unusedNames.Count; i++) {
                unusedNameIndexes.Add(i);
            }

            for (int i = 0; i < _parameters.Count; i++) {
                ParameterWrapper pw = _parameters[i];

                if (_parameters[i].IsParamsArray)
                {
                    elementType = pw.Type.GetElementType();
                    index = i;
                } else {
                    for (int j = 0; j < unusedNames.Count; j++) {
                        if (unusedNames[j] == _parameters[i].Name) {
                            unusedNames.RemoveAt(j);
                            unusedNameIndexes.RemoveAt(j);
                            break;
                        }
                    }
                    newParameters.Add(pw);
                }
            }

            if (index != -1) {
                while (newParameters.Count < (count - unusedNames.Count)) {
                    ParameterWrapper param = new ParameterWrapper(binder, elementType);
                    newParameters.Insert(System.Math.Min(index, newParameters.Count), param);
                }
            }

            if (kwIndex != -1) {
                foreach (SymbolId si in unusedNames) {
                    ParameterWrapper pw = new ParameterWrapper(binder, typeof(object), si);
                    newParameters.Add(pw);
                }
            } else if (unusedNames.Count != 0) {
                // unbound kw args and no where to put them, can't call...
                return null;
            }

            // if we have too many or too few args we also can't call
            if(count != newParameters.Count) return null;

            return new MethodCandidate(_target.MakeParamsExtended(count, unusedNames.ToArray(), unusedNameIndexes.ToArray()), newParameters);
        }

        public override string ToString() {
            return string.Format("MethodCandidate({0})", Target);
        }

        public string ToSignatureString(string name, CallType callType) {
            StringBuilder buf = new StringBuilder(name);
            buf.Append("(");
            bool isFirstArg = true;
            int i = 0;
            if (callType == CallType.ImplicitInstance) i = 1;
            for (; i < _parameters.Count; i++) {
                if (isFirstArg) isFirstArg = false;
                else buf.Append(", ");
                buf.Append(_parameters[i].ToSignatureString());
            }
            buf.Append(")");
            return buf.ToString(); //@todo add helper info for more interesting signatures
        }

        public NarrowingLevel NarrowingLevel {
            get {
                return _narrowingLevel;
            }
        }

        internal IList<ParameterWrapper> Parameters {
            get {
                return _parameters;
            }
        }
    }

}
