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

using Microsoft.Scripting.Ast;

namespace Microsoft.Scripting.Generation
{
    using Ast = Microsoft.Scripting.Ast.Ast;
    using Microsoft.Scripting.Generation.Builders;

    public class MethodTarget  {
        private MethodBinder _binder;
        private MethodBase _method;
        private int _parameterCount;
        private IList<ArgBuilder> _argBuilders;
        private ArgBuilder _instanceBuilder;
        private ReturnBuilder _returnBuilder;

        internal MethodTarget(MethodBinder binder, MethodBase method, int parameterCount, ArgBuilder instanceBuilder, IList<ArgBuilder> argBuilders, ReturnBuilder returnBuilder) {
            this._binder = binder;
            this._method = method;
            this._parameterCount = parameterCount;
            this._instanceBuilder = instanceBuilder;
            this._argBuilders = argBuilders;
            this._returnBuilder = returnBuilder;

            //argBuilders.TrimExcess();
        }

        public MethodBase Method {
            get { return _method; }
            set { _method = value; }
        }
        
        public int ParameterCount {
            get { return _parameterCount; }
        }

        public bool HasParams
        {
            get { return _argBuilders.Count > _parameterCount && _argBuilders[_argBuilders.Count - 1] is ParamsArgBuilder; }
        }

      public bool NeedsContext
      {
        get { return _argBuilders.Count > 0 && _argBuilders[0] is ContextArgBuilder; }
      }

        public bool CheckArgs(CodeContext context, object[] args) {
            //if (!instanceBuilder.Check(context, args)) return false;
            //foreach (ArgBuilder arg in argBuilders) {
            //    if (!arg.Check(context, args)) return false;
            //}
            //return true;
            try {
                _instanceBuilder.Build(context, args);
                for (int i = 0; i < _argBuilders.Count; i++) {
                    _argBuilders[i].Build(context, args);
                }
                return true;
            } catch (OverflowException) {
                return false;
            } catch (ArgumentTypeException) {
                return false;
            }
        }


        public object CallReflected(CodeContext context, object[] args)
        {
          object instance = _instanceBuilder.Build(context, args);
            object[] callArgs = new object[_argBuilders.Count];
            for (int i = 0; i < callArgs.Length; i++) {
                callArgs[i] = _argBuilders[i].Build(context, args);
            }

            object result;
            try {
                if (Method is ConstructorInfo) {
                    result = ((ConstructorInfo)Method).Invoke(callArgs);
                } else {
                    result = Method.Invoke(instance, callArgs);
                }
            } catch (TargetInvocationException tie) {
                throw tie.InnerException;
            }

            //This is only used to support explicit Reference arguments
            for (int i = 0; i < callArgs.Length; i++) {
                _argBuilders[i].UpdateFromReturn(callArgs[i], args);
            }

            return _returnBuilder.Build(context, callArgs, args, result);
        }

        private static int FindMaxPriority(IList<ArgBuilder> abs, int ceiling) {
            int max = 0;
            foreach (ArgBuilder ab in abs) {
                if (ab.Priority > ceiling) continue;

                max = System.Math.Max(max, ab.Priority);
            }
            return max;
        }

        public int CompareEqualParameters(MethodTarget other) {
            // Prefer normal methods over explicit interface implementations
            if (other.Method.IsPrivate && !this.Method.IsPrivate) return +1;
            if (this.Method.IsPrivate && !other.Method.IsPrivate) return -1;

            // Prefer non-generic methods over generic methods
            if (Method.IsGenericMethod) {
                if (!other.Method.IsGenericMethod) {
                    return -1;
                } else {
                    //!!! Need to support selecting least generic method here
                    return 0;
                }
            } else if (other.Method.IsGenericMethod) {
                return +1;
            }

            //prefer methods without out params over those with them
            switch (Compare(_returnBuilder.CountOutParams, other._returnBuilder.CountOutParams)) {
                case 1: return -1;
                case -1: return 1;
            }

            //prefer methods using earlier conversions rules to later ones            
            for (int i = Int32.MaxValue; i >= 0; ) {
                int maxPriorityThis = FindMaxPriority(this._argBuilders, i);
                int maxPriorityOther = FindMaxPriority(other._argBuilders, i);

                if (maxPriorityThis < maxPriorityOther) return +1;
                if (maxPriorityOther < maxPriorityThis) return -1;

                i = maxPriorityThis - 1;
            }

            return 0;
        }

        protected static int Compare(int x, int y) {
            if (x < y) return -1;
            else if (x > y) return +1;
            else return 0;
        }

        public override string ToString() {
            return string.Format("MethodTarget({0} on {1})", Method, Method.DeclaringType.FullName);
        }

        public Type ReturnType {
            get {
                return _returnBuilder.ReturnType;
            }
        }

        public MethodTarget MakeParamsExtended(int argCount, SymbolId[] names, int[] nameIndexes) {
            Debug.Assert(CompilerHelpers.IsParamsMethod(Method));

            List<ArgBuilder> newArgBuilders = new List<ArgBuilder>(_argBuilders.Count);
            
            // current argument that we consume, initially skip this if we have it.
            int curArg = CompilerHelpers.IsStatic(_method) ? 0 : 1;
            int kwIndex = -1;

            foreach (ArgBuilder ab in _argBuilders) {
                SimpleArgBuilder sab = ab as SimpleArgBuilder;
                if (sab != null) {
                    // we consume one or more incoming argument(s)
                    if (sab.IsParamsArray) {
                        // consume all the extra arguments
                        int paramsUsed = argCount -
                            GetConsumedArguments() - 
                            names.Length +
                            (CompilerHelpers.IsStatic(_method) ? 1 : 0);

                        newArgBuilders.Add(new ParamsArgBuilder(
                            curArg,
                            paramsUsed,
                            sab.Type.GetElementType()));

                        curArg += paramsUsed;
                    } else if (sab.IsParamsDict) {
                        // consume all the kw arguments
                        kwIndex = newArgBuilders.Count;
                    } else {
                        // consume the next argument
                        newArgBuilders.Add(new SimpleArgBuilder(curArg++, sab.Type));
                    }
                } else {
                    // CodeContext, null, default, etc...  we don't consume an 
                    // actual incoming argument.
                    newArgBuilders.Add(ab);
                }
            }

            return new MethodTarget(_binder, Method, argCount, _instanceBuilder, newArgBuilders, _returnBuilder);
        }

        private int GetConsumedArguments() {
            int consuming = 0;
            foreach (ArgBuilder argb in _argBuilders) {
                SimpleArgBuilder sab = argb as SimpleArgBuilder;
                if (sab != null && !sab.IsParamsDict) consuming++;
            }
            return consuming;
        }
    }
}
