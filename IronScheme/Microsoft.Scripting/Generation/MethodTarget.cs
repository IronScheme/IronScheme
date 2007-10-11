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
using System.Diagnostics;
using System.Reflection;
using System.Reflection.Emit;

using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Actions;

namespace Microsoft.Scripting.Generation {
    using Ast = Microsoft.Scripting.Ast.Ast;
    using Microsoft.Scripting.Utils;

    public class MethodTarget  {
        private MethodBinder _binder;
        private MethodBase _method;
        private int _parameterCount;
        private IList<ArgBuilder> _argBuilders;
        private ArgBuilder _instanceBuilder;
        private ReturnBuilder _returnBuilder;

        public MethodTarget(MethodBinder binder, MethodBase method, int parameterCount, ArgBuilder instanceBuilder, IList<ArgBuilder> argBuilders, ReturnBuilder returnBuilder) {
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


        public object CallReflected(CodeContext context, object[] args) {
            if (ScriptDomainManager.Options.EngineDebug) {
                PerfTrack.NoteEvent(PerfTrack.Categories.Methods, this);
            }

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
#if SILVERLIGHT && DEBUG // TODO: drop when Silverlight gets fixed
            } catch (System.Security.SecurityException) {
                throw new System.Security.SecurityException(String.Format("Access to method '{0}' denied.", 
                    ReflectionUtils.FormatSignature(new System.Text.StringBuilder(), Method)));
#endif
            } catch (TargetInvocationException tie) {
                throw ExceptionHelpers.UpdateForRethrow(tie.InnerException);
            }

            //This is only used to support explicit Reference arguments
            for (int i = 0; i < callArgs.Length; i++) {
                _argBuilders[i].UpdateFromReturn(callArgs[i], args);
            }

            return _returnBuilder.Build(context, callArgs, args, result);
        }

        public Expression MakeExpression(ActionBinder binder, StandardRule rule, Expression[] parameters) {
            MethodBinderContext context = new MethodBinderContext(binder, rule);

            Expression check = Ast.Constant(true);
            if (_binder.IsBinaryOperator) {
                // TODO: only if we have a narrowing level

                // need to emit check to see if args are convertible...
                for (int i = 0; i < _argBuilders.Count; i++) {
                    Expression checkedExpr = _argBuilders[i].CheckExpression(context, parameters);
                    if(checkedExpr != null) {
                        check = Ast.AndAlso(check, checkedExpr);
                    }
                }
            }

            Expression[] args = new Expression[_argBuilders.Count];
            for (int i = 0; i < _argBuilders.Count; i++) {
                args[i] = _argBuilders[i].ToExpression(context, parameters);
            }

            MethodInfo mi = Method as MethodInfo;
            Expression ret, call;
            if (Method.IsPublic && Method.DeclaringType.IsVisible) {
                // public method
                if (mi != null) {
                    Expression instance = mi.IsStatic ? null : _instanceBuilder.ToExpression(context, parameters);
                    call = Ast.Call(instance, mi, args);
                } else {
                    call = Ast.New((ConstructorInfo)Method, args);
                }
            } else {
                // Private binding, invoke via reflection
                if (mi != null) {
                    Expression instance = mi.IsStatic ? null : _instanceBuilder.ToExpression(context, parameters);
                    call = Ast.Call(
                        Ast.RuntimeConstant(mi),
                        typeof(MethodInfo).GetMethod("Invoke", new Type[] { typeof(object), typeof(object[]) }),
                        instance,
                        Ast.NewArray(typeof(object[]), args)
                    );
                } else {
                    call = Ast.Call(
                        Ast.RuntimeConstant((ConstructorInfo)Method),
                        typeof(ConstructorInfo).GetMethod("Invoke", new Type[] { typeof(object[]) }), 
                        Ast.NewArray(typeof(object[]), args)
                    ); 
                }
            }

            ret = _returnBuilder.ToExpression(context, _argBuilders, parameters, call);

            List<Expression> updates = null;
            for (int i = 0; i < _argBuilders.Count; i++) {                
                Expression next = _argBuilders[i].UpdateFromReturn(context, parameters);
                if (next != null) {
                    if (updates == null) updates = new List<Expression>();
                    updates.Add(next);
                }
            }

            if (updates != null) {
                updates.Insert(0, ret);
                ret = Ast.Comma(0, updates.ToArray());
            }

            if (!check.IsConstant(true)) {
                ret = Ast.Condition(check, ret, GetNotImplemented());
            }
            return ret;
        }

        private static MethodCallExpression GetNotImplemented() {
            return Ast.Call(Ast.ReadProperty(Ast.CodeContext(), typeof(CodeContext), "LanguageContext"), typeof(LanguageContext).GetMethod("GetNotImplemented"), Ast.NewArray(typeof(MethodCandidate[])));
        }

        /// <summary>
        /// Creates a call to this MethodTarget with the specified parameters.  Casts are inserted to force
        /// the types to the provided known types.
        /// </summary>
        /// <param name="binder"></param>
        /// <param name="rule"></param>
        /// <param name="parameters"></param>
        /// <param name="knownTypes"></param>
        /// <returns></returns>
        public Expression MakeExpression(ActionBinder binder, StandardRule rule, Expression[] parameters, Type[] knownTypes) {
            Expression[] args = parameters;
            if (knownTypes != null) {
                args = new Expression[parameters.Length];
                for (int i = 0; i < args.Length; i++) {
                    args[i] = parameters[i];
                    if (knownTypes[i] != null && !knownTypes[i].IsAssignableFrom(parameters[i].Type)) {
                        args[i] = Ast.Convert(parameters[i], CompilerHelpers.GetVisibleType(knownTypes[i]));
                    }
                }
            }

            return MakeExpression(binder, rule, args);
        }

        public AbstractValue AbstractCall(AbstractContext context, IList<AbstractValue> args) {
            AbstractValue[] callArgs = new AbstractValue[_argBuilders.Count];
            for (int i = 0; i < _argBuilders.Count; i++) {
                callArgs[i] = _argBuilders[i].AbstractBuild(context, args);
            }

            Expression[] argExprs = new Expression[callArgs.Length];
            for (int i = 0; i < callArgs.Length; i++) {
                Expression expr = callArgs[i].Expression;
                if (expr == null) {
                    argExprs = null;
                    break;
                } else {
                    argExprs[i] = expr;
                }
            }

            Expression callExpr = null;
            if (argExprs != null) {
                MethodInfo mi = Method as MethodInfo;
                if (mi != null) {
                    Expression instance = mi.IsStatic ? null : _instanceBuilder.AbstractBuild(context, args).Expression;
                    callExpr = Ast.Call(instance, mi, argExprs);
                } else {
                    callExpr = Ast.New((ConstructorInfo)Method, argExprs);
                }
            }

            return AbstractValue.LimitType(this.ReturnType, callExpr);
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

            if (kwIndex != -1) {
                newArgBuilders.Insert(kwIndex, new ParamsDictArgBuilder(curArg, names, nameIndexes));
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
