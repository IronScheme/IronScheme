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
using System.Collections.ObjectModel;

using System.Reflection;
using System.Reflection.Emit;

using System.Diagnostics;

using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Ast {
    public class MethodCallExpression : Expression {
        private readonly MethodInfo _method;
        private readonly Expression _instance;
        private readonly ReadOnlyCollection<Expression> _arguments;
        private readonly ParameterInfo[] _parameterInfos;

        internal MethodCallExpression(MethodInfo method, Expression instance, IList<Expression> arguments, ParameterInfo[] parameters) {
            _method = method;
            _instance = instance;
            _arguments = new ReadOnlyCollection<Expression>(arguments);
            _parameterInfos = parameters;
        }

        public MethodInfo Method {
            get { return _method; }
        }

        public Expression Instance {
            get { return _instance; }
        }

        public ReadOnlyCollection<Expression> Arguments {
            get { return _arguments; }
        }

        public override Type Type {
            get {
                return _method.ReturnType;
            }
        }

        public object EvaluateArgument(CodeContext context, Expression arg, Type asType) {
            if (arg == null) {
                return null;
            } else {
                // Convert the evaluated argument to the appropriate type to mirror the emit case.
                // C# would try to convert for us, but using the binder preserves the semantics of the target language.
                return context.LanguageContext.Binder.Convert(arg.Evaluate(context), asType);
            }
        }

        protected override object DoEvaluate(CodeContext context) {
            object instance = null;
            // Evaluate the instance first (if the method is non-static)
            if (!Method.IsStatic) {
                instance = EvaluateInstance(context);
            }

            object[] parameters = new object[_parameterInfos.Length];
            if (_parameterInfos.Length > 0) {
                int last = parameters.Length - 1;
                for (int i = 0; i < last; i++) {
                    EvaluateOneArgument(context, parameters, i);
                }
                
                // If the last parameter is a parameter array, throw the extra arguments into an array
                int extraArgs = _arguments.Count - last;
                if (CompilerHelpers.IsParamArray(_parameterInfos[last])) {
                    if (extraArgs == 1 && _arguments[last] != null
                        && _arguments[last].Type == _parameterInfos[last].ParameterType) {
                        // If the last argument is an array, copy it over directly
                        parameters[last] = _arguments[last].Evaluate(context);
                    } else {
                        object[] varargs = new object[_arguments.Count - last];
                        for (int i = last; i < _arguments.Count; i++) {
                            // No need to convert, since the destination type is just Object
                            varargs[i - last] =  _arguments[i].Evaluate(context);
                        }
                        parameters[last] = varargs;
                    }
                } else {
                    EvaluateOneArgument(context, parameters, last);
                }
            }

            try {
                object res;
                try {                    
                    if (Type == typeof(Boolean)) {
                        // Return the singleton True or False object
                        res = RuntimeHelpers.BooleanToObject((bool)_method.Invoke(instance, parameters));
                    } else {
                        res = _method.Invoke(instance, parameters);
                    }
                } finally {
                    // expose by-ref args
                    for (int i = 0; i < _parameterInfos.Length; i++) {
                        if (_parameterInfos[i].ParameterType.IsByRef) {
                            _arguments[i].EvaluateAssign(context, parameters[i]);
                        }
                    }
                }

                // back propagate instance on value types if the instance supports it.
                if (_method.DeclaringType != null && _method.DeclaringType.IsValueType && !_method.IsStatic) {
                    _instance.EvaluateAssign(context, instance);
                }

                return res;
            } catch (TargetInvocationException e) {                
                // Unwrap the real (inner) exception and raise it
                throw ExceptionHelpers.UpdateForRethrow(e.InnerException);
            }
        }

        private void EvaluateOneArgument(CodeContext context, object[] parameters, int i) {
            if (!_parameterInfos[i].IsOut || (_parameterInfos[i].Attributes & ParameterAttributes.In) != 0) {
                if (!_parameterInfos[i].ParameterType.IsByRef) {
                    parameters[i] = EvaluateArgument(context, _arguments[i], _parameterInfos[i].ParameterType);
                } else {
                    parameters[i] = EvaluateArgument(context, _arguments[i], _parameterInfos[i].ParameterType.GetElementType());
                }
            }
        }

        public override void Emit(CodeGen cg) {
            // Emit instance, if calling an instance method
            Slot temp = null;
            if (!_method.IsStatic) {
                EmitInstance(cg);
            }

            // Emit arguments
            if (_parameterInfos.Length > 0) {
                int current = 0;

                // Emit all but the last directly, the last may be param array
                while (current < _parameterInfos.Length - 1) {
                    EmitArgument(cg, _parameterInfos[current], current);
                    current++;
                }

                // Emit the last argument, possible a param array
                ParameterInfo last = _parameterInfos[_parameterInfos.Length - 1];
                if (CompilerHelpers.IsParamArray(last)) {
                    Debug.Assert(last.ParameterType.HasElementType);
                    Type elementType = last.ParameterType.GetElementType();

                    // There are arguments available for emit
                    int size = 0;
                    if (_arguments.Count > _parameterInfos.Length - 1) {
                        size = _arguments.Count - _parameterInfos.Length + 1;
                    }

                    if (size == 1 && _arguments[current].Type == last.ParameterType) {
                        _arguments[current].Emit(cg);
                    } else {
                        cg.EmitInt(size);
                        cg.Emit(OpCodes.Newarr, elementType);
                        for (int i = 0; i < size; i++) {
                            cg.Emit(OpCodes.Dup);
                            cg.EmitInt(i);
                            _arguments[current + i].EmitAs(cg, elementType);
                            cg.EmitStoreElement(elementType);
                        }
                    }
                } else {
                    EmitArgument(cg, _parameterInfos[_parameterInfos.Length - 1], _parameterInfos.Length - 1);
                }
            }

            // Emit the actual call
            cg.EmitCall(_method, tailcall);

            if (temp != null) {
                cg.FreeLocalTmp(temp);
            }
        }

        bool tailcall = false;

        public bool TailCall
        {
          get { return tailcall; }
          set { tailcall = false; }
        }


        private void EmitInstance(CodeGen cg) {
            if (!_method.DeclaringType.IsValueType) {
                _instance.EmitAs(cg, _method.DeclaringType);
            } else {
                _instance.EmitAddress(cg, _method.DeclaringType);
            }
        }

        private object EvaluateInstance(CodeContext context) {
            object res = _instance.Evaluate(context);

            // box "this" if it is a value type (in case _method tries to modify it)
            // -- this keeps the same semantics as Emit().
            if (_method.DeclaringType != null && _method.DeclaringType.IsValueType)
                res = System.Runtime.CompilerServices.RuntimeHelpers.GetObjectValue(res);

            return res;
        }

        private void EmitArgument(CodeGen cg, ParameterInfo param, int index) {
            if (index < _arguments.Count) {
                if (param.ParameterType.IsByRef) {
                    _arguments[index].EmitAddress(cg, param.ParameterType.GetElementType());
                } else {
                    _arguments[index].EmitAs(cg, param.ParameterType);
                }
            } else {
                Debug.Assert(!CompilerHelpers.IsMandatoryParameter(param));
                if (CompilerHelpers.HasDefaultValue(param)) {
                    object defaultValue = param.DefaultValue;
                    cg.EmitConstant(defaultValue);
                    cg.EmitConvert(defaultValue != null ? defaultValue.GetType() : typeof(object), param.ParameterType);
                } else {
                    Debug.Assert(param.IsOptional);
                    cg.EmitMissingValue(param.ParameterType);
                }
            }
        }

        public override void Walk(Walker walker) {
            if (walker.Walk(this)) {
                if (_instance != null) {
                    _instance.Walk(walker);
                }
                if (_arguments != null) {
                    foreach (Expression e in _arguments) {
                        e.Walk(walker);
                    }
                }
            }
            walker.PostWalk(this);
        }
    }

    /// <summary>
    /// Factory methods.
    /// </summary>
    public static partial class Ast {
        public static MethodCallExpression Call(Expression instance, MethodInfo method, params Expression[] arguments) {
            Contract.RequiresNotNull(method, "method");
            Contract.RequiresNotNullItems(arguments, "arguments");
            Contract.Requires(method.IsStatic == (instance == null), "instance", "Cannot call static/instance method with/without an instance.");

            ParameterInfo[] ps = method.GetParameters();
            Contract.Requires(CompilerHelpers.FormalParamsMatchActual(ps, arguments.Length), "method", "The number of parameters doesn't match the number of actual arguments");

            return new MethodCallExpression(method, instance, arguments, ps);
        }
    }
}
