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
        private ReflectedCaller _caller;

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

        private object EvaluateInstance(CodeContext context) {
            object res = _instance.Evaluate(context);

            // box "this" if it is a value type (in case _method tries to modify it)
            // -- this keeps the same semantics as Emit().
            if (_method.DeclaringType != null && _method.DeclaringType.IsValueType) {
                res = System.Runtime.CompilerServices.RuntimeHelpers.GetObjectValue(res);
            }
            return res;
        }

        protected override object DoEvaluate(CodeContext context) {
            object instance = null;
            // Evaluate the instance first (if the method is non-static)
            if (!Method.IsStatic) {
                instance = EvaluateInstance(context);
            }

            object[] parameters = new object[_parameterInfos.Length];
            EvaluationAddress[] paramAddrs = new EvaluationAddress[_parameterInfos.Length];
            if (_parameterInfos.Length > 0) {
                int last = parameters.Length;
                for (int i = 0; i < last; i++) {
                    ParameterInfo pi = _parameterInfos[i];

                    if (pi.ParameterType.IsByRef) {
                        paramAddrs[i] = _arguments[i].EvaluateAddress(context);

                        object value = paramAddrs[i].GetValue(context, !IsInputParameter(i));
                        if (IsInputParameter(i)) {
                            parameters[i] = context.LanguageContext.Binder.Convert(
                                value,
                                _parameterInfos[i].ParameterType.GetElementType()
                            );
                        }
                    } else if (IsInputParameter(i)) {
                        Expression arg = _arguments[i];
                        parameters[i] = arg != null ? arg.Evaluate(context) : null;
                    }
                }
            }

            try {
                object res;
                try {
                    // Call the method
                    res = InvokeMethod(instance, parameters);

                    // Return the singleton True or False object
                    if (Type == typeof(Boolean)) {
                        res = RuntimeHelpers.BooleanToObject((bool)res);
                    }
                } finally {
                    // expose by-ref args
                    for (int i = 0; i < _parameterInfos.Length; i++) {
                        if (_parameterInfos[i].ParameterType.IsByRef) {
                            paramAddrs[i].AssignValue(context, parameters[i]);
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

        private bool IsInputParameter(int i) {
            return !_parameterInfos[i].IsOut || (_parameterInfos[i].Attributes & ParameterAttributes.In) != 0;
        }

        private object InvokeMethod(object instance, object[] parameters) {
            if (_caller == null) {
                _caller = ReflectedCaller.Create(_method);
            }
            if (instance == null) {
                return _caller.Invoke(parameters);
            } else {
                return _caller.InvokeInstance(instance, parameters);
            }

        }

        public override void Emit(CodeGen cg) {
            // Emit instance, if calling an instance method
            if (!_method.IsStatic) {
                Type type = _method.DeclaringType;

                if (type.IsValueType) {
                    _instance.EmitAddress(cg, type);
                } else {
                    _instance.Emit(cg);
                }
            }

            // Emit arguments
            Debug.Assert(_arguments.Count == _parameterInfos.Length);
            for (int arg = 0; arg < _parameterInfos.Length; arg++) {
                Expression argument = _arguments[arg];
                Type type = _parameterInfos[arg].ParameterType;
                EmitArgument(cg, argument, type);
            }

            // Emit the actual call
            cg.EmitCall(_method, tailcall);
        }

        bool tailcall = false;

        public bool TailCall
        {
          get { return tailcall; }
          set { tailcall = value; }
        }

        private static void EmitArgument(CodeGen cg, Expression argument, Type type) {
            if (type.IsByRef) {
                argument.EmitAddress(cg, type.GetElementType());
            } else {
                argument.Emit(cg);
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
        public static MethodCallExpression Call(MethodInfo method, params Expression[] arguments) {
            return Call(null, method, arguments);
        }

        public static MethodCallExpression Call(Expression instance, MethodInfo method, params Expression[] arguments) {
            Contract.RequiresNotNull(method, "method");
            Contract.Requires(!method.IsGenericMethodDefinition, "method");
            Contract.Requires(!method.ContainsGenericParameters, "method");
            if (method.IsStatic) {
                Contract.Requires(instance == null, "instance", "Instance must be null for static method");
            } else {
                Contract.RequiresNotNull(instance, "instance");
                if (!TypeUtils.CanAssign(method.DeclaringType, instance.Type)) {
                    throw new ArgumentException(
                        String.Format(
                             "Invalid instance type for {0}.{1}. Expected {0}, got {2}.",
                             method.DeclaringType.Name,
                             method.Name,
                             instance.Type.Name
                        ),
                        "instance"
                    );
                }
            }

            Contract.RequiresNotNullItems(arguments, "arguments");
            ParameterInfo[] parameters = method.GetParameters();

            Contract.Requires(parameters.Length == arguments.Length);
            for (int index = 0; index < parameters.Length; index++) {
                Type pt = parameters[index].ParameterType;
                Contract.Requires(!TypeUtils.IsGeneric(pt), "arguments");
                if (pt.IsByRef) {
                    pt = pt.GetElementType();
                }
                if (!TypeUtils.CanAssign(pt, arguments[index].Type)) {
                    throw new ArgumentException(
                        String.Format(
                            "Invalid type for argument {0}. Expected {1}, got {2}.",
                            index, pt.Name, arguments[index].Type.Name
                        ),
                        "arguments"
                    );
                }
            }

            return new MethodCallExpression(method, instance, arguments, parameters);
        }

        /// <summary>
        /// The helper to create the AST method call node. Will add conversions (Ast.Convert())
        /// to parameters and instance if necessary.
        /// </summary>
        public static MethodCallExpression SimpleCallHelper(MethodInfo method, params Expression[] arguments) {
            Contract.RequiresNotNull(method, "method");
            Contract.Requires(method.IsStatic, "method", "Method must be static");
            return SimpleCallHelper(null, method, arguments);
        }

        /// <summary>
        /// The helper to create the AST method call node. Will add conversions (Ast.Convert())
        /// to parameters and instance if necessary.
        /// </summary>
        public static MethodCallExpression SimpleCallHelper(Expression instance, MethodInfo method, params Expression[] arguments) {
            Contract.RequiresNotNull(method, "method");
            Contract.Requires(instance != null ^ method.IsStatic, "instance");
            Contract.RequiresNotNullItems(arguments, "arguments");

            ParameterInfo[] parameters = method.GetParameters();

            Contract.Requires(arguments.Length == parameters.Length, "arguments", "Incorrect number of arguments");

            if (instance != null) {
                instance = ConvertHelper(instance, method.DeclaringType);
            }

            Expression[] clone = null;
            for (int arg = 0; arg < arguments.Length; arg++) {
                Expression argument = arguments[arg];
                if (!CompatibleParameterTypes(parameters[arg].ParameterType, argument.Type)) {
                    // Clone the arguments array if needed
                    if (clone == null) {
                        clone = new Expression[arguments.Length];
                        // Copy the expressions into the clone
                        for (int i = 0; i < arg; i++) {
                            clone[i] = arguments[i];
                        }
                    }

                    argument = ArgumentConvertHelper(argument, parameters[arg].ParameterType);
                }

                if (clone != null) {
                    clone[arg] = argument;
                }
            }

            return Call(instance, method, clone != null ? clone : arguments);
        }

        private static Expression ArgumentConvertHelper(Expression argument, Type type) {
            if (argument.Type != type) {
                if (type.IsByRef) {
                    type = type.GetElementType();
                }
                if (argument.Type != type) {
                    argument = Convert(argument, type);
                }
            }
            return argument;
        }

        private static bool CompatibleParameterTypes(Type parameter, Type argument) {
            if (parameter == argument) {
                return true;
            }
            if (parameter.IsByRef && parameter.GetElementType() == argument) {
                return true;
            }
            return false;
        }

        /// <summary>
        /// The complex call helper to create the AST method call node.
        /// Will add conversions (Ast.Convert()), deals with default parameter values and params arrays.
        /// </summary>
        public static Expression ComplexCallHelper(MethodInfo method, params Expression[] arguments) {
            Contract.RequiresNotNull(method, "method");
            Contract.Requires(method.IsStatic, "method", "Method must be static");
            return ComplexCallHelper(null, method, arguments);
        }

        /// <summary>
        /// The complex call helper to create the AST method call node.
        /// Will add conversions (Ast.Convert()), deals with default parameter values and params arrays.
        /// </summary>
        public static Expression ComplexCallHelper(Expression instance, MethodInfo method, params Expression[] arguments) {
            Contract.RequiresNotNull(method, "method");
            Contract.RequiresNotNullItems(arguments, "arguments");
            Contract.Requires(instance != null ^ method.IsStatic, "instance");

            if (instance != null) {
                instance = ConvertHelper(instance, method.DeclaringType);
            }

            ParameterInfo[] parameters = method.GetParameters();
            Expression[] clone = null;

            int current = 0;    // current parameter being populated
            int consumed = 0;   // arguments so far consumed

            // Validate the argument array, or populate the clone
            while (current < parameters.Length) {
                ParameterInfo parameter = parameters[current];
                Expression argument;

                // last parameter ... params array?
                if ((current == parameters.Length - 1) && (CompilerHelpers.IsParamArray(parameter))) {
                    // do we have any arguments to pass in?
                    if (consumed < arguments.Length) {
                        // Exactly one argument? If it is array of the right type, it goes directly
                        if ((consumed == arguments.Length - 1) &&
                            CompatibleParameterTypes(parameter.ParameterType, arguments[consumed].Type)) {
                            argument = arguments[consumed++];
                        } else {
                            Type elementType = parameter.ParameterType.GetElementType();
                            Expression[] paramArray = new Expression[arguments.Length - consumed];
                            int paramIndex = 0;
                            while (consumed < arguments.Length) {
                                paramArray[paramIndex++] = ConvertHelper(arguments[consumed++], elementType);
                            }
                            argument = NewArray(parameter.ParameterType, paramArray);
                        }
                    } else {
                        // No. Create an empty array.
                        argument = NewArray(parameter.ParameterType);
                    }
                } else {
                    if (consumed < arguments.Length) {
                        // We have argument.
                        argument = arguments[consumed++];
                    } else {
                        // Missing argument, try default value.
                        Contract.Requires(!CompilerHelpers.IsMandatoryParameter(parameter), "arguments", "Argument not provided for a mandatory parameter");
                        argument = CreateDefaultValueExpression(parameter);
                    }
                }

                // Add conversion if needed
                argument = ArgumentConvertHelper(argument, parameter.ParameterType);

                // Do we need to make array clone?
                if (clone == null && !(current < arguments.Length && (object)argument == (object)arguments[current])) {
                    clone = new Expression[parameters.Length];
                    for (int i = 0; i < current; i++) {
                        clone[i] = arguments[i];
                    }
                }

                if (clone != null) {
                    clone[current] = argument;
                }

                // Next parameter
                current++;
            }
            Contract.Requires(consumed == arguments.Length, "arguments", "Incorrect number of arguments");
            return Call(instance, method, clone != null ? clone : arguments);
        }

        private static Expression CreateDefaultValueExpression(ParameterInfo parameter) {
            if (CompilerHelpers.HasDefaultValue(parameter)) {
                return Constant(parameter.DefaultValue, parameter.ParameterType);
            } else {
                // TODO: Handle via compiler constant.
                throw new NotSupportedException("missing parameter value not yet supported");
            }
        }
    }
}
