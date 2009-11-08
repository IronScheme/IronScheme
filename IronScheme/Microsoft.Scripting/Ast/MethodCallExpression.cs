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
using System.Collections.ObjectModel;

using System.Reflection;
using System.Reflection.Emit;

using System.Diagnostics;

using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Ast {
    public class MethodCallExpression : Expression {
        private MethodInfo _method;
        private Expression _instance;
        private readonly ReadOnlyCollection<Expression> _arguments;
        private readonly ParameterInfo[] _parameterInfos;


#if FULL
        private ReflectedCaller _caller; 
#endif


        internal MethodCallExpression(MethodInfo /*!*/ method, Expression instance, ReadOnlyCollection<Expression> /*!*/ arguments, ParameterInfo[] /*!*/ parameters)
            : base(AstNodeType.Call) {
            _method = method;
            _instance = instance;
            _arguments = arguments;
            _parameterInfos = parameters;
        }

        public MethodInfo Method {
            get { return _method; }
          set { _method = value; }
        }

        public Expression Instance {
            get { return _instance; }
          set { _instance = value; }
        }

        public ReadOnlyCollection<Expression> Arguments {
            get { return _arguments; }
        }

        public override Type Type {
            get {
                return _method.ReturnType;
            }
        }


#if FULL
        private object EvaluateInstance(CodeContext context) {
            object res = _instance.Evaluate(context);

            // box "this" if it is a value type (in case _method tries to modify it)
            // -- this keeps the same semantics as Emit().
            if (_method.DeclaringType != null && _method.DeclaringType.IsValueType) {
                res = System.Runtime.CompilerServices.RuntimeHelpers.GetObjectValue(res);
            }
            return res;
        } 
#endif



#if FULL
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
#endif


        private bool IsInputParameter(int i) {
            return !_parameterInfos[i].IsOut || (_parameterInfos[i].Attributes & ParameterAttributes.In) != 0;
        }


#if FULL
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
#endif
        static Expression UnwindBoundExpression(BoundExpression be)
        {
          if (be.Variable.AssumedValue != null && be.Variable.AssumedValue is BoundExpression)
          {
            return UnwindBoundExpression(be.Variable.AssumedValue as BoundExpression);
          }
          return be.Variable.AssumedValue ?? be;
        }


        public override void Emit(CodeGen cg) {
          //EmitLocation(cg);
          if (_instance != null && !cg.IsDynamicMethod && !IsParamsMethod()) // damn DM! // go away! // this dangerous too for now
          {
            if (_instance is UnaryExpression)
            {
              UnaryExpression ue = (UnaryExpression)_instance;
              if (typeof(Delegate).IsAssignableFrom(ue.Type))
              {
                if (ue.Operand is CodeBlockExpression)
                {

                  CodeBlockExpression cbe = (CodeBlockExpression)ue.Operand;
                  Debug.Assert(_arguments.Count == _parameterInfos.Length);

                  CodeGen rcg;

                  if (tailcall && CodeGen._codeBlockImplementations.TryGetValue(cbe.Block, out rcg))
                  {
                    if (rcg == cg && !ScriptDomainManager.Options.DebugMode && HasNoCallableArgs())
                    {
                      List<Variable> pars = new List<Variable>(cbe.Block.Parameters);
                      for (int arg = 0; arg < _parameterInfos.Length; arg++)
                      {
                        Expression argument = _arguments[arg];
                        if (argument is BoundExpression)
                        {
                          var abe = argument as BoundExpression;
                          if (abe.Variable == pars[arg])
                          {
                            pars[arg] = null;
                            continue;
                          }
                        }
                        Type type = _parameterInfos[arg].ParameterType;
                        EmitArgument(cg, argument, type);
                      }

                      EmitLocation(cg);

                      for (int arg = 0; arg < _parameterInfos.Length; arg++)
                      {
                        if (pars[_parameterInfos.Length - arg - 1] != null)
                        {
                          cg.Emit(OpCodes.Starg_S, _parameterInfos.Length - arg - 1);
                        }
                      }

                      cg.Emit(OpCodes.Br, cg.startpoint);

                      cg.skipreturn = true;
                      return;
                    }
                  }

                  for (int arg = 0; arg < _parameterInfos.Length; arg++)
                  {
                    Expression argument = _arguments[arg];
                    Type type = _parameterInfos[arg].ParameterType;
                    EmitArgument(cg, argument, type);
                  }

                  EmitLocation(cg);

                  cbe.EmitDirect(cg, tailcall);
                  return;
                }
                else
                {
                  ;
                }
              }
              else if (ue.Operand is BoundExpression && pt != null)
              {
                BoundExpression be = ue.Operand as BoundExpression;
                var v = UnwindBoundExpression(be);
                if (v is MethodCallExpression)
                {
                  var mce = v as MethodCallExpression;
                  if (mce.Arguments.Count == 2)
                  {
                    var cbe = mce.Arguments[1] as CodeBlockExpression;
                    if (cbe != null)
                    {
                      CodeGen rcg = null;

                      if (tailcall && CodeGen._codeBlockImplementations.TryGetValue(cbe.Block, out rcg))
                      {
                        if (rcg == cg && !ScriptDomainManager.Options.DebugMode && HasNoCallableArgs())
                        {
                          List<Variable> pars = new List<Variable>(cbe.Block.Parameters);
                          for (int arg = 0; arg < _parameterInfos.Length; arg++)
                          {
                            Expression argument = _arguments[arg];
                            if (argument is BoundExpression)
                            {
                              var abe = argument as BoundExpression;
                              if (abe.Variable == pars[arg])
                              {
                                pars[arg] = null;
                                continue;
                              }
                            }
                            Type type = _parameterInfos[arg].ParameterType;
                            EmitArgument(cg, argument, type);
                          }

                          var ptt = pt.GetValue(cg.MethodInfo) as Type[];

                          int adjust = ptt.Length - 1 - _parameterInfos.Length;

                          EmitLocation(cg);

                          for (int arg = 0; arg < _parameterInfos.Length; arg++)
                          {
                            if (pars[_parameterInfos.Length - arg - 1] != null)
                            {
                              cg.Emit(OpCodes.Starg_S, _parameterInfos.Length - arg + adjust);
                            }
                          }

                          cg.Emit(OpCodes.Br, cg.startpoint);

                          cg.skipreturn = true;
                          return;
                        }
                      }

                      if (rcg != null)
                      {
                        //this wont work, need to look for another way
                        //rcg.ContextSlot.EmitGet(cg);

                        //for (int arg = 0; arg < _parameterInfos.Length; arg++)
                        //{
                        //  Expression argument = _arguments[arg];
                        //  Type type = _parameterInfos[arg].ParameterType;
                        //  EmitArgument(cg, argument, type);
                        //}

                        //cbe.EmitDirect(cg, tailcall);
                        //return;
                      }
                    }

                  }
                }
              }
              else
              {
                ;
              }
            }
            else if (_instance is CodeBlockExpression)
            {
              CodeBlockExpression cbe = (CodeBlockExpression)_instance;

              CodeGen rcg = null;

              if (tailcall && CodeGen._codeBlockImplementations.TryGetValue(cbe.Block, out rcg))
              {
                if (rcg == cg && !ScriptDomainManager.Options.DebugMode && HasNoCallableArgs())
                {
                  List<Variable> pars = new List<Variable>(cbe.Block.Parameters);
                  for (int arg = 0; arg < _parameterInfos.Length; arg++)
                  {
                    Expression argument = _arguments[arg];
                    if (argument is BoundExpression)
                    {
                      var abe = argument as BoundExpression;
                      if (abe.Variable == pars[arg])
                      {
                        pars[arg] = null;
                        continue;
                      }
                    }
                    Type type = _parameterInfos[arg].ParameterType;
                    EmitArgument(cg, argument, type);
                  }

                  var ptt = pt.GetValue(cg.MethodInfo) as Type[];

                  int adjust = ptt.Length - 1 - _parameterInfos.Length;

                  EmitLocation(cg);

                  for (int arg = 0; arg < _parameterInfos.Length; arg++)
                  {
                    if (pars[_parameterInfos.Length - arg - 1] != null)
                    {
                      cg.Emit(OpCodes.Starg_S, _parameterInfos.Length - arg + adjust);
                    }
                  }

                  cg.Emit(OpCodes.Br, cg.startpoint);

                  cg.skipreturn = true;
                  return;
                }
              }



              Debug.Assert(_arguments.Count == _parameterInfos.Length);
              for (int arg = 0; arg < _parameterInfos.Length; arg++)
              {
                Expression argument = _arguments[arg];
                Type type = _parameterInfos[arg].ParameterType;
                EmitArgument(cg, argument, type);
              }

              EmitLocation(cg);

              //ever hit????
              // in some cases
              cbe.EmitDirect(cg, tailcall);
              return;
            }
            else
              if (typeof(Delegate).IsAssignableFrom(_instance.Type))
              {
                //Console.WriteLine("eeeeek");
              }
            else
            {
              ;
            }
          }

          var ii = _instance;

          while (ii is UnaryExpression && ii.NodeType == AstNodeType.Convert)
          {
            ii = ((UnaryExpression)ii).Operand;
          }

          BoundExpression.Emitter fixup = null;

          if (ii is BoundExpression)
          {
            var be = ii as BoundExpression;

            //if (be.Variable.Name == SymbolTable.StringToId("car"))
            //{
            //  Debugger.Break();
            //}

            if (BoundExpression.Fixups.ContainsKey(be.Variable.Name))
            {
              fixup = BoundExpression.Fixups[be.Variable.Name];
            }
          }
            // Emit instance, if calling an instance method
            if (!_method.IsStatic) {
                Type type = _method.DeclaringType;

                if (type.IsValueType) {
                    _instance.EmitAddress(cg, type);
                } else {
                  if (fixup == null)
                  {
                    _instance.Emit(cg);
                  }
                  else
                  {
                    //ii.Emit(cg);
                  }
                }
            }

            // Emit arguments
            Debug.Assert(_arguments.Count == _parameterInfos.Length);
            for (int arg = 0; arg < _parameterInfos.Length; arg++) {
                Expression argument = _arguments[arg];
                Type type = _parameterInfos[arg].ParameterType;
                EmitArgument(cg, argument, type);
            }

            EmitLocation(cg);
            // Emit the actual call

            if (fixup == null)
            {
              cg.EmitCall(_method, tailcall);
            }
            else
            {
              fixup(cg, tailcall);
            }
        }

        private bool HasNoCallableArgs()
        {
          foreach (var a in _arguments)
          {
            if (a.Type.Name == "Callable")
            {
              return false;
            }
          }
          return true;
        }

        static FieldInfo pt = typeof(MethodBuilder).GetField("m_parameterTypes", BindingFlags.NonPublic | BindingFlags.Instance);

        bool IsParamsMethod()
        {
          return _parameterInfos.Length == 1 && _parameterInfos[0].ParameterType.IsArray;
        }

        bool tailcall = false;

        public bool TailCall
        {
          get { return tailcall; }
          set { tailcall = value; }
        }

        private static void EmitArgument(CodeGen cg, Expression argument, Type type) {
          if (argument.Type != type && argument.Type.IsValueType)
          {
            argument = Ast.Convert(argument, type);
          }

            if (type.IsByRef) {
                argument.EmitAddress(cg, type.GetElementType());
            } else {
                argument.Emit(cg);
            }
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

            ValidateCallArguments(parameters, arguments);

            return new MethodCallExpression(method, instance, CollectionUtils.ToReadOnlyCollection(arguments), parameters);
        }

        private static void ValidateCallArguments(IList<ParameterInfo> parameters, IList<Expression> arguments) {
            Contract.Requires(parameters.Count == arguments.Count, "arguments", "Argument count must match parameter count");

            int count = parameters.Count;
            for (int index = 0; index < count; index++) {
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
        }

        internal static MethodCallExpression Call(Expression instance, MethodInfo method, IList<Expression> arguments) {
            if (arguments == null) {
                return Call(instance, method);
            } else {
                Expression[] args = new Expression[arguments.Count];
                arguments.CopyTo(args, 0);
                return Call(instance, method, args);
            }
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

            arguments = ArgumentConvertHelper(arguments, parameters);

            return Call(instance, method, arguments);
        }

        private static Expression[]/*!*/ ArgumentConvertHelper(Expression[] /*!*/ arguments, ParameterInfo[] /*!*/ parameters) {
            Debug.Assert(arguments != null);
            Debug.Assert(arguments != null);

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
            return clone ?? arguments;
        }

        private static Expression/*!*/ ArgumentConvertHelper(Expression/*!*/ argument, Type/*!*/ type) {
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
