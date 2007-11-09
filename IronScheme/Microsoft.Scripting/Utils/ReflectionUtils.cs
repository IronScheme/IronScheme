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
using System.Reflection;
using System.Reflection.Emit;
using System.Diagnostics;

namespace Microsoft.Scripting.Utils {
    public static class ReflectionUtils {

        // Generic type names have the arity (number of generic type paramters) appended at the end. 
        // For eg. the mangled name of System.List<T> is "List`1". This mangling is done to enable multiple 
        // generic types to exist as long as they have different arities.
        public const char GenericArityDelimiter = '`';

#if SILVERLIGHT
        public static bool IsNested(Type t) {
            return t.DeclaringType != null;
        }
#else
        public static bool IsNested(Type t) { return t.IsNested; }
#endif

        public static StringBuilder FormatSignature(StringBuilder result, MethodBase method) {
            Contract.RequiresNotNull(result, "result");
            Contract.RequiresNotNull(method, "method");

            MethodInfo methodInfo = method as MethodInfo;
            if (methodInfo != null) {
                FormatTypeName(result, methodInfo.ReturnType);
                result.Append(' ');
            }

            MethodBuilder builder = method as MethodBuilder;
            if (builder != null) {
                result.Append(builder.Signature);
                return result;
            }

            ConstructorBuilder cb = method as ConstructorBuilder;
            if (cb != null) {
                result.Append(cb.Signature);
                return result;
            }

            FormatTypeName(result, method.DeclaringType);
            result.Append("::");
            result.Append(method.Name);

            if (!method.IsConstructor) {
                FormatTypeArgs(result, method.GetGenericArguments());
            }

            result.Append("(");

            if (!method.ContainsGenericParameters) {
                ParameterInfo[] ps = method.GetParameters();
                for (int i = 0; i < ps.Length; i++) {
                    if (i > 0) result.Append(", ");
                    FormatTypeName(result, ps[i].ParameterType);
                    if (!System.String.IsNullOrEmpty(ps[i].Name)) {
                        result.Append(" ");
                        result.Append(ps[i].Name);
                    }
                }
            } else {
                result.Append("?");
            }

            result.Append(")");
            return result;
        }

        public static StringBuilder FormatTypeName(StringBuilder result, Type type) {
            Assert.NotNull(result, type);

            if (type.IsGenericType) {
                string genericName = type.GetGenericTypeDefinition().FullName.Replace('+', '.');
                int tickIndex = genericName.IndexOf('`');
                result.Append(tickIndex != -1 ? genericName.Substring(0, tickIndex) : genericName);
                FormatTypeArgs(result, type.GetGenericArguments());
            } else if (type.IsGenericParameter) {
                result.Append(type.Name);
            } else {
                result.Append(type.FullName.Replace('+', '.'));
            }
            return result;
        }

        public static StringBuilder FormatTypeArgs(StringBuilder result, Type[] types) {
            Assert.NotNull(result, types);
            if (types.Length > 0) {
                result.Append("<");

                for (int i = 0; i < types.Length; i++) {
                    if (i > 0) result.Append(", ");
                    FormatTypeName(result, types[i]);
                }

                result.Append(">");
            }
            return result;
        }

        /// <exception cref="InvalidImplementationException">The type failed to instantiate.</exception>
        internal static T CreateInstance<T>(Type actualType, params object[] args) {
            Type type = typeof(T);

            Debug.Assert(type.IsAssignableFrom(actualType));

            try {
                return (T)Activator.CreateInstance(actualType, args);
            } catch (TargetInvocationException e) {
                throw new InvalidImplementationException(System.String.Format(Resources.InvalidCtorImplementation, actualType), e.InnerException);
            } catch (Exception e) {
                throw new InvalidImplementationException(System.String.Format(Resources.InvalidCtorImplementation, actualType), e);
            }
        }

        public static object InvokeDelegate(Delegate d, params object[] args) {
#if SILVERLIGHT
            // delegates:
            //   - close (target != null)
            //     - static (target becomes the first argument)
            //     - instance (no argument shuffling)
            //   - open (target == null)
            //     - static (no argument shuffling)
            //     - instance (first argument becomes the target)

            object target = d.Target;

            if (d.Method.IsStatic && target != null) {
                // closed static -> target needs to be passed as the first arg:
                object[] new_args = new object[args.Length + 1];
                args.CopyTo(new_args, 1);
                new_args[0] = d.Target;

                target = null;
                args = new_args;

            } else if (!d.Method.IsStatic && target == null) {

                // open instance -> the first arg is the target:
                object[] new_args = new object[args.Length - 1];
                System.Array.Copy(args, 1, new_args, 0, new_args.Length);

                target = args[0];
                args = new_args;
            }

            return d.Method.Invoke(target, args);
#else
            return d.DynamicInvoke(args);
#endif
        }

        /// <summary>
        /// Creates an open delegate for the given (dynamic)method.
        /// </summary>
        public static Delegate CreateDelegate(MethodInfo methodInfo, Type delegateType) {
            Contract.RequiresNotNull(delegateType, "delegateType");
            Contract.RequiresNotNull(methodInfo, "methodInfo");

            DynamicMethod dm = methodInfo as DynamicMethod;
            if (dm != null) {
                return dm.CreateDelegate(delegateType);
            } else {
                return Delegate.CreateDelegate(delegateType, methodInfo);
            }
        }

        /// <summary>
        /// Creates a closed delegate for the given (dynamic)method.
        /// </summary>
        public static Delegate CreateDelegate(MethodInfo methodInfo, Type delegateType, object target) {
            Contract.RequiresNotNull(methodInfo, "methodInfo");
            Contract.RequiresNotNull(delegateType, "delegateType");

            DynamicMethod dm = methodInfo as DynamicMethod;
            if (dm != null) {
                return dm.CreateDelegate(delegateType, target);
            } else {
                return Delegate.CreateDelegate(delegateType, target, methodInfo);
            }
        }

        public static void GetDelegateSignature(Type delegateType, out ParameterInfo[] parameterInfos, out ParameterInfo returnInfo) {
            Contract.RequiresNotNull(delegateType, "delegateType");

            MethodInfo invokeMethod = delegateType.GetMethod("Invoke");
            Contract.Requires(invokeMethod != null, "delegateType", "Invalid delegate type (Invoke method not found).");

            parameterInfos = invokeMethod.GetParameters();
            returnInfo = invokeMethod.ReturnParameter;
        }

        public static MethodInfo[] GetMethodInfos(Delegate[] delegates) {
            MethodInfo[] result = new MethodInfo[delegates.Length];
            for (int i = 0; i < delegates.Length; i++) result[i] = delegates[i].Method;
            return result;
        }

        public static MethodBase[] GetMethodInfos(MemberInfo[] members) {
            return ArrayUtils.ConvertAll<MemberInfo, MethodBase>(
                members,
                delegate(MemberInfo inp) { return (MethodBase)inp; });
        }

        public static Type[] GetParameterTypes(ParameterInfo[] parameterInfos) {
            Type[] result = new Type[parameterInfos.Length];
            for (int i = 0; i < parameterInfos.Length; i++) result[i] = parameterInfos[i].ParameterType;
            return result;
        }

        public static Type GetDelegateType(Type[] arguments, Type returnType) {
            Assert.NotNull(arguments, returnType);
            Type result;

            if (returnType == typeof(void)) {
                switch (arguments.Length) {
                    case 0: return typeof(Action);
                    case 1: result = typeof(Action<>); break;
                    case 2: result = typeof(Action<,>); break;
                    case 3: result = typeof(Action<,,>); break;
                    case 4: result = typeof(Action<,,,>); break;
                    case 5: result = typeof(Action<,,,,>); break;
                    case 6: result = typeof(Action<,,,,,>); break;
                    case 7: result = typeof(Action<,,,,,,>); break;
                    default:
                        throw new NotImplementedException("Action delegate not implemented for " + arguments.Length + " arguments.");
                }
            } else {
                arguments = ArrayUtils.Append(arguments, returnType);
                switch (arguments.Length) {
                    case 0: throw Assert.Unreachable;
                    case 1: result = typeof(Function<>); break;
                    case 2: result = typeof(Function<,>); break;
                    case 3: result = typeof(Function<,,>); break;
                    case 4: result = typeof(Function<,,,>); break;
                    case 5: result = typeof(Function<,,,,>); break;
                    case 6: result = typeof(Function<,,,,,>); break;
                    case 7: result = typeof(Function<,,,,,,>); break;
                    case 8: result = typeof(Function<,,,,,,,>); break;
                    default:
                        throw new NotImplementedException("Function delegate not implemented for " + arguments.Length + " arguments.");
                }
            }

            return result.MakeGenericType(arguments);
        }

        public const int MaxSignatureSize = 8;

        public static bool SignatureEquals(MethodInfo method, params Type[] requiredSignature) {
            Contract.RequiresNotNull(method, "method");

            Type[] actualTypes = ReflectionUtils.GetParameterTypes(method.GetParameters());
            Debug.Assert(actualTypes.Length == requiredSignature.Length - 1);
            int i = 0;
            while (i < actualTypes.Length) {
                if (actualTypes[i] != requiredSignature[i]) return false;
                i++;
            }

            return method.ReturnType == requiredSignature[i];
        }

        internal static string ToValidTypeName(string str) {
            if (String.IsNullOrEmpty(str)) {
                return "_";
            }

            StringBuilder sb = new StringBuilder(str);
            for (int i = 0; i < str.Length; i++) {
                if (str[i] == '\0' || str[i] == '.' || str[i] == '*' || str[i] == '+' || str[i] == '[' || str[i] == ']' || str[i] == '\\') {
                    sb[i] = '_';
                }
            }
            return sb.ToString();
        }

        /// <summary>
        /// Like Type.GetInterfaces, but only returns the interfaces implemented by this type
        /// and not its parents.
        /// </summary>
        public static List<Type> GetDeclaredInterfaces(Type type) {
            IList<Type> baseInterfaces = (type.BaseType != null) ? type.BaseType.GetInterfaces() : Type.EmptyTypes;
            List<Type> interfaces = new List<Type>();
            foreach (Type iface in type.GetInterfaces()) {
                if (!baseInterfaces.Contains(iface)) {
                    interfaces.Add(iface);
                }
            }
            return interfaces;
        }

        public static string GetNormalizedTypeName(Type type) {
            string name = type.Name;
            if (type.IsGenericType) {
                return GetNormalizedTypeName(name);
            }
            return name;
        }

        public static string GetNormalizedTypeName(string typeName) {
            Debug.Assert(typeName.IndexOf(Type.Delimiter) == -1); // This is the simple name, not the full name
            int backtick = typeName.IndexOf(ReflectionUtils.GenericArityDelimiter);
            if (backtick != -1) return typeName.Substring(0, backtick);
            return typeName;
        }

    }
}
