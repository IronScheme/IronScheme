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
    internal static class ReflectionUtils {

        /// <exception cref="InvalidImplementationException">The type failed to instantiate.</exception>
        internal static T CreateInstance<T>(Type actualType, params object[] args) {
            Type type = typeof(T);

            Debug.Assert(type.IsAssignableFrom(actualType));

            try {
                return (T)Activator.CreateInstance(actualType, 
                  BindingFlags.CreateInstance | BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance, null, args, null);
            } catch (TargetInvocationException e) {
                throw new InvalidImplementationException(System.String.Format(Resources.InvalidCtorImplementation, actualType), e.InnerException);
            } catch (Exception e) {
                throw new InvalidImplementationException(System.String.Format(Resources.InvalidCtorImplementation, actualType), e);
            }
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

        public static Type[] GetParameterTypes(ParameterInfo[] parameterInfos) {
            Type[] result = new Type[parameterInfos.Length];
            for (int i = 0; i < parameterInfos.Length; i++) result[i] = parameterInfos[i].ParameterType;
            return result;
        }

        public const int MaxSignatureSize = 8;
    }
}
