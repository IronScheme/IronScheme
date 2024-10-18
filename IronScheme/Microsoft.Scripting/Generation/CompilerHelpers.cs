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
using System.Reflection;
using System.Diagnostics;
using System.Collections.Generic;
using Microsoft.Scripting.Utils;
using Microsoft.Scripting.Generation.Allocators;
using Microsoft.Scripting.Generation.Factories;

namespace Microsoft.Scripting.Generation
{
    internal static class CompilerHelpers {
        public static readonly MethodAttributes PublicStatic = MethodAttributes.Public | MethodAttributes.Static;

        public static Type GetReturnType(MethodBase mi) {
            if (mi.IsConstructor) return mi.DeclaringType;
            else return ((MethodInfo)mi).ReturnType;
        }

        public static bool IsParamsMethod(MethodBase method) {
            return IsParamsMethod(method.GetParameters());
        }

        public static bool IsParamsMethod(ParameterInfo[] pis) {
            foreach (ParameterInfo pi in pis) {
              if (IsParamArray(pi)) return true;
            }
            return false;
        }

        public static bool IsParamArray(ParameterInfo parameter) {
            return parameter.IsDefined(typeof(ParamArrayAttribute), false);
        }

        public static bool IsOutParameter(ParameterInfo pi) {
            // not using IsIn/IsOut properties as they are not available in Silverlight:
            return (pi.Attributes & (ParameterAttributes.Out | ParameterAttributes.In)) == ParameterAttributes.Out;
        }

        /// <summary>
        /// Returns <c>true</c> if the specified parameter is mandatory, i.e. is not optional and doesn't have a default value.
        /// </summary>
        public static bool IsMandatoryParameter(ParameterInfo pi) {
            return (pi.Attributes & (ParameterAttributes.Optional | ParameterAttributes.HasDefault)) == 0;
        }

        public static bool HasDefaultValue(ParameterInfo pi) {
            return (pi.Attributes & ParameterAttributes.HasDefault) != 0;
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Maintainability", "CA1502:AvoidExcessiveComplexity")]
        public static object GetMissingValue(Type type) {
            Contract.RequiresNotNull(type, "type");
            
            if (type.IsByRef) type = type.GetElementType();
            if (type.IsEnum) return Activator.CreateInstance(type);

            switch (Type.GetTypeCode(type)) {
                default:
                case TypeCode.Object:
                    // struct
                    if (type.IsSealed && type.IsValueType) {
                        return Activator.CreateInstance(type);
                    } else if (type == typeof(object)) {
                        // parameter of type object receives the actual Missing value
                        return Missing.Value;
                    } else if (!type.IsValueType) {
                        return null;
                    } else {
                        throw new ArgumentException(String.Format("Cannot create default value for type {0}", type));
                    }
                case TypeCode.Empty:
                case TypeCode.DBNull:
                case TypeCode.String:
                    return null;

                case TypeCode.Boolean: return false;
                case TypeCode.Char: return '\0';
                case TypeCode.SByte: return (sbyte)0;
                case TypeCode.Byte: return (byte)0;
                case TypeCode.Int16: return (short)0;
                case TypeCode.UInt16: return (ushort)0;
                case TypeCode.Int32: return (int)0;
                case TypeCode.UInt32: return (uint)0;
                case TypeCode.Int64: return 0L;
                case TypeCode.UInt64: return 0UL;
                case TypeCode.Single: return 0.0f;
                case TypeCode.Double: return 0.0D;
                case TypeCode.Decimal: return (decimal)0;
                case TypeCode.DateTime: return DateTime.MinValue;
            }
        }

        public static bool IsStatic(MethodBase mi) {
            return mi.IsConstructor || mi.IsStatic;
        }

        public static T[] MakeRepeatedArray<T>(T item, int count) {
            T[] ret = new T[count];
            for (int i = 0; i < count; i++) ret[i] = item;
            return ret;
        }

        /// <summary>
        /// Will create storage allocator which allocates locals on the CLR stack (in the context of the codeGen).
        /// This doesn't set up allocator for globals. Further initialization needed.
        /// </summary>
        /// <param name="outer">Codegen of the lexically enclosing block.</param>
        /// <param name="codeGen">CodeGen object to use to allocate the locals on the CLR stack.</param>
        /// <returns>New ScopeAllocator</returns>
        internal static ScopeAllocator CreateLocalStorageAllocator(CodeGen outer, CodeGen codeGen) {
            LocalStorageAllocator allocator = new LocalStorageAllocator(new LocalSlotFactory(codeGen));
            return new ScopeAllocator((outer != null && outer.HasAllocator) ? outer.Allocator : null, allocator);
        }

        /// <summary>
        /// allocates slots out of a FunctionEnvironment.
        /// </summary>
        internal static ScopeAllocator CreateFrameAllocator() {
            // Globals
            ScopeAllocator global = new ScopeAllocator(
                null,
                new GlobalNamedAllocator()
            );

            // Locals
            ScopeAllocator ns = new ScopeAllocator(
                global,
                new FrameStorageAllocator()
            );
            return ns;
        }

        public static Type[] MakeParamTypeArray(IList<Type> baseParamTypes, ConstantPool constantPool) {
            if (constantPool == null) return new List<Type>(baseParamTypes).ToArray();

            List<Type> ret = new List<Type>();
            ret.Add(constantPool.SlotType);
            ret.AddRange(baseParamTypes);
            return ret.ToArray();
        }

        #region CodeGen Creation Support

        internal static CodeGen CreateDebuggableDynamicCodeGenerator(CompilerContext context, string name, Type retType, IList<Type> paramTypes, IList<string> paramNames, ConstantPool constantPool) {
            TypeGen tg = ScriptDomainManager.CurrentManager.Snippets.DefineDebuggableType(name, context.SourceUnit);
            CodeGen cg = tg.DefineMethod("Initialize", retType, paramTypes, paramNames, constantPool);

            tg.AddCodeContextField();
            cg.DynamicMethod = true;

            return cg;
        }

        /// <summary>
        /// 
        /// </summary>
        internal static CodeGen CreateDynamicCodeGenerator(string name, Type retType, IList<Type> paramTypes, ConstantPool constantPool) {
            return ScriptDomainManager.CurrentManager.Snippets.Assembly.DefineMethod(name, retType, paramTypes, constantPool);
        }

        /// <summary>
        /// Creates a new CodeGenerator for emitting the given code.
        /// The CodeGenerator is usually tied to a dynamic method
        /// unless debugging has been enabled.
        /// </summary>
        public static CodeGen CreateDynamicCodeGenerator(CompilerContext context) {
            CodeGen cg;

            string typeName = "";

            if (NeedDebuggableDynamicCodeGenerator(context)) {
                cg = CreateDebuggableDynamicCodeGenerator(
                    context,
                    typeName,
                    typeof(object),
                    new Type[] { typeof(CodeContext) },
                    null,
                    new ConstantPool()
                );
            } else {
                cg = CreateDynamicCodeGenerator(
                    typeName,
                    typeof(object),
                    new Type[] { typeof(CodeContext) },
                    new ConstantPool());

                cg.CacheConstants = false;
            }

            cg.ContextSlot = cg.GetArgumentSlot(0);
            cg.Context = context;

            // Caller wanted dynamic method, we should produce it.
            Debug.Assert(cg.DynamicMethod);

            return cg;
        }

        internal static bool NeedDebuggableDynamicCodeGenerator(CompilerContext context) {
            return context != null && context.SourceUnit.IsVisibleToDebugger;
        }

        #endregion

        /// <summary>
        /// Returns the System.Type for any object, including null.  The type of null
        /// is represented by None.Type and all other objects just return the 
        /// result of Object.GetType
        /// </summary>
        public static Type GetType(object obj) {
            return obj == null ? None.Type : obj.GetType();
        }

        /// <summary>
        /// Simply returns a Type[] from calling GetType on each element of args.
        /// </summary>
        public static Type[] GetTypes(object[] args) {
            Type[] types = new Type[args.Length];
            for (int i = 0; i < args.Length; i++) {
                types[i] = GetType(args[i]);
            }
            return types;
        }

        /// <summary>
        /// Given a MethodInfo which may be declared on a non-public type this attempts to
        /// return a MethodInfo which will dispatch to the original MethodInfo but is declared
        /// on a public type.
        /// 
        /// Returns null if a public method cannot be obtained.
        /// </summary>
        public static MethodInfo TryGetCallableMethod(MethodInfo method) {
            if (method.DeclaringType.IsVisible) return method;
            // first try and get it from the base type we're overriding...
            method = method.GetBaseDefinition();

            if (method.DeclaringType.IsVisible) return method;
            // maybe we can get it from an interface...
            Type[] interfaces = method.DeclaringType.GetInterfaces();
            foreach (Type iface in interfaces) {
                InterfaceMapping mapping = method.DeclaringType.GetInterfaceMap(iface);
                for (int i = 0; i < mapping.TargetMethods.Length; i++) {
                    if (mapping.TargetMethods[i] == method) {
                        return mapping.InterfaceMethods[i];
                    }
                }
            }

            return method;
        }

        public static Type GetVisibleType(object value) {
            return GetVisibleType(GetType(value));
        }

        public static Type GetVisibleType(Type t) {
            while (!t.IsVisible) {
                t = t.BaseType;
            }
            return t;
        }
    }
}
