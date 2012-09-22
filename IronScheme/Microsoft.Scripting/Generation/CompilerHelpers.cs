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
using System.Reflection.Emit;
using System.IO;

using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Utils;
using Microsoft.Scripting.Math;
using Microsoft.Scripting.Actions;

namespace Microsoft.Scripting.Generation {
    using Ast = Microsoft.Scripting.Ast.Ast;

    public static class CompilerHelpers {
        public static readonly MethodAttributes PublicStatic = MethodAttributes.Public | MethodAttributes.Static;

        public static string[] GetArgumentNames(ParameterInfo[] parameterInfos) {
            string[] ret = new string[parameterInfos.Length];
            for (int i = 0; i < parameterInfos.Length; i++) ret[i] = parameterInfos[i].Name;
            return ret;
        }

        public static Type[] GetTypesWithThis(MethodBase mi) {
            Type[] types = ReflectionUtils.GetParameterTypes(mi.GetParameters());
            if (IsStatic(mi)) {
                return types;
            }

            return ArrayUtils.Insert(mi.DeclaringType, types);
        }


        public static Type GetReturnType(MethodBase mi) {
            if (mi.IsConstructor) return mi.DeclaringType;
            else return ((MethodInfo)mi).ReturnType;
        }

        public static int GetStaticNumberOfArgs(MethodBase method) {
            if (IsStatic(method)) return method.GetParameters().Length;

            return method.GetParameters().Length + 1;
        }

        public static bool IsParamsMethod(MethodBase method) {
            return IsParamsMethod(method.GetParameters());
        }

        public static bool IsParamsMethod(ParameterInfo[] pis) {
            foreach (ParameterInfo pi in pis) {
              if (IsParamArray(pi)
) return true;
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

        private static int GetMandatoryParameterCount(ParameterInfo[] parameters) {
            Assert.NotNull(parameters);
            int lastMandatory = parameters.Length - 1;
            while (lastMandatory >= 0 && !IsMandatoryParameter(parameters[lastMandatory])) lastMandatory--;
            return (lastMandatory >= 0 && IsParamArray(parameters[lastMandatory])) ? lastMandatory : lastMandatory + 1;
        }

        public static int GetOutAndByRefParameterCount(MethodBase method) {
            int res = 0;
            ParameterInfo[] pis = method.GetParameters();
            for (int i = 0; i < pis.Length; i++) {
                if (IsByRefParameter(pis[i])) res++;
            }
            return res;
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

        public static bool IsByRefParameter(ParameterInfo pi) {
            // not using IsIn/IsOut properties as they are not available in Silverlight:
            if (pi.ParameterType.IsByRef) return true;

            return (pi.Attributes & (ParameterAttributes.Out)) == ParameterAttributes.Out;
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
        /// A helper routine to check if a type can be treated as sealed - i.e. there
        /// can never be a subtype of this given type.  This corresponds to a type
        /// that is either declared "Sealed" or is a ValueType and thus unable to be
        /// extended.
        /// </summary>
        public static bool IsSealed(Type type) {
            return type.IsSealed || type.IsValueType;
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
#if DEBUG
            if (!String.IsNullOrEmpty(context.SourceUnit.Id)) {
                typeName = ReflectionUtils.ToValidTypeName(Path.GetFileNameWithoutExtension(IOUtils.ToValidPath(context.SourceUnit.Id)));
            }
#endif

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
            return context != null && context.SourceUnit.Engine.Options.ClrDebuggingEnabled && context.SourceUnit.IsVisibleToDebugger;
        }

        #endregion

        public static Operators OperatorToReverseOperator(Operators op) {
            switch (op) {
                case Operators.LessThan: return Operators.GreaterThan;
                case Operators.LessThanOrEqual: return Operators.GreaterThanOrEqual;
                case Operators.GreaterThan: return Operators.LessThan;
                case Operators.GreaterThanOrEqual: return Operators.LessThanOrEqual;
                case Operators.Equals: return Operators.Equals;
                case Operators.NotEquals: return Operators.NotEquals;
                default:
                    if (op >= Operators.Add && op <= Operators.Xor) {
                        return (Operators)((int)op + (int)Operators.ReverseAdd - (int)Operators.Add);
                    }
                    return Operators.None;
            }
        }

        public static Operators InPlaceOperatorToOperator(Operators op) {
            switch (op) {
                case Operators.InPlaceAdd: return Operators.Add;
                case Operators.InPlaceBitwiseAnd: return Operators.BitwiseAnd;
                case Operators.InPlaceBitwiseOr: return Operators.BitwiseOr;
                case Operators.InPlaceDivide: return Operators.Divide;
                case Operators.InPlaceFloorDivide: return Operators.FloorDivide;
                case Operators.InPlaceLeftShift: return Operators.LeftShift;
                case Operators.InPlaceMod: return Operators.Mod;
                case Operators.InPlaceMultiply: return Operators.Multiply;
                case Operators.InPlacePower: return Operators.Power;
                case Operators.InPlaceRightShift: return Operators.RightShift;
                case Operators.InPlaceSubtract: return Operators.Subtract;
                case Operators.InPlaceTrueDivide: return Operators.TrueDivide;
                case Operators.InPlaceXor: return Operators.Xor;
                case Operators.InPlaceRightShiftUnsigned: return Operators.RightShiftUnsigned;
                default: return Operators.None;
            }

        }
        public static bool IsComparisonOperator(Operators op) {
            switch (op) {
                case Operators.LessThan: return true;
                case Operators.LessThanOrEqual: return true;
                case Operators.GreaterThan: return true;
                case Operators.GreaterThanOrEqual: return true;
                case Operators.Equals: return true;
                case Operators.NotEquals: return true;
                case Operators.Compare: return true;
            }
            return false;
        }

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

        public static bool CanOptimizeMethod(MethodBase method) {
            if (method.ContainsGenericParameters ||
                method.IsFamily ||
                method.IsPrivate ||
                method.IsFamilyOrAssembly ||
                !method.DeclaringType.IsVisible) {
                return false;
            }
            return true;
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

        /// <summary>
        /// Given a MethodInfo which may be declared on a non-public type this attempts to
        /// return a MethodInfo which will dispatch to the original MethodInfo but is declared
        /// on a public type.
        /// 
        /// Throws InvalidOperationException if the method cannot be obtained.
        /// </summary>
        public static MethodInfo GetCallableMethod(MethodInfo method) {
            MethodInfo mi = TryGetCallableMethod(method);
            if (mi == null) {
                if (!ScriptDomainManager.Options.PrivateBinding) {
                    throw new InvalidOperationException(String.Format("{0}.{1} has no publiclly visible method", method.DeclaringType, method.Name));
                }
            }
            return mi;
        }

        public static bool CanOptimizeField(FieldInfo fi) {
            return fi.IsPublic && fi.DeclaringType.IsVisible;
        }

        internal static void CreateYieldLabels(CodeGen cg, List<YieldTarget> targets) {
            if (targets != null) {
                foreach (YieldTarget yt in targets) {
                    yt.EnsureLabel(cg);
                }
            }
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

        public static MethodBase[] GetConstructors(Type t) {
            if (t.IsArray) {
                // The JIT verifier doesn't like new int[](3) even though it appears as a ctor.
                // We could do better and return newarr in the future.
                return new MethodBase[] { GetArrayCtor(t) };
            }

            BindingFlags bf = BindingFlags.Instance | BindingFlags.Public;
            if (ScriptDomainManager.Options.PrivateBinding) {
                bf |= BindingFlags.NonPublic;
            }

            ConstructorInfo[] ci = t.GetConstructors(bf);

            if (t.IsValueType) {
                // structs don't define a parameterless ctor, add a generic method for that.
                return ArrayUtils.Insert<MethodBase>(GetStructDefaultCtor(t), ci);
            }

            if (typeof(Delegate).IsAssignableFrom(t)) {
                return ArrayUtils.Insert<MethodBase>(GetDelegateCtor(t), ci);
            }

            return ci;
        }

        private static MethodBase GetStructDefaultCtor(Type t) {
            return typeof(BinderOps).GetMethod("CreateInstance").MakeGenericMethod(t);
        }

        private static MethodBase GetArrayCtor(Type t) {
            return typeof(BinderOps).GetMethod("CreateArray").MakeGenericMethod(t.GetElementType());
        }

        private static MethodBase GetDelegateCtor(Type t) {
            return typeof(BinderOps).GetMethod("CreateDelegate").MakeGenericMethod(t);
        }

        public static bool HasImplicitConversion(Type fromType, Type toType) {
            if (CompilerHelpers.HasImplicitConversion(fromType, toType, toType.GetMember("op_Implicit"))) {
                return true;
            }

            Type curType = fromType;
            do {
                if (CompilerHelpers.HasImplicitConversion(fromType, toType, curType.GetMember("op_Implicit"))) {
                    return true;
                }
                curType = curType.BaseType;
            } while (curType != null);

            return false;
        }

        public static bool TryImplicitConversion(Object value, Type to, out object result) {
            if (CompilerHelpers.TryImplicitConvert(value, to, to.GetMember("op_Implicit"), out result)) {
                return true;
            }

            Type curType = CompilerHelpers.GetType(value);
            do {
                if (CompilerHelpers.TryImplicitConvert(value, to, curType.GetMember("op_Implicit"), out result)) {
                    return true;
                }
                curType = curType.BaseType;
            } while (curType != null);

            return false;
        }

        private static bool TryImplicitConvert(Object value, Type to, MemberInfo[] implicitConv, out object result) {
            foreach (MethodInfo mi in implicitConv) {
                if (to.IsValueType == mi.ReturnType.IsValueType && to.IsAssignableFrom(mi.ReturnType)) {
                    if (mi.IsStatic) {
                        result = mi.Invoke(null, new object[] { value });
                    } else {
                        result = mi.Invoke(value, ArrayUtils.EmptyObjects);
                    }
                    return true;
                }
            }

            result = null;
            return false;
        }

        private static bool HasImplicitConversion(Type fromType, Type to, MemberInfo[] implicitConv) {
            foreach (MethodInfo mi in implicitConv) {
                if (mi.ReturnType == to && mi.GetParameters()[0].ParameterType.IsAssignableFrom(fromType)) {
                    return true;
                }
            }

            return false;
        }

        public static bool IsStrongBox(object target) {
            Type t = CompilerHelpers.GetType(target);

            return IsStrongBox(t);
        }

        public static bool IsStrongBox(Type t) {
            return t.IsGenericType && t.GetGenericTypeDefinition() == typeof(StrongBox<>);
        }
    }
}
