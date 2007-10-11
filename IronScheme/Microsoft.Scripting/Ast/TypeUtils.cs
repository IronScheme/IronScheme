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

namespace Microsoft.Scripting.Ast {
    static class TypeUtils {
        internal static Type GetNonNullableType(Type type) {
            if (type.IsGenericType && type.GetGenericTypeDefinition() == typeof(Nullable<>)) {
                return type.GetGenericArguments()[0];
            }
            return type;
        }

        internal static bool IsBool(Type type) {
            return GetNonNullableType(type) == typeof(bool);
        }

        internal static bool IsNumeric(Type type) {
            type = GetNonNullableType(type);
            if (!type.IsEnum) {
                switch (Type.GetTypeCode(type)) {
                    case TypeCode.Char:
                    case TypeCode.SByte:
                    case TypeCode.Byte:
                    case TypeCode.Int16:
                    case TypeCode.Int32:
                    case TypeCode.Int64:
                    case TypeCode.Double:
                    case TypeCode.Single:
                    case TypeCode.UInt16:
                    case TypeCode.UInt32:
                    case TypeCode.UInt64:
                        return true;
                }
            }
            return false;
        }

        internal static bool IsArithmetic(Type type) {
            type = GetNonNullableType(type);
            if (!type.IsEnum) {
                switch (Type.GetTypeCode(type)) {
                    case TypeCode.Int16:
                    case TypeCode.Int32:
                    case TypeCode.Int64:
                    case TypeCode.Double:
                    case TypeCode.Single:
                    case TypeCode.UInt16:
                    case TypeCode.UInt32:
                    case TypeCode.UInt64:
                        return true;
                }
            }
            return false;
        }

        internal static bool IsIntegerOrBool(Type type) {
            type = GetNonNullableType(type);
            if (!type.IsEnum) {
                switch (Type.GetTypeCode(type)) {
                    case TypeCode.Int64:
                    case TypeCode.Int32:
                    case TypeCode.Int16:
                    case TypeCode.UInt64:
                    case TypeCode.UInt32:
                    case TypeCode.UInt16:
                    case TypeCode.Boolean:
                    case TypeCode.SByte:
                    case TypeCode.Byte:
                        return true;
                }
            }
            return false;
        }

        internal static bool CanAssign(Type to, Type from) {
            if (to == from) {
                return true;
            }
            // Reference types
            if (!to.IsValueType && !from.IsValueType) {
                if (to.IsAssignableFrom(from)) {
                    return true;
                }
                // Arrays can be assigned if they have same rank and assignable element types.
                if (to.IsArray && from.IsArray &&
                    to.GetArrayRank() == from.GetArrayRank() &&
                    CanAssign(to.GetElementType(), from.GetElementType())) {
                    return true;
                }
            }
            return false;
        }
    }
}
