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

        internal static bool IsUnsigned(Type type) {
            type = GetNonNullableType(type);
            if (!type.IsEnum) {
                switch (Type.GetTypeCode(type)) {
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

        internal static bool IsGeneric(Type type) {
            return type.ContainsGenericParameters || type.IsGenericTypeDefinition;
        }

        internal static bool CanCompareToNull(Type type) {
            // This is a bit too conservative.
            return !type.IsValueType;
        }

        /// <summary>
        /// Returns a numerical code of the size of a type.  All types get both a horizontal
        /// and vertical code.  Types that are lower in both dimensions have implicit conversions
        /// to types that are higher in both dimensions.
        /// </summary>
        internal static bool GetNumericConversionOrder(TypeCode code, out int x, out int y) {
            // implicit conversions:
            //     0     1     2     3     4
            // 0:       U1 -> U2 -> U4 -> U8
            //          |     |     |
            //          v     v     v
            // 1: I1 -> I2 -> I4 -> I8
            //          |     |     
            //          v     v     
            // 2:       R4 -> R8

            switch (code) {
                case TypeCode.Byte: x = 0; y = 0; break;
                case TypeCode.UInt16: x = 1; y = 0; break;
                case TypeCode.UInt32: x = 2; y = 0; break;
                case TypeCode.UInt64: x = 3; y = 0; break;

                case TypeCode.SByte: x = 0; y = 1; break;
                case TypeCode.Int16: x = 1; y = 1; break;
                case TypeCode.Int32: x = 2; y = 1; break;
                case TypeCode.Int64: x = 3; y = 1; break;

                case TypeCode.Single: x = 1; y = 2; break;
                case TypeCode.Double: x = 2; y = 2; break;

                default:
                    x = y = 0;
                    return false;
            }
            return true;
        }

        internal static bool IsImplicitlyConvertible(int fromX, int fromY, int toX, int toY) {
            return fromX <= toX && fromY <= toY;
        }
    }
}
