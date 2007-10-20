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
using System.Diagnostics;
using System.CodeDom.Compiler;

namespace Microsoft.Scripting {
    /// <summary>
    /// Implements explicit casts supported by the runtime.
    /// </summary>
    [GeneratedCode("DLR", "2.0")]
    public static partial class Cast {

        #region Generated Type Casts

        // *** BEGIN GENERATED CODE ***

        public static Boolean ExplicitCastToBoolean(object o) {
            if (o != null) {
                Type type = o.GetType();
                if (type == BooleanType) {
                    return (Boolean)(Boolean)o;
                } else if (type == NullableBooleanType) {
                    return (Boolean)(Nullable<Boolean>)o;
                }
            }
            throw InvalidCast(o, "Boolean");
        }

        public static Byte ExplicitCastToByte(object o) {
            if (o != null) {
                Type type = o.GetType();
                if (type == Int32Type) {
                    return (Byte)(Int32)o;
                } else if (type == DoubleType) {
                    return (Byte)(Double)o;
                } else if (type == Int64Type) {
                    return (Byte)(Int64)o;
                } else if (type == Int16Type) {
                    return (Byte)(Int16)o;
                } else if (type == UInt32Type) {
                    return (Byte)(UInt32)o;
                } else if (type == UInt64Type) {
                    return (Byte)(UInt64)o;
                } else if (type == UInt16Type) {
                    return (Byte)(UInt16)o;
                } else if (type == SByteType) {
                    return (Byte)(SByte)o;
                } else if (type == ByteType) {
                    return (Byte)(Byte)o;
                } else if (type == SingleType) {
                    return (Byte)(Single)o;
                } else if (type == CharType) {
                    return (Byte)(Char)o;
                } else if (type == DecimalType) {
                    return (Byte)(Decimal)o;
                } else if (type.IsEnum) {
                    return (Byte)ExplicitCastEnumToByte(o);
                } else if (type == NullableInt32Type) {
                    return (Byte)(Nullable<Int32>)o;
                } else if (type == NullableDoubleType) {
                    return (Byte)(Nullable<Double>)o;
                } else if (type == NullableInt64Type) {
                    return (Byte)(Nullable<Int64>)o;
                } else if (type == NullableInt16Type) {
                    return (Byte)(Nullable<Int16>)o;
                } else if (type == NullableUInt32Type) {
                    return (Byte)(Nullable<UInt32>)o;
                } else if (type == NullableUInt64Type) {
                    return (Byte)(Nullable<UInt64>)o;
                } else if (type == NullableUInt16Type) {
                    return (Byte)(Nullable<UInt16>)o;
                } else if (type == NullableSByteType) {
                    return (Byte)(Nullable<SByte>)o;
                } else if (type == NullableByteType) {
                    return (Byte)(Nullable<Byte>)o;
                } else if (type == NullableSingleType) {
                    return (Byte)(Nullable<Single>)o;
                } else if (type == NullableCharType) {
                    return (Byte)(Nullable<Char>)o;
                } else if (type == NullableDecimalType) {
                    return (Byte)(Nullable<Decimal>)o;
                }
            }
            throw InvalidCast(o, "Byte");
        }

        public static Char ExplicitCastToChar(object o) {
            if (o != null) {
                Type type = o.GetType();
                if (type == Int32Type) {
                    return (Char)(Int32)o;
                } else if (type == DoubleType) {
                    return (Char)(Double)o;
                } else if (type == Int64Type) {
                    return (Char)(Int64)o;
                } else if (type == Int16Type) {
                    return (Char)(Int16)o;
                } else if (type == UInt32Type) {
                    return (Char)(UInt32)o;
                } else if (type == UInt64Type) {
                    return (Char)(UInt64)o;
                } else if (type == UInt16Type) {
                    return (Char)(UInt16)o;
                } else if (type == SByteType) {
                    return (Char)(SByte)o;
                } else if (type == ByteType) {
                    return (Char)(Byte)o;
                } else if (type == SingleType) {
                    return (Char)(Single)o;
                } else if (type == CharType) {
                    return (Char)(Char)o;
                } else if (type == DecimalType) {
                    return (Char)(Decimal)o;
                } else if (type.IsEnum) {
                    return (Char)ExplicitCastEnumToInt32(o);
                } else if (type == NullableInt32Type) {
                    return (Char)(Nullable<Int32>)o;
                } else if (type == NullableDoubleType) {
                    return (Char)(Nullable<Double>)o;
                } else if (type == NullableInt64Type) {
                    return (Char)(Nullable<Int64>)o;
                } else if (type == NullableInt16Type) {
                    return (Char)(Nullable<Int16>)o;
                } else if (type == NullableUInt32Type) {
                    return (Char)(Nullable<UInt32>)o;
                } else if (type == NullableUInt64Type) {
                    return (Char)(Nullable<UInt64>)o;
                } else if (type == NullableUInt16Type) {
                    return (Char)(Nullable<UInt16>)o;
                } else if (type == NullableSByteType) {
                    return (Char)(Nullable<SByte>)o;
                } else if (type == NullableByteType) {
                    return (Char)(Nullable<Byte>)o;
                } else if (type == NullableSingleType) {
                    return (Char)(Nullable<Single>)o;
                } else if (type == NullableCharType) {
                    return (Char)(Nullable<Char>)o;
                } else if (type == NullableDecimalType) {
                    return (Char)(Nullable<Decimal>)o;
                }
            }
            throw InvalidCast(o, "Char");
        }

        public static Decimal ExplicitCastToDecimal(object o) {
            if (o != null) {
                Type type = o.GetType();
                if (type == Int32Type) {
                    return (Decimal)(Int32)o;
                } else if (type == DoubleType) {
                    return (Decimal)(Double)o;
                } else if (type == Int64Type) {
                    return (Decimal)(Int64)o;
                } else if (type == Int16Type) {
                    return (Decimal)(Int16)o;
                } else if (type == UInt32Type) {
                    return (Decimal)(UInt32)o;
                } else if (type == UInt64Type) {
                    return (Decimal)(UInt64)o;
                } else if (type == UInt16Type) {
                    return (Decimal)(UInt16)o;
                } else if (type == SByteType) {
                    return (Decimal)(SByte)o;
                } else if (type == ByteType) {
                    return (Decimal)(Byte)o;
                } else if (type == SingleType) {
                    return (Decimal)(Single)o;
                } else if (type == CharType) {
                    return (Decimal)(Char)o;
                } else if (type == DecimalType) {
                    return (Decimal)(Decimal)o;
                } else if (type.IsEnum) {
                    return (Decimal)ExplicitCastEnumToInt64(o);
                } else if (type == NullableInt32Type) {
                    return (Decimal)(Nullable<Int32>)o;
                } else if (type == NullableDoubleType) {
                    return (Decimal)(Nullable<Double>)o;
                } else if (type == NullableInt64Type) {
                    return (Decimal)(Nullable<Int64>)o;
                } else if (type == NullableInt16Type) {
                    return (Decimal)(Nullable<Int16>)o;
                } else if (type == NullableUInt32Type) {
                    return (Decimal)(Nullable<UInt32>)o;
                } else if (type == NullableUInt64Type) {
                    return (Decimal)(Nullable<UInt64>)o;
                } else if (type == NullableUInt16Type) {
                    return (Decimal)(Nullable<UInt16>)o;
                } else if (type == NullableSByteType) {
                    return (Decimal)(Nullable<SByte>)o;
                } else if (type == NullableByteType) {
                    return (Decimal)(Nullable<Byte>)o;
                } else if (type == NullableSingleType) {
                    return (Decimal)(Nullable<Single>)o;
                } else if (type == NullableCharType) {
                    return (Decimal)(Nullable<Char>)o;
                } else if (type == NullableDecimalType) {
                    return (Decimal)(Nullable<Decimal>)o;
                }
            }
            throw InvalidCast(o, "Decimal");
        }

        public static Double ExplicitCastToDouble(object o) {
            if (o != null) {
                Type type = o.GetType();
                if (type == Int32Type) {
                    return (Double)(Int32)o;
                } else if (type == DoubleType) {
                    return (Double)(Double)o;
                } else if (type == Int64Type) {
                    return (Double)(Int64)o;
                } else if (type == Int16Type) {
                    return (Double)(Int16)o;
                } else if (type == UInt32Type) {
                    return (Double)(UInt32)o;
                } else if (type == UInt64Type) {
                    return (Double)(UInt64)o;
                } else if (type == UInt16Type) {
                    return (Double)(UInt16)o;
                } else if (type == SByteType) {
                    return (Double)(SByte)o;
                } else if (type == ByteType) {
                    return (Double)(Byte)o;
                } else if (type == SingleType) {
                    return (Double)(Single)o;
                } else if (type == CharType) {
                    return (Double)(Char)o;
                } else if (type == DecimalType) {
                    return (Double)(Decimal)o;
                } else if (type.IsEnum) {
                    return (Double)ExplicitCastEnumToInt64(o);
                } else if (type == NullableInt32Type) {
                    return (Double)(Nullable<Int32>)o;
                } else if (type == NullableDoubleType) {
                    return (Double)(Nullable<Double>)o;
                } else if (type == NullableInt64Type) {
                    return (Double)(Nullable<Int64>)o;
                } else if (type == NullableInt16Type) {
                    return (Double)(Nullable<Int16>)o;
                } else if (type == NullableUInt32Type) {
                    return (Double)(Nullable<UInt32>)o;
                } else if (type == NullableUInt64Type) {
                    return (Double)(Nullable<UInt64>)o;
                } else if (type == NullableUInt16Type) {
                    return (Double)(Nullable<UInt16>)o;
                } else if (type == NullableSByteType) {
                    return (Double)(Nullable<SByte>)o;
                } else if (type == NullableByteType) {
                    return (Double)(Nullable<Byte>)o;
                } else if (type == NullableSingleType) {
                    return (Double)(Nullable<Single>)o;
                } else if (type == NullableCharType) {
                    return (Double)(Nullable<Char>)o;
                } else if (type == NullableDecimalType) {
                    return (Double)(Nullable<Decimal>)o;
                }
            }
            throw InvalidCast(o, "Double");
        }

        public static Int16 ExplicitCastToInt16(object o) {
            if (o != null) {
                Type type = o.GetType();
                if (type == Int32Type) {
                    return (Int16)(Int32)o;
                } else if (type == DoubleType) {
                    return (Int16)(Double)o;
                } else if (type == Int64Type) {
                    return (Int16)(Int64)o;
                } else if (type == Int16Type) {
                    return (Int16)(Int16)o;
                } else if (type == UInt32Type) {
                    return (Int16)(UInt32)o;
                } else if (type == UInt64Type) {
                    return (Int16)(UInt64)o;
                } else if (type == UInt16Type) {
                    return (Int16)(UInt16)o;
                } else if (type == SByteType) {
                    return (Int16)(SByte)o;
                } else if (type == ByteType) {
                    return (Int16)(Byte)o;
                } else if (type == SingleType) {
                    return (Int16)(Single)o;
                } else if (type == CharType) {
                    return (Int16)(Char)o;
                } else if (type == DecimalType) {
                    return (Int16)(Decimal)o;
                } else if (type.IsEnum) {
                    return (Int16)ExplicitCastEnumToInt16(o);
                } else if (type == NullableInt32Type) {
                    return (Int16)(Nullable<Int32>)o;
                } else if (type == NullableDoubleType) {
                    return (Int16)(Nullable<Double>)o;
                } else if (type == NullableInt64Type) {
                    return (Int16)(Nullable<Int64>)o;
                } else if (type == NullableInt16Type) {
                    return (Int16)(Nullable<Int16>)o;
                } else if (type == NullableUInt32Type) {
                    return (Int16)(Nullable<UInt32>)o;
                } else if (type == NullableUInt64Type) {
                    return (Int16)(Nullable<UInt64>)o;
                } else if (type == NullableUInt16Type) {
                    return (Int16)(Nullable<UInt16>)o;
                } else if (type == NullableSByteType) {
                    return (Int16)(Nullable<SByte>)o;
                } else if (type == NullableByteType) {
                    return (Int16)(Nullable<Byte>)o;
                } else if (type == NullableSingleType) {
                    return (Int16)(Nullable<Single>)o;
                } else if (type == NullableCharType) {
                    return (Int16)(Nullable<Char>)o;
                } else if (type == NullableDecimalType) {
                    return (Int16)(Nullable<Decimal>)o;
                }
            }
            throw InvalidCast(o, "Int16");
        }

        public static Int32 ExplicitCastToInt32(object o) {
            if (o != null) {
                Type type = o.GetType();
                if (type == Int32Type) {
                    return (Int32)(Int32)o;
                } else if (type == DoubleType) {
                    return (Int32)(Double)o;
                } else if (type == Int64Type) {
                    return (Int32)(Int64)o;
                } else if (type == Int16Type) {
                    return (Int32)(Int16)o;
                } else if (type == UInt32Type) {
                    return (Int32)(UInt32)o;
                } else if (type == UInt64Type) {
                    return (Int32)(UInt64)o;
                } else if (type == UInt16Type) {
                    return (Int32)(UInt16)o;
                } else if (type == SByteType) {
                    return (Int32)(SByte)o;
                } else if (type == ByteType) {
                    return (Int32)(Byte)o;
                } else if (type == SingleType) {
                    return (Int32)(Single)o;
                } else if (type == CharType) {
                    return (Int32)(Char)o;
                } else if (type == DecimalType) {
                    return (Int32)(Decimal)o;
                } else if (type.IsEnum) {
                    return (Int32)ExplicitCastEnumToInt32(o);
                } else if (type == NullableInt32Type) {
                    return (Int32)(Nullable<Int32>)o;
                } else if (type == NullableDoubleType) {
                    return (Int32)(Nullable<Double>)o;
                } else if (type == NullableInt64Type) {
                    return (Int32)(Nullable<Int64>)o;
                } else if (type == NullableInt16Type) {
                    return (Int32)(Nullable<Int16>)o;
                } else if (type == NullableUInt32Type) {
                    return (Int32)(Nullable<UInt32>)o;
                } else if (type == NullableUInt64Type) {
                    return (Int32)(Nullable<UInt64>)o;
                } else if (type == NullableUInt16Type) {
                    return (Int32)(Nullable<UInt16>)o;
                } else if (type == NullableSByteType) {
                    return (Int32)(Nullable<SByte>)o;
                } else if (type == NullableByteType) {
                    return (Int32)(Nullable<Byte>)o;
                } else if (type == NullableSingleType) {
                    return (Int32)(Nullable<Single>)o;
                } else if (type == NullableCharType) {
                    return (Int32)(Nullable<Char>)o;
                } else if (type == NullableDecimalType) {
                    return (Int32)(Nullable<Decimal>)o;
                }
            }
            throw InvalidCast(o, "Int32");
        }

        public static Int64 ExplicitCastToInt64(object o) {
            if (o != null) {
                Type type = o.GetType();
                if (type == Int32Type) {
                    return (Int64)(Int32)o;
                } else if (type == DoubleType) {
                    return (Int64)(Double)o;
                } else if (type == Int64Type) {
                    return (Int64)(Int64)o;
                } else if (type == Int16Type) {
                    return (Int64)(Int16)o;
                } else if (type == UInt32Type) {
                    return (Int64)(UInt32)o;
                } else if (type == UInt64Type) {
                    return (Int64)(UInt64)o;
                } else if (type == UInt16Type) {
                    return (Int64)(UInt16)o;
                } else if (type == SByteType) {
                    return (Int64)(SByte)o;
                } else if (type == ByteType) {
                    return (Int64)(Byte)o;
                } else if (type == SingleType) {
                    return (Int64)(Single)o;
                } else if (type == CharType) {
                    return (Int64)(Char)o;
                } else if (type == DecimalType) {
                    return (Int64)(Decimal)o;
                } else if (type.IsEnum) {
                    return (Int64)ExplicitCastEnumToInt64(o);
                } else if (type == NullableInt32Type) {
                    return (Int64)(Nullable<Int32>)o;
                } else if (type == NullableDoubleType) {
                    return (Int64)(Nullable<Double>)o;
                } else if (type == NullableInt64Type) {
                    return (Int64)(Nullable<Int64>)o;
                } else if (type == NullableInt16Type) {
                    return (Int64)(Nullable<Int16>)o;
                } else if (type == NullableUInt32Type) {
                    return (Int64)(Nullable<UInt32>)o;
                } else if (type == NullableUInt64Type) {
                    return (Int64)(Nullable<UInt64>)o;
                } else if (type == NullableUInt16Type) {
                    return (Int64)(Nullable<UInt16>)o;
                } else if (type == NullableSByteType) {
                    return (Int64)(Nullable<SByte>)o;
                } else if (type == NullableByteType) {
                    return (Int64)(Nullable<Byte>)o;
                } else if (type == NullableSingleType) {
                    return (Int64)(Nullable<Single>)o;
                } else if (type == NullableCharType) {
                    return (Int64)(Nullable<Char>)o;
                } else if (type == NullableDecimalType) {
                    return (Int64)(Nullable<Decimal>)o;
                }
            }
            throw InvalidCast(o, "Int64");
        }

        [CLSCompliant(false)]
        public static SByte ExplicitCastToSByte(object o) {
            if (o != null) {
                Type type = o.GetType();
                if (type == Int32Type) {
                    return (SByte)(Int32)o;
                } else if (type == DoubleType) {
                    return (SByte)(Double)o;
                } else if (type == Int64Type) {
                    return (SByte)(Int64)o;
                } else if (type == Int16Type) {
                    return (SByte)(Int16)o;
                } else if (type == UInt32Type) {
                    return (SByte)(UInt32)o;
                } else if (type == UInt64Type) {
                    return (SByte)(UInt64)o;
                } else if (type == UInt16Type) {
                    return (SByte)(UInt16)o;
                } else if (type == SByteType) {
                    return (SByte)(SByte)o;
                } else if (type == ByteType) {
                    return (SByte)(Byte)o;
                } else if (type == SingleType) {
                    return (SByte)(Single)o;
                } else if (type == CharType) {
                    return (SByte)(Char)o;
                } else if (type == DecimalType) {
                    return (SByte)(Decimal)o;
                } else if (type.IsEnum) {
                    return (SByte)ExplicitCastEnumToSByte(o);
                } else if (type == NullableInt32Type) {
                    return (SByte)(Nullable<Int32>)o;
                } else if (type == NullableDoubleType) {
                    return (SByte)(Nullable<Double>)o;
                } else if (type == NullableInt64Type) {
                    return (SByte)(Nullable<Int64>)o;
                } else if (type == NullableInt16Type) {
                    return (SByte)(Nullable<Int16>)o;
                } else if (type == NullableUInt32Type) {
                    return (SByte)(Nullable<UInt32>)o;
                } else if (type == NullableUInt64Type) {
                    return (SByte)(Nullable<UInt64>)o;
                } else if (type == NullableUInt16Type) {
                    return (SByte)(Nullable<UInt16>)o;
                } else if (type == NullableSByteType) {
                    return (SByte)(Nullable<SByte>)o;
                } else if (type == NullableByteType) {
                    return (SByte)(Nullable<Byte>)o;
                } else if (type == NullableSingleType) {
                    return (SByte)(Nullable<Single>)o;
                } else if (type == NullableCharType) {
                    return (SByte)(Nullable<Char>)o;
                } else if (type == NullableDecimalType) {
                    return (SByte)(Nullable<Decimal>)o;
                }
            }
            throw InvalidCast(o, "SByte");
        }

        public static Single ExplicitCastToSingle(object o) {
            if (o != null) {
                Type type = o.GetType();
                if (type == Int32Type) {
                    return (Single)(Int32)o;
                } else if (type == DoubleType) {
                    return (Single)(Double)o;
                } else if (type == Int64Type) {
                    return (Single)(Int64)o;
                } else if (type == Int16Type) {
                    return (Single)(Int16)o;
                } else if (type == UInt32Type) {
                    return (Single)(UInt32)o;
                } else if (type == UInt64Type) {
                    return (Single)(UInt64)o;
                } else if (type == UInt16Type) {
                    return (Single)(UInt16)o;
                } else if (type == SByteType) {
                    return (Single)(SByte)o;
                } else if (type == ByteType) {
                    return (Single)(Byte)o;
                } else if (type == SingleType) {
                    return (Single)(Single)o;
                } else if (type == CharType) {
                    return (Single)(Char)o;
                } else if (type == DecimalType) {
                    return (Single)(Decimal)o;
                } else if (type.IsEnum) {
                    return (Single)ExplicitCastEnumToInt64(o);
                } else if (type == NullableInt32Type) {
                    return (Single)(Nullable<Int32>)o;
                } else if (type == NullableDoubleType) {
                    return (Single)(Nullable<Double>)o;
                } else if (type == NullableInt64Type) {
                    return (Single)(Nullable<Int64>)o;
                } else if (type == NullableInt16Type) {
                    return (Single)(Nullable<Int16>)o;
                } else if (type == NullableUInt32Type) {
                    return (Single)(Nullable<UInt32>)o;
                } else if (type == NullableUInt64Type) {
                    return (Single)(Nullable<UInt64>)o;
                } else if (type == NullableUInt16Type) {
                    return (Single)(Nullable<UInt16>)o;
                } else if (type == NullableSByteType) {
                    return (Single)(Nullable<SByte>)o;
                } else if (type == NullableByteType) {
                    return (Single)(Nullable<Byte>)o;
                } else if (type == NullableSingleType) {
                    return (Single)(Nullable<Single>)o;
                } else if (type == NullableCharType) {
                    return (Single)(Nullable<Char>)o;
                } else if (type == NullableDecimalType) {
                    return (Single)(Nullable<Decimal>)o;
                }
            }
            throw InvalidCast(o, "Single");
        }

        [CLSCompliant(false)]
        public static UInt16 ExplicitCastToUInt16(object o) {
            if (o != null) {
                Type type = o.GetType();
                if (type == Int32Type) {
                    return (UInt16)(Int32)o;
                } else if (type == DoubleType) {
                    return (UInt16)(Double)o;
                } else if (type == Int64Type) {
                    return (UInt16)(Int64)o;
                } else if (type == Int16Type) {
                    return (UInt16)(Int16)o;
                } else if (type == UInt32Type) {
                    return (UInt16)(UInt32)o;
                } else if (type == UInt64Type) {
                    return (UInt16)(UInt64)o;
                } else if (type == UInt16Type) {
                    return (UInt16)(UInt16)o;
                } else if (type == SByteType) {
                    return (UInt16)(SByte)o;
                } else if (type == ByteType) {
                    return (UInt16)(Byte)o;
                } else if (type == SingleType) {
                    return (UInt16)(Single)o;
                } else if (type == CharType) {
                    return (UInt16)(Char)o;
                } else if (type == DecimalType) {
                    return (UInt16)(Decimal)o;
                } else if (type.IsEnum) {
                    return (UInt16)ExplicitCastEnumToUInt16(o);
                } else if (type == NullableInt32Type) {
                    return (UInt16)(Nullable<Int32>)o;
                } else if (type == NullableDoubleType) {
                    return (UInt16)(Nullable<Double>)o;
                } else if (type == NullableInt64Type) {
                    return (UInt16)(Nullable<Int64>)o;
                } else if (type == NullableInt16Type) {
                    return (UInt16)(Nullable<Int16>)o;
                } else if (type == NullableUInt32Type) {
                    return (UInt16)(Nullable<UInt32>)o;
                } else if (type == NullableUInt64Type) {
                    return (UInt16)(Nullable<UInt64>)o;
                } else if (type == NullableUInt16Type) {
                    return (UInt16)(Nullable<UInt16>)o;
                } else if (type == NullableSByteType) {
                    return (UInt16)(Nullable<SByte>)o;
                } else if (type == NullableByteType) {
                    return (UInt16)(Nullable<Byte>)o;
                } else if (type == NullableSingleType) {
                    return (UInt16)(Nullable<Single>)o;
                } else if (type == NullableCharType) {
                    return (UInt16)(Nullable<Char>)o;
                } else if (type == NullableDecimalType) {
                    return (UInt16)(Nullable<Decimal>)o;
                }
            }
            throw InvalidCast(o, "UInt16");
        }

        [CLSCompliant(false)]
        public static UInt32 ExplicitCastToUInt32(object o) {
            if (o != null) {
                Type type = o.GetType();
                if (type == Int32Type) {
                    return (UInt32)(Int32)o;
                } else if (type == DoubleType) {
                    return (UInt32)(Double)o;
                } else if (type == Int64Type) {
                    return (UInt32)(Int64)o;
                } else if (type == Int16Type) {
                    return (UInt32)(Int16)o;
                } else if (type == UInt32Type) {
                    return (UInt32)(UInt32)o;
                } else if (type == UInt64Type) {
                    return (UInt32)(UInt64)o;
                } else if (type == UInt16Type) {
                    return (UInt32)(UInt16)o;
                } else if (type == SByteType) {
                    return (UInt32)(SByte)o;
                } else if (type == ByteType) {
                    return (UInt32)(Byte)o;
                } else if (type == SingleType) {
                    return (UInt32)(Single)o;
                } else if (type == CharType) {
                    return (UInt32)(Char)o;
                } else if (type == DecimalType) {
                    return (UInt32)(Decimal)o;
                } else if (type.IsEnum) {
                    return (UInt32)ExplicitCastEnumToUInt32(o);
                } else if (type == NullableInt32Type) {
                    return (UInt32)(Nullable<Int32>)o;
                } else if (type == NullableDoubleType) {
                    return (UInt32)(Nullable<Double>)o;
                } else if (type == NullableInt64Type) {
                    return (UInt32)(Nullable<Int64>)o;
                } else if (type == NullableInt16Type) {
                    return (UInt32)(Nullable<Int16>)o;
                } else if (type == NullableUInt32Type) {
                    return (UInt32)(Nullable<UInt32>)o;
                } else if (type == NullableUInt64Type) {
                    return (UInt32)(Nullable<UInt64>)o;
                } else if (type == NullableUInt16Type) {
                    return (UInt32)(Nullable<UInt16>)o;
                } else if (type == NullableSByteType) {
                    return (UInt32)(Nullable<SByte>)o;
                } else if (type == NullableByteType) {
                    return (UInt32)(Nullable<Byte>)o;
                } else if (type == NullableSingleType) {
                    return (UInt32)(Nullable<Single>)o;
                } else if (type == NullableCharType) {
                    return (UInt32)(Nullable<Char>)o;
                } else if (type == NullableDecimalType) {
                    return (UInt32)(Nullable<Decimal>)o;
                }
            }
            throw InvalidCast(o, "UInt32");
        }

        [CLSCompliant(false)]
        public static UInt64 ExplicitCastToUInt64(object o) {
            if (o != null) {
                Type type = o.GetType();
                if (type == Int32Type) {
                    return (UInt64)(Int32)o;
                } else if (type == DoubleType) {
                    return (UInt64)(Double)o;
                } else if (type == Int64Type) {
                    return (UInt64)(Int64)o;
                } else if (type == Int16Type) {
                    return (UInt64)(Int16)o;
                } else if (type == UInt32Type) {
                    return (UInt64)(UInt32)o;
                } else if (type == UInt64Type) {
                    return (UInt64)(UInt64)o;
                } else if (type == UInt16Type) {
                    return (UInt64)(UInt16)o;
                } else if (type == SByteType) {
                    return (UInt64)(SByte)o;
                } else if (type == ByteType) {
                    return (UInt64)(Byte)o;
                } else if (type == SingleType) {
                    return (UInt64)(Single)o;
                } else if (type == CharType) {
                    return (UInt64)(Char)o;
                } else if (type == DecimalType) {
                    return (UInt64)(Decimal)o;
                } else if (type.IsEnum) {
                    return (UInt64)ExplicitCastEnumToUInt64(o);
                } else if (type == NullableInt32Type) {
                    return (UInt64)(Nullable<Int32>)o;
                } else if (type == NullableDoubleType) {
                    return (UInt64)(Nullable<Double>)o;
                } else if (type == NullableInt64Type) {
                    return (UInt64)(Nullable<Int64>)o;
                } else if (type == NullableInt16Type) {
                    return (UInt64)(Nullable<Int16>)o;
                } else if (type == NullableUInt32Type) {
                    return (UInt64)(Nullable<UInt32>)o;
                } else if (type == NullableUInt64Type) {
                    return (UInt64)(Nullable<UInt64>)o;
                } else if (type == NullableUInt16Type) {
                    return (UInt64)(Nullable<UInt16>)o;
                } else if (type == NullableSByteType) {
                    return (UInt64)(Nullable<SByte>)o;
                } else if (type == NullableByteType) {
                    return (UInt64)(Nullable<Byte>)o;
                } else if (type == NullableSingleType) {
                    return (UInt64)(Nullable<Single>)o;
                } else if (type == NullableCharType) {
                    return (UInt64)(Nullable<Char>)o;
                } else if (type == NullableDecimalType) {
                    return (UInt64)(Nullable<Decimal>)o;
                }
            }
            throw InvalidCast(o, "UInt64");
        }

        public static Nullable<Boolean> ExplicitCastToNullableBoolean(object o) {
            if (o == null) {
                return new Nullable<Boolean>();
            }
            Type type = o.GetType();
            if (type == BooleanType) {
                return (Nullable<Boolean>)(Boolean)o;
            } else if (type == NullableBooleanType) {
                return (Nullable<Boolean>)(Nullable<Boolean>)o;
            }
            throw InvalidCast(o, "Boolean");
        }

        public static Nullable<Byte> ExplicitCastToNullableByte(object o) {
            if (o == null) {
                return new Nullable<Byte>();
            }
            Type type = o.GetType();
            if (type == Int32Type) {
                return (Nullable<Byte>)(Int32)o;
            } else if (type == DoubleType) {
                return (Nullable<Byte>)(Double)o;
            } else if (type == Int64Type) {
                return (Nullable<Byte>)(Int64)o;
            } else if (type == Int16Type) {
                return (Nullable<Byte>)(Int16)o;
            } else if (type == UInt32Type) {
                return (Nullable<Byte>)(UInt32)o;
            } else if (type == UInt64Type) {
                return (Nullable<Byte>)(UInt64)o;
            } else if (type == UInt16Type) {
                return (Nullable<Byte>)(UInt16)o;
            } else if (type == SByteType) {
                return (Nullable<Byte>)(SByte)o;
            } else if (type == ByteType) {
                return (Nullable<Byte>)(Byte)o;
            } else if (type == SingleType) {
                return (Nullable<Byte>)(Single)o;
            } else if (type == CharType) {
                return (Nullable<Byte>)(Char)o;
            } else if (type == DecimalType) {
                return (Nullable<Byte>)(Decimal)o;
            } else if (type.IsEnum) {
                return (Nullable<Byte>)ExplicitCastEnumToByte(o);
            } else if (type == NullableInt32Type) {
                return (Nullable<Byte>)(Nullable<Int32>)o;
            } else if (type == NullableDoubleType) {
                return (Nullable<Byte>)(Nullable<Double>)o;
            } else if (type == NullableInt64Type) {
                return (Nullable<Byte>)(Nullable<Int64>)o;
            } else if (type == NullableInt16Type) {
                return (Nullable<Byte>)(Nullable<Int16>)o;
            } else if (type == NullableUInt32Type) {
                return (Nullable<Byte>)(Nullable<UInt32>)o;
            } else if (type == NullableUInt64Type) {
                return (Nullable<Byte>)(Nullable<UInt64>)o;
            } else if (type == NullableUInt16Type) {
                return (Nullable<Byte>)(Nullable<UInt16>)o;
            } else if (type == NullableSByteType) {
                return (Nullable<Byte>)(Nullable<SByte>)o;
            } else if (type == NullableByteType) {
                return (Nullable<Byte>)(Nullable<Byte>)o;
            } else if (type == NullableSingleType) {
                return (Nullable<Byte>)(Nullable<Single>)o;
            } else if (type == NullableCharType) {
                return (Nullable<Byte>)(Nullable<Char>)o;
            } else if (type == NullableDecimalType) {
                return (Nullable<Byte>)(Nullable<Decimal>)o;
            }
            throw InvalidCast(o, "Byte");
        }

        public static Nullable<Char> ExplicitCastToNullableChar(object o) {
            if (o == null) {
                return new Nullable<Char>();
            }
            Type type = o.GetType();
            if (type == Int32Type) {
                return (Nullable<Char>)(Int32)o;
            } else if (type == DoubleType) {
                return (Nullable<Char>)(Double)o;
            } else if (type == Int64Type) {
                return (Nullable<Char>)(Int64)o;
            } else if (type == Int16Type) {
                return (Nullable<Char>)(Int16)o;
            } else if (type == UInt32Type) {
                return (Nullable<Char>)(UInt32)o;
            } else if (type == UInt64Type) {
                return (Nullable<Char>)(UInt64)o;
            } else if (type == UInt16Type) {
                return (Nullable<Char>)(UInt16)o;
            } else if (type == SByteType) {
                return (Nullable<Char>)(SByte)o;
            } else if (type == ByteType) {
                return (Nullable<Char>)(Byte)o;
            } else if (type == SingleType) {
                return (Nullable<Char>)(Single)o;
            } else if (type == CharType) {
                return (Nullable<Char>)(Char)o;
            } else if (type == DecimalType) {
                return (Nullable<Char>)(Decimal)o;
            } else if (type.IsEnum) {
                return (Nullable<Char>)ExplicitCastEnumToInt32(o);
            } else if (type == NullableInt32Type) {
                return (Nullable<Char>)(Nullable<Int32>)o;
            } else if (type == NullableDoubleType) {
                return (Nullable<Char>)(Nullable<Double>)o;
            } else if (type == NullableInt64Type) {
                return (Nullable<Char>)(Nullable<Int64>)o;
            } else if (type == NullableInt16Type) {
                return (Nullable<Char>)(Nullable<Int16>)o;
            } else if (type == NullableUInt32Type) {
                return (Nullable<Char>)(Nullable<UInt32>)o;
            } else if (type == NullableUInt64Type) {
                return (Nullable<Char>)(Nullable<UInt64>)o;
            } else if (type == NullableUInt16Type) {
                return (Nullable<Char>)(Nullable<UInt16>)o;
            } else if (type == NullableSByteType) {
                return (Nullable<Char>)(Nullable<SByte>)o;
            } else if (type == NullableByteType) {
                return (Nullable<Char>)(Nullable<Byte>)o;
            } else if (type == NullableSingleType) {
                return (Nullable<Char>)(Nullable<Single>)o;
            } else if (type == NullableCharType) {
                return (Nullable<Char>)(Nullable<Char>)o;
            } else if (type == NullableDecimalType) {
                return (Nullable<Char>)(Nullable<Decimal>)o;
            }
            throw InvalidCast(o, "Char");
        }

        public static Nullable<Decimal> ExplicitCastToNullableDecimal(object o) {
            if (o == null) {
                return new Nullable<Decimal>();
            }
            Type type = o.GetType();
            if (type == Int32Type) {
                return (Nullable<Decimal>)(Int32)o;
            } else if (type == DoubleType) {
                return (Nullable<Decimal>)(Double)o;
            } else if (type == Int64Type) {
                return (Nullable<Decimal>)(Int64)o;
            } else if (type == Int16Type) {
                return (Nullable<Decimal>)(Int16)o;
            } else if (type == UInt32Type) {
                return (Nullable<Decimal>)(UInt32)o;
            } else if (type == UInt64Type) {
                return (Nullable<Decimal>)(UInt64)o;
            } else if (type == UInt16Type) {
                return (Nullable<Decimal>)(UInt16)o;
            } else if (type == SByteType) {
                return (Nullable<Decimal>)(SByte)o;
            } else if (type == ByteType) {
                return (Nullable<Decimal>)(Byte)o;
            } else if (type == SingleType) {
                return (Nullable<Decimal>)(Single)o;
            } else if (type == CharType) {
                return (Nullable<Decimal>)(Char)o;
            } else if (type == DecimalType) {
                return (Nullable<Decimal>)(Decimal)o;
            } else if (type.IsEnum) {
                return (Nullable<Decimal>)ExplicitCastEnumToInt64(o);
            } else if (type == NullableInt32Type) {
                return (Nullable<Decimal>)(Nullable<Int32>)o;
            } else if (type == NullableDoubleType) {
                return (Nullable<Decimal>)(Nullable<Double>)o;
            } else if (type == NullableInt64Type) {
                return (Nullable<Decimal>)(Nullable<Int64>)o;
            } else if (type == NullableInt16Type) {
                return (Nullable<Decimal>)(Nullable<Int16>)o;
            } else if (type == NullableUInt32Type) {
                return (Nullable<Decimal>)(Nullable<UInt32>)o;
            } else if (type == NullableUInt64Type) {
                return (Nullable<Decimal>)(Nullable<UInt64>)o;
            } else if (type == NullableUInt16Type) {
                return (Nullable<Decimal>)(Nullable<UInt16>)o;
            } else if (type == NullableSByteType) {
                return (Nullable<Decimal>)(Nullable<SByte>)o;
            } else if (type == NullableByteType) {
                return (Nullable<Decimal>)(Nullable<Byte>)o;
            } else if (type == NullableSingleType) {
                return (Nullable<Decimal>)(Nullable<Single>)o;
            } else if (type == NullableCharType) {
                return (Nullable<Decimal>)(Nullable<Char>)o;
            } else if (type == NullableDecimalType) {
                return (Nullable<Decimal>)(Nullable<Decimal>)o;
            }
            throw InvalidCast(o, "Decimal");
        }

        public static Nullable<Double> ExplicitCastToNullableDouble(object o) {
            if (o == null) {
                return new Nullable<Double>();
            }
            Type type = o.GetType();
            if (type == Int32Type) {
                return (Nullable<Double>)(Int32)o;
            } else if (type == DoubleType) {
                return (Nullable<Double>)(Double)o;
            } else if (type == Int64Type) {
                return (Nullable<Double>)(Int64)o;
            } else if (type == Int16Type) {
                return (Nullable<Double>)(Int16)o;
            } else if (type == UInt32Type) {
                return (Nullable<Double>)(UInt32)o;
            } else if (type == UInt64Type) {
                return (Nullable<Double>)(UInt64)o;
            } else if (type == UInt16Type) {
                return (Nullable<Double>)(UInt16)o;
            } else if (type == SByteType) {
                return (Nullable<Double>)(SByte)o;
            } else if (type == ByteType) {
                return (Nullable<Double>)(Byte)o;
            } else if (type == SingleType) {
                return (Nullable<Double>)(Single)o;
            } else if (type == CharType) {
                return (Nullable<Double>)(Char)o;
            } else if (type == DecimalType) {
                return (Nullable<Double>)(Decimal)o;
            } else if (type.IsEnum) {
                return (Nullable<Double>)ExplicitCastEnumToInt64(o);
            } else if (type == NullableInt32Type) {
                return (Nullable<Double>)(Nullable<Int32>)o;
            } else if (type == NullableDoubleType) {
                return (Nullable<Double>)(Nullable<Double>)o;
            } else if (type == NullableInt64Type) {
                return (Nullable<Double>)(Nullable<Int64>)o;
            } else if (type == NullableInt16Type) {
                return (Nullable<Double>)(Nullable<Int16>)o;
            } else if (type == NullableUInt32Type) {
                return (Nullable<Double>)(Nullable<UInt32>)o;
            } else if (type == NullableUInt64Type) {
                return (Nullable<Double>)(Nullable<UInt64>)o;
            } else if (type == NullableUInt16Type) {
                return (Nullable<Double>)(Nullable<UInt16>)o;
            } else if (type == NullableSByteType) {
                return (Nullable<Double>)(Nullable<SByte>)o;
            } else if (type == NullableByteType) {
                return (Nullable<Double>)(Nullable<Byte>)o;
            } else if (type == NullableSingleType) {
                return (Nullable<Double>)(Nullable<Single>)o;
            } else if (type == NullableCharType) {
                return (Nullable<Double>)(Nullable<Char>)o;
            } else if (type == NullableDecimalType) {
                return (Nullable<Double>)(Nullable<Decimal>)o;
            }
            throw InvalidCast(o, "Double");
        }

        public static Nullable<Int16> ExplicitCastToNullableInt16(object o) {
            if (o == null) {
                return new Nullable<Int16>();
            }
            Type type = o.GetType();
            if (type == Int32Type) {
                return (Nullable<Int16>)(Int32)o;
            } else if (type == DoubleType) {
                return (Nullable<Int16>)(Double)o;
            } else if (type == Int64Type) {
                return (Nullable<Int16>)(Int64)o;
            } else if (type == Int16Type) {
                return (Nullable<Int16>)(Int16)o;
            } else if (type == UInt32Type) {
                return (Nullable<Int16>)(UInt32)o;
            } else if (type == UInt64Type) {
                return (Nullable<Int16>)(UInt64)o;
            } else if (type == UInt16Type) {
                return (Nullable<Int16>)(UInt16)o;
            } else if (type == SByteType) {
                return (Nullable<Int16>)(SByte)o;
            } else if (type == ByteType) {
                return (Nullable<Int16>)(Byte)o;
            } else if (type == SingleType) {
                return (Nullable<Int16>)(Single)o;
            } else if (type == CharType) {
                return (Nullable<Int16>)(Char)o;
            } else if (type == DecimalType) {
                return (Nullable<Int16>)(Decimal)o;
            } else if (type.IsEnum) {
                return (Nullable<Int16>)ExplicitCastEnumToInt16(o);
            } else if (type == NullableInt32Type) {
                return (Nullable<Int16>)(Nullable<Int32>)o;
            } else if (type == NullableDoubleType) {
                return (Nullable<Int16>)(Nullable<Double>)o;
            } else if (type == NullableInt64Type) {
                return (Nullable<Int16>)(Nullable<Int64>)o;
            } else if (type == NullableInt16Type) {
                return (Nullable<Int16>)(Nullable<Int16>)o;
            } else if (type == NullableUInt32Type) {
                return (Nullable<Int16>)(Nullable<UInt32>)o;
            } else if (type == NullableUInt64Type) {
                return (Nullable<Int16>)(Nullable<UInt64>)o;
            } else if (type == NullableUInt16Type) {
                return (Nullable<Int16>)(Nullable<UInt16>)o;
            } else if (type == NullableSByteType) {
                return (Nullable<Int16>)(Nullable<SByte>)o;
            } else if (type == NullableByteType) {
                return (Nullable<Int16>)(Nullable<Byte>)o;
            } else if (type == NullableSingleType) {
                return (Nullable<Int16>)(Nullable<Single>)o;
            } else if (type == NullableCharType) {
                return (Nullable<Int16>)(Nullable<Char>)o;
            } else if (type == NullableDecimalType) {
                return (Nullable<Int16>)(Nullable<Decimal>)o;
            }
            throw InvalidCast(o, "Int16");
        }

        public static Nullable<Int32> ExplicitCastToNullableInt32(object o) {
            if (o == null) {
                return new Nullable<Int32>();
            }
            Type type = o.GetType();
            if (type == Int32Type) {
                return (Nullable<Int32>)(Int32)o;
            } else if (type == DoubleType) {
                return (Nullable<Int32>)(Double)o;
            } else if (type == Int64Type) {
                return (Nullable<Int32>)(Int64)o;
            } else if (type == Int16Type) {
                return (Nullable<Int32>)(Int16)o;
            } else if (type == UInt32Type) {
                return (Nullable<Int32>)(UInt32)o;
            } else if (type == UInt64Type) {
                return (Nullable<Int32>)(UInt64)o;
            } else if (type == UInt16Type) {
                return (Nullable<Int32>)(UInt16)o;
            } else if (type == SByteType) {
                return (Nullable<Int32>)(SByte)o;
            } else if (type == ByteType) {
                return (Nullable<Int32>)(Byte)o;
            } else if (type == SingleType) {
                return (Nullable<Int32>)(Single)o;
            } else if (type == CharType) {
                return (Nullable<Int32>)(Char)o;
            } else if (type == DecimalType) {
                return (Nullable<Int32>)(Decimal)o;
            } else if (type.IsEnum) {
                return (Nullable<Int32>)ExplicitCastEnumToInt32(o);
            } else if (type == NullableInt32Type) {
                return (Nullable<Int32>)(Nullable<Int32>)o;
            } else if (type == NullableDoubleType) {
                return (Nullable<Int32>)(Nullable<Double>)o;
            } else if (type == NullableInt64Type) {
                return (Nullable<Int32>)(Nullable<Int64>)o;
            } else if (type == NullableInt16Type) {
                return (Nullable<Int32>)(Nullable<Int16>)o;
            } else if (type == NullableUInt32Type) {
                return (Nullable<Int32>)(Nullable<UInt32>)o;
            } else if (type == NullableUInt64Type) {
                return (Nullable<Int32>)(Nullable<UInt64>)o;
            } else if (type == NullableUInt16Type) {
                return (Nullable<Int32>)(Nullable<UInt16>)o;
            } else if (type == NullableSByteType) {
                return (Nullable<Int32>)(Nullable<SByte>)o;
            } else if (type == NullableByteType) {
                return (Nullable<Int32>)(Nullable<Byte>)o;
            } else if (type == NullableSingleType) {
                return (Nullable<Int32>)(Nullable<Single>)o;
            } else if (type == NullableCharType) {
                return (Nullable<Int32>)(Nullable<Char>)o;
            } else if (type == NullableDecimalType) {
                return (Nullable<Int32>)(Nullable<Decimal>)o;
            }
            throw InvalidCast(o, "Int32");
        }

        public static Nullable<Int64> ExplicitCastToNullableInt64(object o) {
            if (o == null) {
                return new Nullable<Int64>();
            }
            Type type = o.GetType();
            if (type == Int32Type) {
                return (Nullable<Int64>)(Int32)o;
            } else if (type == DoubleType) {
                return (Nullable<Int64>)(Double)o;
            } else if (type == Int64Type) {
                return (Nullable<Int64>)(Int64)o;
            } else if (type == Int16Type) {
                return (Nullable<Int64>)(Int16)o;
            } else if (type == UInt32Type) {
                return (Nullable<Int64>)(UInt32)o;
            } else if (type == UInt64Type) {
                return (Nullable<Int64>)(UInt64)o;
            } else if (type == UInt16Type) {
                return (Nullable<Int64>)(UInt16)o;
            } else if (type == SByteType) {
                return (Nullable<Int64>)(SByte)o;
            } else if (type == ByteType) {
                return (Nullable<Int64>)(Byte)o;
            } else if (type == SingleType) {
                return (Nullable<Int64>)(Single)o;
            } else if (type == CharType) {
                return (Nullable<Int64>)(Char)o;
            } else if (type == DecimalType) {
                return (Nullable<Int64>)(Decimal)o;
            } else if (type.IsEnum) {
                return (Nullable<Int64>)ExplicitCastEnumToInt64(o);
            } else if (type == NullableInt32Type) {
                return (Nullable<Int64>)(Nullable<Int32>)o;
            } else if (type == NullableDoubleType) {
                return (Nullable<Int64>)(Nullable<Double>)o;
            } else if (type == NullableInt64Type) {
                return (Nullable<Int64>)(Nullable<Int64>)o;
            } else if (type == NullableInt16Type) {
                return (Nullable<Int64>)(Nullable<Int16>)o;
            } else if (type == NullableUInt32Type) {
                return (Nullable<Int64>)(Nullable<UInt32>)o;
            } else if (type == NullableUInt64Type) {
                return (Nullable<Int64>)(Nullable<UInt64>)o;
            } else if (type == NullableUInt16Type) {
                return (Nullable<Int64>)(Nullable<UInt16>)o;
            } else if (type == NullableSByteType) {
                return (Nullable<Int64>)(Nullable<SByte>)o;
            } else if (type == NullableByteType) {
                return (Nullable<Int64>)(Nullable<Byte>)o;
            } else if (type == NullableSingleType) {
                return (Nullable<Int64>)(Nullable<Single>)o;
            } else if (type == NullableCharType) {
                return (Nullable<Int64>)(Nullable<Char>)o;
            } else if (type == NullableDecimalType) {
                return (Nullable<Int64>)(Nullable<Decimal>)o;
            }
            throw InvalidCast(o, "Int64");
        }

        [CLSCompliant(false)]
        public static Nullable<SByte> ExplicitCastToNullableSByte(object o) {
            if (o == null) {
                return new Nullable<SByte>();
            }
            Type type = o.GetType();
            if (type == Int32Type) {
                return (Nullable<SByte>)(Int32)o;
            } else if (type == DoubleType) {
                return (Nullable<SByte>)(Double)o;
            } else if (type == Int64Type) {
                return (Nullable<SByte>)(Int64)o;
            } else if (type == Int16Type) {
                return (Nullable<SByte>)(Int16)o;
            } else if (type == UInt32Type) {
                return (Nullable<SByte>)(UInt32)o;
            } else if (type == UInt64Type) {
                return (Nullable<SByte>)(UInt64)o;
            } else if (type == UInt16Type) {
                return (Nullable<SByte>)(UInt16)o;
            } else if (type == SByteType) {
                return (Nullable<SByte>)(SByte)o;
            } else if (type == ByteType) {
                return (Nullable<SByte>)(Byte)o;
            } else if (type == SingleType) {
                return (Nullable<SByte>)(Single)o;
            } else if (type == CharType) {
                return (Nullable<SByte>)(Char)o;
            } else if (type == DecimalType) {
                return (Nullable<SByte>)(Decimal)o;
            } else if (type.IsEnum) {
                return (Nullable<SByte>)ExplicitCastEnumToSByte(o);
            } else if (type == NullableInt32Type) {
                return (Nullable<SByte>)(Nullable<Int32>)o;
            } else if (type == NullableDoubleType) {
                return (Nullable<SByte>)(Nullable<Double>)o;
            } else if (type == NullableInt64Type) {
                return (Nullable<SByte>)(Nullable<Int64>)o;
            } else if (type == NullableInt16Type) {
                return (Nullable<SByte>)(Nullable<Int16>)o;
            } else if (type == NullableUInt32Type) {
                return (Nullable<SByte>)(Nullable<UInt32>)o;
            } else if (type == NullableUInt64Type) {
                return (Nullable<SByte>)(Nullable<UInt64>)o;
            } else if (type == NullableUInt16Type) {
                return (Nullable<SByte>)(Nullable<UInt16>)o;
            } else if (type == NullableSByteType) {
                return (Nullable<SByte>)(Nullable<SByte>)o;
            } else if (type == NullableByteType) {
                return (Nullable<SByte>)(Nullable<Byte>)o;
            } else if (type == NullableSingleType) {
                return (Nullable<SByte>)(Nullable<Single>)o;
            } else if (type == NullableCharType) {
                return (Nullable<SByte>)(Nullable<Char>)o;
            } else if (type == NullableDecimalType) {
                return (Nullable<SByte>)(Nullable<Decimal>)o;
            }
            throw InvalidCast(o, "SByte");
        }

        public static Nullable<Single> ExplicitCastToNullableSingle(object o) {
            if (o == null) {
                return new Nullable<Single>();
            }
            Type type = o.GetType();
            if (type == Int32Type) {
                return (Nullable<Single>)(Int32)o;
            } else if (type == DoubleType) {
                return (Nullable<Single>)(Double)o;
            } else if (type == Int64Type) {
                return (Nullable<Single>)(Int64)o;
            } else if (type == Int16Type) {
                return (Nullable<Single>)(Int16)o;
            } else if (type == UInt32Type) {
                return (Nullable<Single>)(UInt32)o;
            } else if (type == UInt64Type) {
                return (Nullable<Single>)(UInt64)o;
            } else if (type == UInt16Type) {
                return (Nullable<Single>)(UInt16)o;
            } else if (type == SByteType) {
                return (Nullable<Single>)(SByte)o;
            } else if (type == ByteType) {
                return (Nullable<Single>)(Byte)o;
            } else if (type == SingleType) {
                return (Nullable<Single>)(Single)o;
            } else if (type == CharType) {
                return (Nullable<Single>)(Char)o;
            } else if (type == DecimalType) {
                return (Nullable<Single>)(Decimal)o;
            } else if (type.IsEnum) {
                return (Nullable<Single>)ExplicitCastEnumToInt64(o);
            } else if (type == NullableInt32Type) {
                return (Nullable<Single>)(Nullable<Int32>)o;
            } else if (type == NullableDoubleType) {
                return (Nullable<Single>)(Nullable<Double>)o;
            } else if (type == NullableInt64Type) {
                return (Nullable<Single>)(Nullable<Int64>)o;
            } else if (type == NullableInt16Type) {
                return (Nullable<Single>)(Nullable<Int16>)o;
            } else if (type == NullableUInt32Type) {
                return (Nullable<Single>)(Nullable<UInt32>)o;
            } else if (type == NullableUInt64Type) {
                return (Nullable<Single>)(Nullable<UInt64>)o;
            } else if (type == NullableUInt16Type) {
                return (Nullable<Single>)(Nullable<UInt16>)o;
            } else if (type == NullableSByteType) {
                return (Nullable<Single>)(Nullable<SByte>)o;
            } else if (type == NullableByteType) {
                return (Nullable<Single>)(Nullable<Byte>)o;
            } else if (type == NullableSingleType) {
                return (Nullable<Single>)(Nullable<Single>)o;
            } else if (type == NullableCharType) {
                return (Nullable<Single>)(Nullable<Char>)o;
            } else if (type == NullableDecimalType) {
                return (Nullable<Single>)(Nullable<Decimal>)o;
            }
            throw InvalidCast(o, "Single");
        }

        [CLSCompliant(false)]
        public static Nullable<UInt16> ExplicitCastToNullableUInt16(object o) {
            if (o == null) {
                return new Nullable<UInt16>();
            }
            Type type = o.GetType();
            if (type == Int32Type) {
                return (Nullable<UInt16>)(Int32)o;
            } else if (type == DoubleType) {
                return (Nullable<UInt16>)(Double)o;
            } else if (type == Int64Type) {
                return (Nullable<UInt16>)(Int64)o;
            } else if (type == Int16Type) {
                return (Nullable<UInt16>)(Int16)o;
            } else if (type == UInt32Type) {
                return (Nullable<UInt16>)(UInt32)o;
            } else if (type == UInt64Type) {
                return (Nullable<UInt16>)(UInt64)o;
            } else if (type == UInt16Type) {
                return (Nullable<UInt16>)(UInt16)o;
            } else if (type == SByteType) {
                return (Nullable<UInt16>)(SByte)o;
            } else if (type == ByteType) {
                return (Nullable<UInt16>)(Byte)o;
            } else if (type == SingleType) {
                return (Nullable<UInt16>)(Single)o;
            } else if (type == CharType) {
                return (Nullable<UInt16>)(Char)o;
            } else if (type == DecimalType) {
                return (Nullable<UInt16>)(Decimal)o;
            } else if (type.IsEnum) {
                return (Nullable<UInt16>)ExplicitCastEnumToUInt16(o);
            } else if (type == NullableInt32Type) {
                return (Nullable<UInt16>)(Nullable<Int32>)o;
            } else if (type == NullableDoubleType) {
                return (Nullable<UInt16>)(Nullable<Double>)o;
            } else if (type == NullableInt64Type) {
                return (Nullable<UInt16>)(Nullable<Int64>)o;
            } else if (type == NullableInt16Type) {
                return (Nullable<UInt16>)(Nullable<Int16>)o;
            } else if (type == NullableUInt32Type) {
                return (Nullable<UInt16>)(Nullable<UInt32>)o;
            } else if (type == NullableUInt64Type) {
                return (Nullable<UInt16>)(Nullable<UInt64>)o;
            } else if (type == NullableUInt16Type) {
                return (Nullable<UInt16>)(Nullable<UInt16>)o;
            } else if (type == NullableSByteType) {
                return (Nullable<UInt16>)(Nullable<SByte>)o;
            } else if (type == NullableByteType) {
                return (Nullable<UInt16>)(Nullable<Byte>)o;
            } else if (type == NullableSingleType) {
                return (Nullable<UInt16>)(Nullable<Single>)o;
            } else if (type == NullableCharType) {
                return (Nullable<UInt16>)(Nullable<Char>)o;
            } else if (type == NullableDecimalType) {
                return (Nullable<UInt16>)(Nullable<Decimal>)o;
            }
            throw InvalidCast(o, "UInt16");
        }

        [CLSCompliant(false)]
        public static Nullable<UInt32> ExplicitCastToNullableUInt32(object o) {
            if (o == null) {
                return new Nullable<UInt32>();
            }
            Type type = o.GetType();
            if (type == Int32Type) {
                return (Nullable<UInt32>)(Int32)o;
            } else if (type == DoubleType) {
                return (Nullable<UInt32>)(Double)o;
            } else if (type == Int64Type) {
                return (Nullable<UInt32>)(Int64)o;
            } else if (type == Int16Type) {
                return (Nullable<UInt32>)(Int16)o;
            } else if (type == UInt32Type) {
                return (Nullable<UInt32>)(UInt32)o;
            } else if (type == UInt64Type) {
                return (Nullable<UInt32>)(UInt64)o;
            } else if (type == UInt16Type) {
                return (Nullable<UInt32>)(UInt16)o;
            } else if (type == SByteType) {
                return (Nullable<UInt32>)(SByte)o;
            } else if (type == ByteType) {
                return (Nullable<UInt32>)(Byte)o;
            } else if (type == SingleType) {
                return (Nullable<UInt32>)(Single)o;
            } else if (type == CharType) {
                return (Nullable<UInt32>)(Char)o;
            } else if (type == DecimalType) {
                return (Nullable<UInt32>)(Decimal)o;
            } else if (type.IsEnum) {
                return (Nullable<UInt32>)ExplicitCastEnumToUInt32(o);
            } else if (type == NullableInt32Type) {
                return (Nullable<UInt32>)(Nullable<Int32>)o;
            } else if (type == NullableDoubleType) {
                return (Nullable<UInt32>)(Nullable<Double>)o;
            } else if (type == NullableInt64Type) {
                return (Nullable<UInt32>)(Nullable<Int64>)o;
            } else if (type == NullableInt16Type) {
                return (Nullable<UInt32>)(Nullable<Int16>)o;
            } else if (type == NullableUInt32Type) {
                return (Nullable<UInt32>)(Nullable<UInt32>)o;
            } else if (type == NullableUInt64Type) {
                return (Nullable<UInt32>)(Nullable<UInt64>)o;
            } else if (type == NullableUInt16Type) {
                return (Nullable<UInt32>)(Nullable<UInt16>)o;
            } else if (type == NullableSByteType) {
                return (Nullable<UInt32>)(Nullable<SByte>)o;
            } else if (type == NullableByteType) {
                return (Nullable<UInt32>)(Nullable<Byte>)o;
            } else if (type == NullableSingleType) {
                return (Nullable<UInt32>)(Nullable<Single>)o;
            } else if (type == NullableCharType) {
                return (Nullable<UInt32>)(Nullable<Char>)o;
            } else if (type == NullableDecimalType) {
                return (Nullable<UInt32>)(Nullable<Decimal>)o;
            }
            throw InvalidCast(o, "UInt32");
        }

        [CLSCompliant(false)]
        public static Nullable<UInt64> ExplicitCastToNullableUInt64(object o) {
            if (o == null) {
                return new Nullable<UInt64>();
            }
            Type type = o.GetType();
            if (type == Int32Type) {
                return (Nullable<UInt64>)(Int32)o;
            } else if (type == DoubleType) {
                return (Nullable<UInt64>)(Double)o;
            } else if (type == Int64Type) {
                return (Nullable<UInt64>)(Int64)o;
            } else if (type == Int16Type) {
                return (Nullable<UInt64>)(Int16)o;
            } else if (type == UInt32Type) {
                return (Nullable<UInt64>)(UInt32)o;
            } else if (type == UInt64Type) {
                return (Nullable<UInt64>)(UInt64)o;
            } else if (type == UInt16Type) {
                return (Nullable<UInt64>)(UInt16)o;
            } else if (type == SByteType) {
                return (Nullable<UInt64>)(SByte)o;
            } else if (type == ByteType) {
                return (Nullable<UInt64>)(Byte)o;
            } else if (type == SingleType) {
                return (Nullable<UInt64>)(Single)o;
            } else if (type == CharType) {
                return (Nullable<UInt64>)(Char)o;
            } else if (type == DecimalType) {
                return (Nullable<UInt64>)(Decimal)o;
            } else if (type.IsEnum) {
                return (Nullable<UInt64>)ExplicitCastEnumToUInt64(o);
            } else if (type == NullableInt32Type) {
                return (Nullable<UInt64>)(Nullable<Int32>)o;
            } else if (type == NullableDoubleType) {
                return (Nullable<UInt64>)(Nullable<Double>)o;
            } else if (type == NullableInt64Type) {
                return (Nullable<UInt64>)(Nullable<Int64>)o;
            } else if (type == NullableInt16Type) {
                return (Nullable<UInt64>)(Nullable<Int16>)o;
            } else if (type == NullableUInt32Type) {
                return (Nullable<UInt64>)(Nullable<UInt32>)o;
            } else if (type == NullableUInt64Type) {
                return (Nullable<UInt64>)(Nullable<UInt64>)o;
            } else if (type == NullableUInt16Type) {
                return (Nullable<UInt64>)(Nullable<UInt16>)o;
            } else if (type == NullableSByteType) {
                return (Nullable<UInt64>)(Nullable<SByte>)o;
            } else if (type == NullableByteType) {
                return (Nullable<UInt64>)(Nullable<Byte>)o;
            } else if (type == NullableSingleType) {
                return (Nullable<UInt64>)(Nullable<Single>)o;
            } else if (type == NullableCharType) {
                return (Nullable<UInt64>)(Nullable<Char>)o;
            } else if (type == NullableDecimalType) {
                return (Nullable<UInt64>)(Nullable<Decimal>)o;
            }
            throw InvalidCast(o, "UInt64");
        }


        // *** END GENERATED CODE ***

        #endregion

        #region Generated Nullable Instance

        // *** BEGIN GENERATED CODE ***

        public static object NewNullableInstance(Type type) {
            if (type == Int32Type) {
                return new Nullable<Int32>();
            } else if (type == DoubleType) {
                return new Nullable<Double>();
            } else if (type == BooleanType) {
                return new Nullable<Boolean>();
            } else if (type == Int64Type) {
                return new Nullable<Int64>();
            } else if (type == Int16Type) {
                return new Nullable<Int16>();
            } else if (type == UInt32Type) {
                return new Nullable<UInt32>();
            } else if (type == UInt64Type) {
                return new Nullable<UInt64>();
            } else if (type == UInt16Type) {
                return new Nullable<UInt16>();
            } else if (type == SByteType) {
                return new Nullable<SByte>();
            } else if (type == ByteType) {
                return new Nullable<Byte>();
            } else if (type == SingleType) {
                return new Nullable<Single>();
            } else if (type == CharType) {
                return new Nullable<Char>();
            } else if (type == DecimalType) {
                return new Nullable<Decimal>();
            } else {
                return NewNullableInstanceSlow(type);
            }
        }

        // *** END GENERATED CODE ***

        #endregion

        #region Generated Enum Casts

        // *** BEGIN GENERATED CODE ***

        internal static Byte ExplicitCastEnumToByte(object o) {
            Debug.Assert(o is Enum);
            switch (((Enum)o).GetTypeCode()) {
                case TypeCode.Byte: return (Byte)(Byte)o;
                case TypeCode.SByte: return (Byte)(SByte)o;
                case TypeCode.Int16: return (Byte)(Int16)o;
                case TypeCode.UInt16: return (Byte)(UInt16)o;
                case TypeCode.Int32: return (Byte)(Int32)o;
                case TypeCode.UInt32: return (Byte)(UInt32)o;
                case TypeCode.Int64: return (Byte)(Int64)o;
                case TypeCode.UInt64: return (Byte)(UInt64)o;
            }
            throw new InvalidOperationException("Invalid enum");
        }

        internal static SByte ExplicitCastEnumToSByte(object o) {
            Debug.Assert(o is Enum);
            switch (((Enum)o).GetTypeCode()) {
                case TypeCode.Byte: return (SByte)(Byte)o;
                case TypeCode.SByte: return (SByte)(SByte)o;
                case TypeCode.Int16: return (SByte)(Int16)o;
                case TypeCode.UInt16: return (SByte)(UInt16)o;
                case TypeCode.Int32: return (SByte)(Int32)o;
                case TypeCode.UInt32: return (SByte)(UInt32)o;
                case TypeCode.Int64: return (SByte)(Int64)o;
                case TypeCode.UInt64: return (SByte)(UInt64)o;
            }
            throw new InvalidOperationException("Invalid enum");
        }

        internal static Int16 ExplicitCastEnumToInt16(object o) {
            Debug.Assert(o is Enum);
            switch (((Enum)o).GetTypeCode()) {
                case TypeCode.Byte: return (Int16)(Byte)o;
                case TypeCode.SByte: return (Int16)(SByte)o;
                case TypeCode.Int16: return (Int16)(Int16)o;
                case TypeCode.UInt16: return (Int16)(UInt16)o;
                case TypeCode.Int32: return (Int16)(Int32)o;
                case TypeCode.UInt32: return (Int16)(UInt32)o;
                case TypeCode.Int64: return (Int16)(Int64)o;
                case TypeCode.UInt64: return (Int16)(UInt64)o;
            }
            throw new InvalidOperationException("Invalid enum");
        }

        internal static UInt16 ExplicitCastEnumToUInt16(object o) {
            Debug.Assert(o is Enum);
            switch (((Enum)o).GetTypeCode()) {
                case TypeCode.Byte: return (UInt16)(Byte)o;
                case TypeCode.SByte: return (UInt16)(SByte)o;
                case TypeCode.Int16: return (UInt16)(Int16)o;
                case TypeCode.UInt16: return (UInt16)(UInt16)o;
                case TypeCode.Int32: return (UInt16)(Int32)o;
                case TypeCode.UInt32: return (UInt16)(UInt32)o;
                case TypeCode.Int64: return (UInt16)(Int64)o;
                case TypeCode.UInt64: return (UInt16)(UInt64)o;
            }
            throw new InvalidOperationException("Invalid enum");
        }

        internal static Int32 ExplicitCastEnumToInt32(object o) {
            Debug.Assert(o is Enum);
            switch (((Enum)o).GetTypeCode()) {
                case TypeCode.Byte: return (Int32)(Byte)o;
                case TypeCode.SByte: return (Int32)(SByte)o;
                case TypeCode.Int16: return (Int32)(Int16)o;
                case TypeCode.UInt16: return (Int32)(UInt16)o;
                case TypeCode.Int32: return (Int32)(Int32)o;
                case TypeCode.UInt32: return (Int32)(UInt32)o;
                case TypeCode.Int64: return (Int32)(Int64)o;
                case TypeCode.UInt64: return (Int32)(UInt64)o;
            }
            throw new InvalidOperationException("Invalid enum");
        }

        internal static UInt32 ExplicitCastEnumToUInt32(object o) {
            Debug.Assert(o is Enum);
            switch (((Enum)o).GetTypeCode()) {
                case TypeCode.Byte: return (UInt32)(Byte)o;
                case TypeCode.SByte: return (UInt32)(SByte)o;
                case TypeCode.Int16: return (UInt32)(Int16)o;
                case TypeCode.UInt16: return (UInt32)(UInt16)o;
                case TypeCode.Int32: return (UInt32)(Int32)o;
                case TypeCode.UInt32: return (UInt32)(UInt32)o;
                case TypeCode.Int64: return (UInt32)(Int64)o;
                case TypeCode.UInt64: return (UInt32)(UInt64)o;
            }
            throw new InvalidOperationException("Invalid enum");
        }

        internal static Int64 ExplicitCastEnumToInt64(object o) {
            Debug.Assert(o is Enum);
            switch (((Enum)o).GetTypeCode()) {
                case TypeCode.Byte: return (Int64)(Byte)o;
                case TypeCode.SByte: return (Int64)(SByte)o;
                case TypeCode.Int16: return (Int64)(Int16)o;
                case TypeCode.UInt16: return (Int64)(UInt16)o;
                case TypeCode.Int32: return (Int64)(Int32)o;
                case TypeCode.UInt32: return (Int64)(UInt32)o;
                case TypeCode.Int64: return (Int64)(Int64)o;
                case TypeCode.UInt64: return (Int64)(UInt64)o;
            }
            throw new InvalidOperationException("Invalid enum");
        }

        internal static UInt64 ExplicitCastEnumToUInt64(object o) {
            Debug.Assert(o is Enum);
            switch (((Enum)o).GetTypeCode()) {
                case TypeCode.Byte: return (UInt64)(Byte)o;
                case TypeCode.SByte: return (UInt64)(SByte)o;
                case TypeCode.Int16: return (UInt64)(Int16)o;
                case TypeCode.UInt16: return (UInt64)(UInt16)o;
                case TypeCode.Int32: return (UInt64)(Int32)o;
                case TypeCode.UInt32: return (UInt64)(UInt32)o;
                case TypeCode.Int64: return (UInt64)(Int64)o;
                case TypeCode.UInt64: return (UInt64)(UInt64)o;
            }
            throw new InvalidOperationException("Invalid enum");
        }


        // *** END GENERATED CODE ***

        #endregion

        #region Generated Type Cache

        // *** BEGIN GENERATED CODE ***

        internal static readonly Type BooleanType = typeof(Boolean);
        internal static readonly Type ByteType = typeof(Byte);
        internal static readonly Type CharType = typeof(Char);
        internal static readonly Type DecimalType = typeof(Decimal);
        internal static readonly Type DoubleType = typeof(Double);
        internal static readonly Type Int16Type = typeof(Int16);
        internal static readonly Type Int32Type = typeof(Int32);
        internal static readonly Type Int64Type = typeof(Int64);
        internal static readonly Type ObjectType = typeof(Object);
        internal static readonly Type SByteType = typeof(SByte);
        internal static readonly Type SingleType = typeof(Single);
        internal static readonly Type UInt16Type = typeof(UInt16);
        internal static readonly Type UInt32Type = typeof(UInt32);
        internal static readonly Type UInt64Type = typeof(UInt64);

        internal static readonly Type NullableBooleanType = typeof(Nullable<Boolean>);
        internal static readonly Type NullableByteType = typeof(Nullable<Byte>);
        internal static readonly Type NullableCharType = typeof(Nullable<Char>);
        internal static readonly Type NullableDecimalType = typeof(Nullable<Decimal>);
        internal static readonly Type NullableDoubleType = typeof(Nullable<Double>);
        internal static readonly Type NullableInt16Type = typeof(Nullable<Int16>);
        internal static readonly Type NullableInt32Type = typeof(Nullable<Int32>);
        internal static readonly Type NullableInt64Type = typeof(Nullable<Int64>);
        internal static readonly Type NullableSByteType = typeof(Nullable<SByte>);
        internal static readonly Type NullableSingleType = typeof(Nullable<Single>);
        internal static readonly Type NullableUInt16Type = typeof(Nullable<UInt16>);
        internal static readonly Type NullableUInt32Type = typeof(Nullable<UInt32>);
        internal static readonly Type NullableUInt64Type = typeof(Nullable<UInt64>);

        // *** END GENERATED CODE ***

        #endregion

        internal static readonly Type NullableType = typeof(Nullable<>);
    }
}
