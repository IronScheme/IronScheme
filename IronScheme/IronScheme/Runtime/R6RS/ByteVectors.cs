#region License
/* ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 
 *
 * This source code is subject to terms and conditions of the Microsoft Public License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Public License.
 *
 * You must not remove this notice, or any other, from this software.
 * ***************************************************************************/
#endregion

#if R6RS
using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting.Generation;
using Microsoft.Scripting;
using System.Reflection;
using Microsoft.Scripting.Utils;
using System.Reflection.Emit;
using System.Collections;

namespace IronScheme.Runtime.R6RS
{
  public class ByteVectors : Builtins
  {

    [Builtin("bytevector?")]
    public static object IsByteVector(object obj)
    {
      return obj is byte[];
    }

    [Builtin("make-bytevector")]
    public static object MakeByteVector(object k)
    {
      int i = RequiresNotNull<int>(k);
      return new byte[i];
    }

    [Builtin("make-bytevector")]
    public static object MakeByteVector(object k, object fill)
    {
      int i = RequiresNotNull<int>(k);
      int c = RequiresNotNull<int>(fill);
      byte[] b = new byte[i];

      for (i = 0; i < b.Length; i++)
      {
        b[i] = (byte)c;
      }
      return b;
    }


    [Builtin("bytevector-length")]
    public static object ByteVectorLength(object obj)
    {
      byte[] v = RequiresNotNull<byte[]>(obj);
      return v.Length;
    }

    [Builtin("bytevector=?")]
    public static object IsByteVectorEqual(object v1, object v2)
    {
      byte[] bv1 = RequiresNotNull<byte[]>(v1);
      byte[] bv2 = RequiresNotNull<byte[]>(v2);

      if (bv1.Length == bv2.Length)
      {
        for (int i = 0; i < bv1.Length; i++)
        {
          if (bv1[i] != bv2[i])
          {
            return false;
          }
        }

        return true;
      }

      return false;
    }

    [Builtin("bytevector-fill!")]
    public static object ByteVectorFill(object v, object fill)
    {
      byte[] b = RequiresNotNull<byte[]>(v);
      int c = RequiresNotNull<int>(fill);

      for (int i = 0; i < b.Length; i++)
      {
        b[i] = (byte)c;
      }
      return Unspecified;
    }

    [Builtin("bytevector-copy!")]
    public static object ByteVectorCopy(object v1, object start1, object v2, object start2, object k)
    {
      byte[] b1 = RequiresNotNull<byte[]>(v1);
      byte[] b2 = RequiresNotNull<byte[]>(v2);

      int s1 = RequiresNotNull<int>(start1);
      int s2 = RequiresNotNull<int>(start2);
      int i = RequiresNotNull<int>(k);

      Buffer.BlockCopy(b1, s1, b2, s2, i);

      return Unspecified;
    }

    [Builtin("bytevector-copy")]
    public static object ByteVectorCopy(object v1)
    {
      byte[] b1 = RequiresNotNull<byte[]>(v1);
      return b1.Clone();
    }


    [Builtin("bytevector-u8-ref")]
    public static object ByteVectorU8Ref(object v1, object k)
    {
      byte[] b1 = RequiresNotNull<byte[]>(v1);
      int i = RequiresNotNull<int>(k);

      return b1[i];
    }

    [Builtin("bytevector-s8-ref")]
    public static object ByteVectorS8Ref(object v1, object k)
    {
      byte[] b1 = RequiresNotNull<byte[]>(v1);
      int i = RequiresNotNull<int>(k);

      return (sbyte) b1[i];
    }

    [Builtin("bytevector-u8-set!")]
    public static object ByteVectorU8Set(object v1, object k, object octet)
    {
      byte[] b1 = RequiresNotNull<byte[]>(v1);
      int i = RequiresNotNull<int>(k);
      int b = RequiresNotNull<int>(octet);

      b1[i] = (byte)b;

      return Unspecified;
    }

    [Builtin("bytevector-s8-set!")]
    public static object ByteVectorS8Set(object v1, object k, object @byte)
    {
      byte[] b1 = RequiresNotNull<byte[]>(v1);
      int i = RequiresNotNull<int>(k);
      int b = RequiresNotNull<int>(@byte);

      b1[i] = (byte)~b;

      return Unspecified;
    }

    [Builtin("bytevector->u8-list")]
    public static object ByteVectorToU8List(object v1)
    {
      byte[] b1 = RequiresNotNull<byte[]>(v1);
      return Runtime.Cons.FromList(b1);
    }

    [Builtin("u8-list->bytevector")]
    public static object U8ListToByteVector(object obj)
    {
      object[] bytes = ListToVector(obj) as object[];
      byte[] buffer = new byte[bytes.Length];
      for (int i = 0; i < buffer.Length; i++)
      {
        buffer[i] = Convert.ToByte(bytes[i]);
      }

      return buffer;
    }

    [Builtin("string->utf8")]
    public static object StringToUTF8(object str)
    {
      string s = GetString(str);
      return Encoding.UTF8.GetBytes(s);
    }

    static Encoding UTF16BE = new UnicodeEncoding(true, false);
    static Encoding UTF16LE = new UnicodeEncoding(false, false);

    [Builtin("string->utf16")]
    public static object StringToUTF16(object str)
    {
      string s = GetString(str);
      return UTF16BE.GetBytes(s);
    }

    [Builtin("string->utf16")]
    public static object StringToUTF16(object str, object endianess)
    {
      string s = GetString(str);
      SymbolId end = RequiresNotNull<SymbolId>(endianess);
      if (end == SymbolTable.StringToId("big"))
      {
        return UTF16BE.GetBytes(s);
      }
      else
      {
        return UTF16LE.GetBytes(s);
      }
    }

    static Encoding UTF32BE = new UTF32Encoding(true, false);
    static Encoding UTF32LE = new UTF32Encoding(false, false);

    [Builtin("string->utf32")]
    public static object StringToUTF32(object str)
    {
      string s = GetString(str);
      return UTF32BE.GetBytes(s);
    }

    [Builtin("string->utf32")]
    public static object StringToUTF32(object str, object endianess)
    {
      string s = GetString(str);
      SymbolId end = RequiresNotNull<SymbolId>(endianess);
      if (end == SymbolTable.StringToId("big"))
      {
        return UTF32BE.GetBytes(s);
      }
      else
      {
        return UTF32LE.GetBytes(s);
      }
    }

    [Builtin("utf8->string")]
    public static object UTF8ToString(object v)
    {
      byte[] b = RequiresNotNull<byte[]>(v);
      return Encoding.UTF8.GetString(b);
    }


    [Builtin("utf16->string")]
    public static object UTF16ToString(object v, object endianess, object isendianmandatory)
    {
      byte[] b = RequiresNotNull<byte[]>(v);
      SymbolId end = RequiresNotNull<SymbolId>(endianess);

      if (end == SymbolTable.StringToId("big"))
      {
        return UTF16BE.GetString(b);
      }
      else
      {
        return UTF16LE.GetString(b);
      }
    }

    [Builtin("utf16->string")]
    public static object UTF16ToString(object v, object endianess)
    {
      byte[] b = RequiresNotNull<byte[]>(v);
      SymbolId end = RequiresNotNull<SymbolId>(endianess);

      if (end == SymbolTable.StringToId("big"))
      {
        return UTF16BE.GetString(b);
      }
      else
      {
        return UTF16LE.GetString(b);
      }
    }


    [Builtin("utf32->string")]
    public static object UTF32ToString(object v, object endianess, object isendianmandatory)
    {
      byte[] b = RequiresNotNull<byte[]>(v);
      SymbolId end = RequiresNotNull<SymbolId>(endianess);

      if (end == SymbolTable.StringToId("big"))
      {
        return UTF32BE.GetString(b);
      }
      else
      {
        return UTF32LE.GetString(b);
      }
    }

    [Builtin("utf32->string")]
    public static object UTF32ToString(object v, object endianess)
    {
      byte[] b = RequiresNotNull<byte[]>(v);
      SymbolId end = RequiresNotNull<SymbolId>(endianess);

      if (end == SymbolTable.StringToId("big"))
      {
        return UTF32BE.GetString(b);
      }
      else
      {
        return UTF32LE.GetString(b);
      }
    }

    /* TODO!!!!
     * 
     * (bytevector-u16-ref bytevector k endianness)    procedure 
(bytevector-s16-ref bytevector k endianness)    procedure 
(bytevector-u16-native-ref bytevector k)    procedure 
(bytevector-s16-native-ref bytevector k)    procedure 
(bytevector-u16-set! bytevector k n endianness)    procedure 
(bytevector-s16-set! bytevector k n endianness)    procedure 
(bytevector-u16-native-set! bytevector k n)    procedure 
(bytevector-s16-native-set! bytevector k n)    procedure 
     * 
     * (bytevector-u32-ref bytevector k endianness)    procedure 
(bytevector-s32-ref bytevector k endianness)    procedure 
(bytevector-u32-native-ref bytevector k)    procedure 
(bytevector-s32-native-ref bytevector k)    procedure 
(bytevector-u32-set! bytevector k n endianness)    procedure 
(bytevector-s32-set! bytevector k n endianness)    procedure 
(bytevector-u32-native-set! bytevector k n)    procedure 
(bytevector-s32-native-set! bytevector k n)    procedure 
     * 
     * (bytevector-u64-ref bytevector k endianness)    procedure 
(bytevector-s64-ref bytevector k endianness)    procedure 
(bytevector-u64-native-ref bytevector k)    procedure 
(bytevector-s64-native-ref bytevector k)    procedure 
(bytevector-u64-set! bytevector k n endianness)    procedure 
(bytevector-s64-set! bytevector k n endianness)    procedure 
(bytevector-u64-native-set! bytevector k n)    procedure 
(bytevector-s64-native-set! bytevector k n)    procedure
     * 
     * (bytevector-ieee-single-native-ref bytevector k)    procedure 
     * (bytevector-ieee-single-ref bytevector k endianness)    procedure 
     * (bytevector-ieee-double-native-ref bytevector k)    procedure 
(bytevector-ieee-double-ref bytevector k endianness)    procedure 
    (bytevector-ieee-single-native-set! bytevector k x)    procedure 
(bytevector-ieee-single-set! bytevector    procedure 
     * (bytevector-ieee-double-native-set! bytevector k x)    procedure 
(bytevector-ieee-double-set! bytevector    procedure
     */
  }
}
#endif
