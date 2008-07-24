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


using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting.Generation;
using Microsoft.Scripting;
using System.Reflection;
using Microsoft.Scripting.Utils;
using System.Reflection.Emit;
using System.Collections;
using Microsoft.Scripting.Math;

namespace IronScheme.Runtime.R6RS
{
  public class ByteVectors : Builtins
  {

    [Builtin("bytevector?")]
    public static object IsByteVector(object obj)
    {
      return GetBool(obj is byte[]);
    }

    [Builtin("make-bytevector")]
    public static object MakeByteVector(object k)
    {
      int i = RequiresNotNull<int>(k);
      return new byte[i];
    }

    static byte GetByteValue(object o)
    {
      int i = ConvertToInteger(o);

      if (i < -128 || i > 255)
      {
        return (byte)AssertionViolation(GetCaller(), "too big or small for octet or byte", o);
      }

      return (byte)i;
    }

    [Builtin("make-bytevector")]
    public static object MakeByteVector(object k, object fill)
    {
      byte c = GetByteValue(fill);
      byte[] b = new byte[RequiresNotNull<int>(k)];

      for (int i = 0; i < b.Length; i++)
      {
        b[i] = c;
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
            return FALSE;
          }
        }

        return TRUE;
      }

      return FALSE;
    }

    [Builtin("bytevector-fill!")]
    public static object ByteVectorFill(object v, object fill)
    {

      byte[] b = RequiresNotNull<byte[]>(v);
      byte c = GetByteValue(fill);

      for (int i = 0; i < b.Length; i++)
      {
        b[i] = c;
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

      return Convert.ToInt32(b1[i]);
    }

    [Builtin("bytevector-s8-ref")]
    public static object ByteVectorS8Ref(object v1, object k)
    {
      byte[] b1 = RequiresNotNull<byte[]>(v1);
      int i = RequiresNotNull<int>(k);

      return Convert.ToInt32( (sbyte) b1[i]);
    }

    [Builtin("bytevector-u8-set!")]
    public static object ByteVectorU8Set(object v1, object k, object octet)
    {
      byte[] b1 = RequiresNotNull<byte[]>(v1);
      int i = RequiresNotNull<int>(k);

      b1[i] = Convert.ToByte(octet);

      return Unspecified;
    }

    [Builtin("bytevector-s8-set!")]
    public static object ByteVectorS8Set(object v1, object k, object @byte)
    {
      byte[] b1 = RequiresNotNull<byte[]>(v1);
      int i = RequiresNotNull<int>(k);
      int b = Convert.ToInt32(@byte);

      b1[i] = (byte)~b;

      return Unspecified;
    }

    [Builtin("bytevector->u8-list")]
    public static object ByteVectorToU8List(object v1)
    {
      byte[] b1 = RequiresNotNull<byte[]>(v1);
      return Runtime.Cons.FromList(Array.ConvertAll<byte, int>(b1, delegate (byte b) {return (int) b;}));
    }

    [Builtin("u8-list->bytevector")]
    public static object U8ListToByteVector(object obj)
    {
      object[] bytes = ListToVector(obj) as object[];
      byte[] buffer = new byte[bytes.Length];
      for (int i = 0; i < buffer.Length; i++)
      {
        buffer[i] = GetByteValue(bytes[i]);
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

    //(bytevector-uint-ref bytevector k endianness size)
    [Builtin("bytevector-uint-ref")]
    public static object BytevectorUintRef(object bytevector, object k, object endianess, object size)
    {
      byte[] b = RequiresNotNull<byte[]>(bytevector);
      int i = RequiresNotNull<int>(k);
      SymbolId end = RequiresNotNull<SymbolId>(endianess);
      int s = RequiresNotNull<int>(size);

      switch (s)
      {
        case 1:
          return (int) b[0];
        case 2:
          return (int) BitConverter.ToUInt16(b, i);
        case 4:
          return (BigInteger) BitConverter.ToUInt32(b, i);
        case 8:
          return (BigInteger) BitConverter.ToUInt64(b, i);
        default:
          byte[] data = new byte[s + 1];
          Buffer.BlockCopy(b, i, data, 0, s);
          BigInteger bi = BigInteger.Create(data);
          return bi;
      }
    }

    [Builtin("bytevector-sint-ref")]
    public static object BytevectorSintRef(object bytevector, object k, object endianess, object size)
    {
      byte[] b = RequiresNotNull<byte[]>(bytevector);
      int i = RequiresNotNull<int>(k);
      SymbolId end = RequiresNotNull<SymbolId>(endianess);
      int s = RequiresNotNull<int>(size);

      switch (s)
      {
        case 1:
          return (int) unchecked ((sbyte)b[0]);
        case 2:
          return (int) BitConverter.ToInt16(b, i);
        case 4:
          return (int) BitConverter.ToInt32(b, i);
        case 8:
          return (BigInteger) BitConverter.ToInt64(b, i);
        default:
          byte[] data = new byte[s];
          Buffer.BlockCopy(b, i, data, 0, s);
          return BigInteger.Create(data);
      }
    }

    //(bytevector-uint-set! bytevector k n endianness size)
    [Builtin("bytevector-uint-set!")]
    public static object BytevectorUintSet(object bytevector, object k, object n, object endianess, object size)
    {
      byte[] b = RequiresNotNull<byte[]>(bytevector);
      int i = RequiresNotNull<int>(k);

      SymbolId end = RequiresNotNull<SymbolId>(endianess);
      int s = RequiresNotNull<int>(size);
      byte[] data = null;

      switch (s)
      {
        case 1:
          b[i] = Convert.ToByte(n);
          break;
        case 2:
          data = BitConverter.GetBytes(Convert.ToUInt16(n));
          Buffer.BlockCopy(data, 0, b, i, s);
          break;
        case 4:
          data = BitConverter.GetBytes(Convert.ToUInt32(n));
          Buffer.BlockCopy(data, 0, b, i, s);
          break;
        case 8:
          data = BitConverter.GetBytes(Convert.ToUInt64(n));
          Buffer.BlockCopy(data, 0, b, i, s);
          break;
        default:
          BigInteger bi = (BigInteger)n;
          data = bi.ToByteArray();
          Buffer.BlockCopy(data, 0, b, i, s);
          break;
      }

      return Unspecified;
    }

    //(bytevector-sint-set! bytevector k n endianness size)
    [Builtin("bytevector-sint-set!")]
    public static object BytevectorSintSet(object bytevector, object k, object n, object endianess, object size)
    {
      byte[] b = RequiresNotNull<byte[]>(bytevector);
      int i = RequiresNotNull<int>(k);

      SymbolId end = RequiresNotNull<SymbolId>(endianess);
      int s = RequiresNotNull<int>(size);
      byte[] data = null;

      switch (s)
      {
        case 1:
          b[i] = unchecked((byte) Convert.ToSByte(n));
          break;
        case 2:
          data = BitConverter.GetBytes(Convert.ToInt16(n));
          Buffer.BlockCopy(data, 0, b, i, s);
          break;
        case 4:
          data = BitConverter.GetBytes(Convert.ToInt32(n));
          Buffer.BlockCopy(data, 0, b, i, s);
          break;
        case 8:
          data = BitConverter.GetBytes(Convert.ToInt64(n));
          Buffer.BlockCopy(data, 0, b, i, s);
          break;
        default:
          BigInteger bi = (BigInteger)n;
          data = bi.ToByteArray();
          Buffer.BlockCopy(data, 0, b, i, s);
          break;
      }

      return Unspecified;
    }

    //(bytevector->uint-list bytevector endianness size)
    [Builtin("bytevector->uint-list")]
    public static object BytevectorToUintList(object bytevector, object endianess, object size)
    {
      byte[] b = RequiresNotNull<byte[]>(bytevector);
      SymbolId end = RequiresNotNull<SymbolId>(endianess);
      int s = RequiresNotNull<int>(size);

      int l = b.Length;

      List<object> list = new List<object>();

      for (int i = 0; i < l; i += s)
      {
        list.Add(BytevectorUintRef(b, i, endianess, size));
      }

      return Runtime.Cons.FromList(list);
    }

    //(bytevector->sint-list bytevector endianness size)
    [Builtin("bytevector->sint-list")]
    public static object BytevectorToSintList(object bytevector, object endianess, object size)
    {
      byte[] b = RequiresNotNull<byte[]>(bytevector);
      SymbolId end = RequiresNotNull<SymbolId>(endianess);
      int s = RequiresNotNull<int>(size);

      int l = b.Length;

      List<object> list = new List<object>();

      for (int i = 0; i < l; i += s)
      {
        list.Add(BytevectorSintRef(b, i, endianess, size));
      }

      return Runtime.Cons.FromList(list);
    }

    //(uint-list->bytevector list endianness size)
    [Builtin("uint-list->bytevector")]
    public static object UintListToBytevector(object list, object endianess, object size)
    {
      Cons c = Requires<Cons>(list);
      SymbolId end = RequiresNotNull<SymbolId>(endianess);
      int s = RequiresNotNull<int>(size);

      List<byte> blist = new List<byte>();

      while (c != null)
      {
        byte[] data = new byte[s];
        BytevectorUintSet(data, 0, c.car, endianess, size);
        blist.AddRange(data);

        c = c.cdr as Cons;
      }

      return blist.ToArray();
    }

    //(sint-list->bytevector list endianness size)
    [Builtin("sint-list->bytevector")]
    public static object SintListToBytevector(object list, object endianess, object size)
    {
      Cons c = Requires<Cons>(list);
      SymbolId end = RequiresNotNull<SymbolId>(endianess);
      int s = RequiresNotNull<int>(size);

      List<byte> blist = new List<byte>();

      while (c != null)
      {
        byte[] data = new byte[s];
        BytevectorSintSet(data, 0, c.car, endianess, size);
        blist.AddRange(data);

        c = c.cdr as Cons;
      }

      return blist.ToArray();
    }

    //(bytevector-ieee-single-ref bytevector k endianness)     
    [Builtin("bytevector-ieee-single-ref")]
    public static object BytevectorIEEESingleRef(object bytevector, object k, object endianess)
    {
      byte[] b = RequiresNotNull<byte[]>(bytevector);
      int i = RequiresNotNull<int>(k);
      SymbolId end = RequiresNotNull<SymbolId>(endianess);

      return (double) BitConverter.ToSingle(b, i);
    }

    //(bytevector-ieee-double-ref bytevector k endianness)   
    [Builtin("bytevector-ieee-double-ref")]
    public static object BytevectorIEEEDoubleRef(object bytevector, object k, object endianess)
    {
      byte[] b = RequiresNotNull<byte[]>(bytevector);
      int i = RequiresNotNull<int>(k);
      SymbolId end = RequiresNotNull<SymbolId>(endianess);

      return BitConverter.ToDouble(b, i);
    }

    //(bytevector-ieee-single-set! bytevector k x endianness)  
    [Builtin("bytevector-ieee-single-set!")]
    public static object BytevectorIEEESingleSet(object bytevector, object k, object x, object endianess)
    {
      byte[] b = RequiresNotNull<byte[]>(bytevector);
      int i = RequiresNotNull<int>(k);
      float f = Convert.ToSingle(x);

      SymbolId end = RequiresNotNull<SymbolId>(endianess);

      byte[] data = BitConverter.GetBytes(f);
      Buffer.BlockCopy(data, 0, b, i, 4);

      return Unspecified;
    }

    //(bytevector-ieee-double-set! bytevector k x endianness)  
    [Builtin("bytevector-ieee-double-set!")]
    public static object BytevectorIEEEDoubleSet(object bytevector, object k, object x, object endianess)
    {
      byte[] b = RequiresNotNull<byte[]>(bytevector);
      int i = RequiresNotNull<int>(k);
      double f = Convert.ToDouble(x);

      SymbolId end = RequiresNotNull<SymbolId>(endianess);

      byte[] data = BitConverter.GetBytes(f);
      Buffer.BlockCopy(data, 0, b, i, 8);

      return Unspecified;
    }
  }
}

