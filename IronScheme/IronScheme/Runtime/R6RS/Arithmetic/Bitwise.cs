using System;
using System.Collections.Generic;
using System.Text;
using System.ComponentModel;
using Microsoft.Scripting.Math;
using System.Collections;

namespace IronScheme.Runtime.R6RS.Arithmetic
{
  public class Bitwise : Builtins
  {
    static TypeConverter BigIntConverter = TypeDescriptor.GetConverter(typeof(BigInteger));

    [Builtin("bitwise-not")]
    public static object BitWiseNot(object ei)
    {
      return ToIntegerIfPossible(~ConvertToBigInteger(ei));
    }

    //http://www.mcs.vuw.ac.nz/cgi-bin/info2www?(elisp)Bitwise+Operations
    [Builtin("bitwise-and")]
    public static object BitWiseAnd(params object[] eis)
    {
      if (eis.Length == 0)
      {
        return -1;
      }

      BigInteger bi = ConvertToBigInteger(eis[0]);

      for (int i = 1; i < eis.Length; i++)
      {
        bi &= ConvertToBigInteger(eis[i]);
      }

      return ToIntegerIfPossible(bi);
      
    }

    [Builtin("bitwise-ior")]
    public static object BitWiseIor(params object[] eis)
    {
      if (eis.Length == 0)
      {
        return 0;
      }

      BigInteger bi = ConvertToBigInteger(eis[0]);

      for (int i = 1; i < eis.Length; i++)
      {
        bi |= ConvertToBigInteger(eis[i]);
      }

      return ToIntegerIfPossible(bi);

    }

    [Builtin("bitwise-xor")]
    public static object BitWiseXor(params object[] eis)
    {
      if (eis.Length == 0)
      {
        return 0;
      }

      BigInteger bi = ConvertToBigInteger(eis[0]);

      for (int i = 1; i < eis.Length; i++)
      {
        bi ^= ConvertToBigInteger(eis[i]);
      }

      return ToIntegerIfPossible(bi);

    }

    [Builtin("bitwise-bit-count")]
    public static object BitWiseBitCount(object ei)
    {
      BigInteger bi = ConvertToBigInteger(ei);

      if (bi <= 0)
      {
        return BitWiseNot(BitWiseBitCount(BitWiseNot(ei)));
      }
      else
      {
        int count = 0;
        while (bi > 0)
        {
          count += (int)(bi & 1);
          bi >>= 1;
        }
        return count;
      }
    }

    [Builtin("bitwise-length")]
    public static object BitWiseLength(object ei)
    {
      BigInteger bi = ConvertToBigInteger(ei);

      if (bi < 0)
      {
        return BitWiseLength(BitWiseNot(ei));
      }
      else
      {
        int count = 0;
        while (bi > 0)
        {
          count++;
          bi >>= 1;
        }
        return count;
      }
    }

    [Builtin("bitwise-first-bit-set")]
    public static object BitWiseFirstBitSet(object ei)
    {
      BigInteger bi = ConvertToBigInteger(ei);

      if (bi == 0)
      {
        return -1;
      }
      else
      {
        int count = 0;
        while (bi != 0)
        {
          if ((int)(bi & 1) == 1)
          {
            return count;
          }
          count++;
          bi >>= 1;
        }
        return count;
      }
    }

    [Builtin("bitwise-bit-set?")]
    public static object BitWiseIsBitSet(object ei, object k)
    {
      BigInteger bi = ConvertToBigInteger(ei);
      BigInteger ki = ConvertToBigInteger(k);

      if (ki < 0)
      {
        AssertionViolation("bitwise-bit-set?", "k is negative", k);
      }

      if (bi == 0)
      {
        return FALSE;
      }
      else if (bi == -1)
      {
        return TRUE;
      }
      else
      {
        int count = 0;
        while (bi != 0)
        {
          if ((int)(bi & 1) == 1 && count == ki)
          {
            return TRUE;
          }
          count++;
          bi >>= 1;
        }
        return FALSE;
      }
    }
    
    [Builtin("bitwise-arithmetic-shift")]
    public static object BitWiseArithmeticShift(object ei, object k)
    {
      BigInteger bi = ConvertToBigInteger(ei);
      int ki = ConvertToInteger(k);
      
      int ii;
      if (bi.AsInt32(out ii))
      {
        if (ki < 0)
        {
          return ii >> -ki;
        }
      }

      if (ki == 0)
      {
        return ei;
      }
      if (ki < 0)
      {
        return ToIntegerIfPossible(bi >> -ki);
      }
      else
      {
        return ToIntegerIfPossible(bi << ki);
      }
    }

    //(bitwise-reverse-bit-field fx1 fx2 fx3)
    [Builtin("bitwise-reverse-bit-field")]
    public static object FxReverseBitField(object e1, object e2, object e3)
    {
      BigInteger i1 = ConvertToBigInteger(e1);
      BigInteger i2 = ConvertToBigInteger(e2);
      BigInteger i3 = ConvertToBigInteger(e3);

      BitArray ba = new BitArray(i1.ToByteArray());

      BigInteger range = (i3 - i2);

      for (BigInteger i = i2; i < (i3 - range / 2); i += 1)
      {
        int m1 = (int) (i);
        int m2 = (int) (i3 - (i - i2 + 1));
        bool b1 = ba[m1];
        bool b2 = ba[m2];
        ba[m1] = b2;
        ba[m2] = b1;
      }

      int[] result = new int[ba.Length/32 + 1];

      ba.CopyTo(result, 0);

      return ToIntegerIfPossible(new BigInteger(1, Array.ConvertAll<int, uint>(result, delegate(int i) { return (uint)i; })));
    }
  }
}
