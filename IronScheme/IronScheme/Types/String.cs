using System;
using System.Collections.Generic;
using System.Text;
using System.Globalization;

namespace IronScheme.Types
{


  public sealed class UnicodeString : ICloneable, IEnumerable<UnicodeScalar>
  {
    readonly byte[] values;

    static byte[] PackScalars(IList<int> value)
    {
      // 24 bits
      int length = value.Count;
      int div = length * 3;

      byte[] values = new byte[div];

      for (int i = 0; i < length; i++)
      {
        int d = i * 3;

        int s = value[i];

        if (s < 0)
        {
          throw new ArgumentOutOfRangeException();
        }

        values[d + 0] = (byte)s;
        values[d + 1] = (byte)(s >> 8);
        values[d + 2] = (byte)(s >> 16);
      }

      return values;
    }

    static List<int> ExtractScalars(string s)
    {
      List<int> chars = new List<int>((s.Length * 3) / 2);

      var ee = StringInfo.GetTextElementEnumerator(s);

      while (ee.MoveNext())
      {
        string e = ee.GetTextElement();
        chars.Add(char.ConvertToUtf32(e, 0));
      }

      return chars;
    }

    public UnicodeString(string s)
      : this(s, false)
    {

    }

    public UnicodeString(string s, bool mutable)
      : this(ExtractScalars(s), mutable)
    {
    }

    internal UnicodeString(IList<int> value, bool mutable)
      : this(PackScalars(value))
    {
      if (mutable)
      {
        SetMutable();
      }
    }

    UnicodeString(byte[] values)
    {
      this.values = values;
    }

    public UnicodeString(int capacity)
      : this(new byte[capacity * 3])
    {
      SetMutable();
    }

    void SetMutable()
    {
      if (values.Length > 0)
      {
        values[2] |= 1 << 5;
      }
    }

    public int Length
    {
      get { return values.Length / 3; }
    }

    enum Flags
    {
      None = 0,
      Mutable = 1
    }

    Flags GetFlags()
    {
      return values.Length == 0 ? Flags.None : (Flags)(values[2] >> 5);
    }

    public bool Mutable
    {
      get
      {
        if (Length == 0)
        {
          return false;
        }
        Flags m = GetFlags();
        return (m & Flags.Mutable) == Flags.Mutable;
      }
    }

    public override bool Equals(object obj)
    {
      UnicodeString o = obj as UnicodeString;

      if (o == null)
      {
        return false;
      }

      if (o == this)
      {
        return true;
      }

      if (o.Length != Length)
      {
        return false;
      }

      for (int i = 0; i < Length; i++)
      {
        if (this[i] != o[i])
        {
          return false;
        }
      }
      return true;
    }

    public override int GetHashCode()
    {
      // eek :|
      return ToString().GetHashCode();
    }

    public UnicodeScalar this[int i]
    {
      get
      {
        int d = i * 3;
        int i1 = values[d + 0];
        int i2 = values[d + 1] << 8;
        int i3 = (values[d + 2] & 0x1f) << 16;
        return new UnicodeScalar(i1 | i2 | i3);
      }
      set
      {
        if (Mutable)
        {
          int s = value.Value;
          if (s < 0)
          {
            throw new ArgumentOutOfRangeException();
          }

          int d = i * 3;

          values[d + 0] = (byte)s;
          values[d + 1] = (byte)(s >> 8);
          values[d + 2] = (byte)(s >> 16);

          if (i == 0)
          {
            SetMutable();
          }
        }
        else
        {
          throw new ArgumentException("string is not mutable");
        }
      }
    }

    public override string ToString()
    {
      if (values == null || values.Length == 0)
      {
        return string.Empty;
      }

      StringBuilder sb = new StringBuilder();

      for (int i = 0; i < Length; i++)
      {
        sb.Append(this[i]);
      }

      return sb.ToString();
    }

    public object Clone()
    {
      return Copy();
    }

    public static readonly UnicodeString Empty = new UnicodeString(string.Empty);

    public UnicodeString Copy()
    {
      return Substring(0);
    }

    public UnicodeString Substring(int start)
    {
      return Substring(start, Length);
    }

    public UnicodeString Substring(int start, int count)
    {
      byte[] data = new byte[count * 3];
      Buffer.BlockCopy(values, start, data, 0, data.Length);
      return new UnicodeString(data);
    }

    public static UnicodeString Concat(UnicodeString a, UnicodeString b)
    {
      byte[] output = new byte[a.values.Length + b.values.Length];
      Buffer.BlockCopy(a.values, 0, output, 0, a.values.Length);
      Buffer.BlockCopy(b.values, 0, output, a.values.Length, b.values.Length);
      return new UnicodeString(output);
    }

    public static UnicodeString Concat(UnicodeString[] s)
    {
      List<byte> all = new List<byte>();

      foreach (UnicodeString v in s)
      {
        all.AddRange(v.values);
      }

      return new UnicodeString(all.ToArray());
    }

    public IEnumerator<UnicodeScalar> GetEnumerator()
    {
      for (int i = 0; i < Length; i++)
      {
        yield return this[i];
      }
    }

    System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
    {
      return GetEnumerator();
    }
  }
}
