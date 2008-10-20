using System;
using System.Collections.Generic;
using System.Text;

namespace IronScheme.Types
{
  public struct UnicodeScalar
  {
    readonly int value;

    public UnicodeScalar(int value)
    {
      this.value = value;
    }

    public override string ToString()
    {
      return char.ConvertFromUtf32(value);
    }

    public int Value
    {
      get { return value; }
    }

    public static readonly UnicodeScalar Eof = new UnicodeScalar(-1);

    public static implicit operator UnicodeScalar(char c)
    {
      return new UnicodeScalar(c);
    }

    public static bool operator ==(UnicodeScalar a, UnicodeScalar b)
    {
      return a.value == b.value;
    }

    public static bool operator !=(UnicodeScalar a, UnicodeScalar b)
    {
      return a.value != b.value;
    }

    public override bool Equals(object obj)
    {
      if (obj is UnicodeScalar)
      {
        return ((UnicodeScalar)obj).value == value;
      }
      else
      {
        return false;
      }
    }

    public override int GetHashCode()
    {
      return value.GetHashCode();
    }

  }
}
