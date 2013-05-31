#region License
// Fraction numbers
// (C) 2004 Andreas Tönne, All right reserved
// No GPL, no license fee!
// Anyone can use this source provided that this copyright stays in place

// Version 1.1 Bugfix release
// - new limit properties MaxValue etc.
// - checked for these limits in the constructor (BigInteger.MinValue slipped through)
// - CompareTo: equality corrected (a case was missing)
// - CompareTo: inqueality of very large fractions lead to OverflowExceptions
// -			added 128bit comparison in these cases only

/* Copyright (c) 2007-2013 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Globalization;
using BigInteger = Oyster.Math.IntX;

namespace System.Runtime.CompilerServices
{
  public class ExtensionAttribute : Attribute
  {
  }
}


namespace IronScheme.Runtime
{
  static class DoubleExtensions
  {
    const long MANTISSA_MASK = (long)(ulong.MaxValue >> (64 - 52));
    const long EXPONENT_MASK = (long)(ulong.MaxValue >> (64 - 11));
    const long SIGN_MASK = 0x1;

    public static long GetMantissa(this double d)
    {
      var r = BitConverter.DoubleToInt64Bits(d);
      return GetMantissa(r);
    }

    static long GetMantissa(long r)
    {
      return r & MANTISSA_MASK;
    }

    public static int GetExponent(this double d)
    {
      var r = BitConverter.DoubleToInt64Bits(d);
      return GetExponent(r);
    }

    static int GetExponent(long r)
    {
      r >>= 52;
      return (int)(r & EXPONENT_MASK);
    }

    public static int GetSign(this double d)
    {
      var r = BitConverter.DoubleToInt64Bits(d);
      return GetSign(r);
    }

    static int GetSign(long r)
    {
      r >>= 63;
      return (int)(r & SIGN_MASK);
    }

    readonly static BigInteger MANTISSA = (ulong.MaxValue >> (64 - 52)) + 1;

    public static bool GetMantissaAndExponent(this double d, out BigInteger mantissa, out BigInteger exponent)
    {
      if (double.IsNaN(d))
      {
        mantissa = exponent = 0;
        return false;
      }

      if (double.IsNegativeInfinity(d))
      {
        mantissa = -1;
        exponent = 0;
        return false;
      }

      if (double.IsPositiveInfinity(d))
      {
        mantissa = 1;
        exponent = 0;
        return false;
      }

      var r = BitConverter.DoubleToInt64Bits(d);

      var man = GetMantissa(r);
      var exp = GetExponent(r);

      const int BIAS = 1075;
      if (exp == 0)
      {
        exponent = exp - BIAS;
        mantissa = (man << 1);
      }
      else
      {
        exponent = exp - BIAS;
        mantissa = man + MANTISSA;
      }

      return true;
    }

    public static bool GetComponents(this double d, out BigInteger numerator, out BigInteger denominator)
    {
      if (double.IsNaN(d))
      {
        numerator = denominator = 0;
        return false;
      }

      if (double.IsNegativeInfinity(d))
      {
        numerator = -1;
        denominator = 0;
        return false;
      }

      if (double.IsPositiveInfinity(d))
      {
        numerator = 1;
        denominator = 0;
        return false;
      }

      if (d == 0.0)
      {
        numerator = 0;
        denominator = 1;
        return true;
      }

      var r = BitConverter.DoubleToInt64Bits(d);

      var m = GetMantissa(r);
      var e = GetExponent(r);
      var s = GetSign(r) == 0 ? 1 : -1;



      const int BIAS = 1023;
      var re = e - BIAS;

      var exp = (((BigInteger)1) << Math.Abs(re));

      if (e == 0)
      {
        denominator = s * exp * (MANTISSA >> 1);
        numerator = m;
        return true;
      }

      if (re < 0)
      {
        denominator = MANTISSA * s * exp;
        numerator = (MANTISSA + m);
      }
      else
      {
        denominator = MANTISSA * s;
        numerator = exp * (MANTISSA + m);
      }
      

      return true;
    }

  }
	/// <summary>
	/// An implementation of rational (fractional) numbers.
	/// Numeric range: -BigInteger.MaxValue/1 to BigInteger.MaxValue/1
	/// Smallest positive number: 1/BigInteger.MaxValue
	/// Smallest negative number: -1/BigInteger.MaxValue
	/// Zero: 0/1
	/// One: 1/1
	/// 
	/// Fractional numbers exhibit no rounding errors as BigInteger as they stay 
	/// in their numeric range. In other cases a System.OverflowException is
	/// raised (coming from the underlying BigInteger computation).
	/// Note that overflow exceptions can be raised even if the reduced result
	/// fraction is inside the numeric range.
	/// 
	/// The typeCode of Fraction is Decimal since this is the closest numeric
	/// representation. Note however that conversion to and from Decimal will
	/// loose numeric precision:
	/// - going from Fraction 1/3 to Decimal is limited by the number of digits
	///		representable by a Decimal (28)
	///	- going from Decimal 1m/3m to Fraction will introduce rounding errors in
	///		the resulting Fraction because Fraction is constructed from a Decimal as
	///		sign * (int / scale) where the int is unsigned 96bit and scale ranges from
	///		1 to 10^28. We need to reduce the int to signed 64bit and limit the scale
	///		to 10^18. This results in rounding errors in the scale number as well
	///		as lost insignificant digits in the int. 
	///	However it is guaranteed that the conversion is true if the Decimal uses 18 or
	///	less significant digits. 
	/// </summary>
	/// <remarks>
	/// A fraction is kept normalized in the following way:
	/// - the denominator is always positive
	/// - the fraction is always reduced by the gcd of the nominator and denominator
	/// </remarks>
  [Serializable]
  //[System.ComponentModel.TypeConverter(typeof(Fraction.TypeConverter))]
	public class Fraction : IComparable, IConvertible
	{
    //class TypeConverter : System.ComponentModel.TypeConverter
    //{
    //  public override bool CanConvertFrom(System.ComponentModel.ITypeDescriptorContext context, Type sourceType)
    //  {
    //    if (sourceType == typeof(Fraction))
    //    {
    //      return true;
    //    }
    //    switch (Type.GetTypeCode(sourceType))
    //    {
    //      case TypeCode.Boolean:
    //      case TypeCode.DateTime:
    //      case TypeCode.DBNull:
    //      case TypeCode.Empty:
    //      case TypeCode.Object:
    //        return false;
    //      default:
    //        return true;
    //    }
    //  }

    //  public override object ConvertFrom(System.ComponentModel.ITypeDescriptorContext context, CultureInfo culture, object value)
    //  {
    //    if (value != null)
    //    {
    //      Type vt = value.GetType();
    //      if (vt == typeof(Fraction))
    //      {
    //        return value;
    //      }
    //      return (Fraction)Convert.ToDouble(value, CultureInfo.InvariantCulture);
    //    }
    //    return base.ConvertFrom(context, culture, value);
    //  }
    //}
		#region Declarations

		private BigInteger numerator;
		private BigInteger denominator;

		#endregion

		#region Constructors

		public Fraction(BigInteger newNumerator, BigInteger newDenominator)
		{
			BigInteger gcd;

			if (newDenominator == 0)
				throw new DivideByZeroException("Illegal fraction");

			numerator = newNumerator;
			denominator = newDenominator;
			if (denominator < 0)
			{
				numerator *= -1;
				denominator *= -1;
			}

			gcd = this.Gcd(numerator, denominator);
			numerator /= gcd;
			denominator /= gcd;
		}

		#endregion

		#region Properties

		public BigInteger Numerator
		{
			get
			{
				return numerator;
			}
		}

		public BigInteger Denominator
		{
			get
			{
				return denominator;
			}
		}

		public readonly static Fraction Zero = new Fraction(0,1);

		#endregion

		#region GCD

		/// <summary>
		/// The classic GCD algorithm of Euclid
		/// </summary>
    /// <param name="smaller"></param>
    /// <param name="larger"></param>
		/// <returns></returns>
		internal BigInteger Gcd(BigInteger smaller, BigInteger larger)
		{
			BigInteger rest;

			smaller = smaller.Abs();
			larger = larger.Abs();

			if (smaller == 0)
				return larger;
			else
			{
				if (larger == 0)
					return smaller;
			}
				

			// the GCD algorithm requires second >= first
			if(smaller > larger)
			{
				rest = larger;
				larger = smaller;
				smaller = rest;
			}
      
			while((rest = larger%smaller) != 0)
			{
				larger = smaller; 
        smaller = rest;
			}
			return smaller;
		}

		#endregion

		#region Conversions

    //public static explicit operator Decimal(Fraction fraction)
    //{
    //  return (Decimal)fraction.numerator / (Decimal)fraction.denominator;
    //}

    public double ToDouble(IFormatProvider foo)
    {
      return (double)this;
    }

    public static explicit operator Double(Fraction fraction)
    {
      BigInteger n = fraction.Numerator, d = fraction.Denominator;
      double r = n.ToFloat64() / d.ToFloat64();
      return r;
    }

    public static explicit operator BigInteger(Fraction fraction)
    {
      return (BigInteger)fraction.numerator / (BigInteger)fraction.denominator;
    }

    //[method:CLSCompliant(false)]
    //public static explicit operator UInt64(Fraction fraction)
    //{
    //  return (UInt64)(BigInteger)fraction;
    //}

    //public static explicit operator Int64(Fraction fraction)
    //{
    //  return (Int64)(BigInteger)fraction;
    //}

    //public static explicit operator Int32(Fraction fraction)
    //{
    //  return (Int32)(BigInteger)fraction;
    //}

    //[method:CLSCompliant(false)]
    //public static explicit operator UInt32(Fraction fraction)
    //{
    //  return (UInt32)(BigInteger)fraction;
    //}

    //public static explicit operator Int16(Fraction fraction)
    //{
    //  return (Int16)(BigInteger)fraction;
    //}

    //[method:CLSCompliant(false)]
    //public static explicit operator UInt16(Fraction fraction)
    //{
    //  return (UInt16)(BigInteger)fraction;
    //}

    //public static explicit operator Byte(Fraction fraction)
    //{
    //  return (Byte)(BigInteger)fraction;
    //}

    //public static explicit operator Single(Fraction fraction)
    //{
    //  return (Single)(BigInteger)fraction;
    //}

    public static implicit operator Fraction(int number)
    {
      return new Fraction(number, 1);
    }

    public static implicit operator Fraction(BigInteger number)
		{
			return new Fraction(number, 1);
		}

    public static implicit operator Fraction(double x)
    {
      if (x == 0.0)
      {
        return Fraction.Zero;
      }

      BigInteger num, den;
      x.GetComponents(out num, out den);
      return new Fraction(num, den);
    }

		#endregion

		#region IConvertible Members

    //[method:CLSCompliant(false)]
    //public UInt64 ToUInt64(IFormatProvider provider)
    //{
    //  return (UInt64)this;
    //}

    //public Int64 ToInt64(IFormatProvider provider)
    //{
    //  return (Int64)this;
    //}

    //[method:CLSCompliant(false)]
    //public sbyte ToSByte(IFormatProvider provider)
    //{
    //  return (SByte)this;
    //}

    //public double ToDouble(IFormatProvider provider)
    //{
    //  return (Double)this;
    //}

    //public DateTime ToDateTime(IFormatProvider provider)
    //{
    //  throw new InvalidCastException("Cannot convert fraction value to DateTime");
    //}

    //public float ToSingle(IFormatProvider provider)
    //{
    //  return (Single)this;
    //}

    //public bool ToBoolean(IFormatProvider provider)
    //{
    //  throw new InvalidCastException("Cannot convert fraction value to Boolean");
    //}

    //public Int32 ToInt32(IFormatProvider provider)
    //{
    //  return (Int32)this;
    //}

    //[method:CLSCompliant(false)]
    //public ushort ToUInt16(IFormatProvider provider)
    //{
    //  return (UInt16)this;
    //}

    //public short ToInt16(IFormatProvider provider)
    //{
    //  return (Int16)this;
    //}

    //public string ToString(IFormatProvider provider)
    //{
    //  throw new InvalidCastException("Cannot convert fraction value to String");
    //}

    //public byte ToByte(IFormatProvider provider)
    //{
    //  return (Byte)this;
    //}

    //public char ToChar(IFormatProvider provider)
    //{
    //  throw new InvalidCastException("Cannot convert fraction value to Char");
    //}

    //public System.TypeCode GetTypeCode()
    //{
    //  return TypeCode.Decimal;
    //}

    //public decimal ToDecimal(IFormatProvider provider)
    //{
    //  return (Decimal)this;
    //}

    //public object ToType(Type conversionType, IFormatProvider provider)
    //{
    //  if (this.denominator == 1)
    //    return Convert.ChangeType((BigInteger)this, conversionType, provider);
    //  else
    //    return Convert.ChangeType((Decimal)this, conversionType, provider);
    //}

    //[method:CLSCompliant(false)]
    //public UInt32 ToUInt32(IFormatProvider provider)
    //{
    //  return (UInt32)this;
    //}

		#endregion

		#region IComparable Members

		/// <summary>
		/// Note: the comparison is turned (by algebraic transformation) into a comparison
		/// of the cross-product of the fractions. This might overflow the available numbers,
		/// in which case we use home-made 128bit multiplication. This is slow! 
		/// </summary>
		/// <param name="obj"></param>
		/// <returns></returns>
    public Int32 CompareTo(object obj)
    {
      Fraction arg = (Fraction)obj;

      if ((this.numerator == arg.numerator) && (this.denominator == arg.denominator))
        return 0;

      Boolean thisNegative = this.numerator < 0;
      Boolean argNegative = arg.numerator < 0;
      if (thisNegative != argNegative)
      {
        if (argNegative)
          return 1;
        else
          return -1;
      }

      if (this.numerator * arg.denominator < this.denominator * arg.numerator)
        return -1;
      else
        return 1;
    }

		#endregion

		#region Operators

    public static Boolean operator <(Fraction fraction1, Fraction fraction2)
		{
			return fraction1.CompareTo(fraction2) < 0;
		}

    public static Boolean operator >(Fraction fraction1, Fraction fraction2)
		{
			return fraction1.CompareTo(fraction2) > 0;
		}

    public static Boolean operator ==(Fraction fraction1, Fraction fraction2)
		{
			return fraction1.CompareTo(fraction2) == 0;
		}

    public static Boolean operator !=(Fraction fraction1, Fraction fraction2)
		{
			return fraction1.CompareTo(fraction2) != 0;
		}

		public override Boolean Equals(object fraction1)
		{
      if (fraction1 is Fraction)
      {
        return this.CompareTo((Fraction)fraction1) == 0;
      }
      return false;
		}

		public override Int32 GetHashCode()
		{
			return (Int32)(double)this;
		}

    public static Boolean operator <=(Fraction fraction1, Fraction fraction2)
		{
			return fraction1.CompareTo(fraction2) <= 0;
		}

    public static Boolean operator >=(Fraction fraction1, Fraction fraction2)
		{
			return fraction1.CompareTo(fraction2) >= 0;
		}

		// **********************************************************************************

    public static Fraction operator +(Fraction fraction1, Fraction fraction2)
		{
			return new Fraction(
				(fraction1.numerator * fraction2.denominator) + (fraction2.numerator * fraction1.denominator), 
				fraction1.denominator * fraction2.denominator);
		}

		public static Fraction operator - (Fraction fraction1, Fraction fraction2)
		{
			return new Fraction(
				(fraction1.numerator * fraction2.denominator) - (fraction2.numerator * fraction1.denominator), 
				fraction1.denominator * fraction2.denominator);
		}

		public static Fraction operator / (Fraction fraction1, Fraction fraction2)
		{
			return new Fraction(
				fraction1.numerator * fraction2.denominator, 
				fraction1.denominator * fraction2.numerator);
		}

		public static Fraction operator * (Fraction fraction1, Fraction fraction2)
		{
			return new Fraction(
				fraction1.numerator * fraction2.numerator, 
				fraction1.denominator * fraction2.denominator);
		}

		public static Fraction operator % (Fraction fraction1, Fraction fraction2)
		{
			BigInteger quo = (BigInteger)(fraction1 / fraction2);
			return fraction1 - new Fraction(
				fraction2.numerator * quo, 
				fraction2.denominator);
		}

		#endregion

		#region Alternative Operations

		public static Fraction Add(Fraction fraction1, Fraction fraction2)
		{
			return fraction1 + fraction2;
		}

		public static Fraction Subtract(Fraction fraction1, Fraction fraction2)
		{
			return fraction1 - fraction2;
		}

		public static Fraction Multiply(Fraction fraction1, Fraction fraction2)
		{
			return fraction1 * fraction2;
		}

		public static Fraction Divide(Fraction fraction1, Fraction fraction2)
		{
			return fraction1 / fraction2;
		}

		public static Fraction Modulus(Fraction fraction1, Fraction fraction2)
		{
			return fraction1 % fraction2;
		}

		#endregion

    public Fraction Abs()
    {
      if (this < 0)
      {
        return 0 - this;
      }
      else
      {
        return this;
      }
    }

		public override string ToString()
		{
			return numerator + "/" + denominator;
		}

    #region IConvertible Members

    TypeCode IConvertible.GetTypeCode()
    {
      throw new NotImplementedException();
    }

    bool IConvertible.ToBoolean(IFormatProvider provider)
    {
      throw new NotImplementedException();
    }

    byte IConvertible.ToByte(IFormatProvider provider)
    {
      throw new NotImplementedException();
    }

    char IConvertible.ToChar(IFormatProvider provider)
    {
      throw new NotImplementedException();
    }

    DateTime IConvertible.ToDateTime(IFormatProvider provider)
    {
      throw new NotImplementedException();
    }

    decimal IConvertible.ToDecimal(IFormatProvider provider)
    {
      throw new NotImplementedException();
    }

    double IConvertible.ToDouble(IFormatProvider provider)
    {
      return (double)this;
    }

    short IConvertible.ToInt16(IFormatProvider provider)
    {
      throw new NotImplementedException();
    }

    int IConvertible.ToInt32(IFormatProvider provider)
    {
      throw new NotImplementedException();
    }

    long IConvertible.ToInt64(IFormatProvider provider)
    {
      throw new NotImplementedException();
    }

    sbyte IConvertible.ToSByte(IFormatProvider provider)
    {
      throw new NotImplementedException();
    }

    float IConvertible.ToSingle(IFormatProvider provider)
    {
      throw new NotImplementedException();
    }

    string IConvertible.ToString(IFormatProvider provider)
    {
      throw new NotImplementedException();
    }

    object IConvertible.ToType(Type conversionType, IFormatProvider provider)
    {
      throw new NotImplementedException();
    }

    ushort IConvertible.ToUInt16(IFormatProvider provider)
    {
      throw new NotImplementedException();
    }

    uint IConvertible.ToUInt32(IFormatProvider provider)
    {
      throw new NotImplementedException();
    }

    ulong IConvertible.ToUInt64(IFormatProvider provider)
    {
      throw new NotImplementedException();
    }

    #endregion
  }
}
