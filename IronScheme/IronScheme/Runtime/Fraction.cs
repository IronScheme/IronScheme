// Fraction numbers
// (C) 2004 Andreas Tönne, All right reserved
// No GPL, no license fee!
// Anyone can use this source provided that this copyright stays in place

// Version 1.1 Bugfix release
// - new limit properties MaxValue etc.
// - checked for these limits in the constructor (Int64.MinValue slipped through)
// - CompareTo: equality corrected (a case was missing)
// - CompareTo: inqueality of very large fractions lead to OverflowExceptions
// -			added 128bit comparison in these cases only

using System;
using System.Globalization;


namespace IronScheme.Runtime
{
	/// <summary>
	/// An implementation of rational (fractional) numbers.
	/// Numeric range: -Int64.MaxValue/1 to Int64.MaxValue/1
	/// Smallest positive number: 1/Int64.MaxValue
	/// Smallest negative number: -1/Int64.MaxValue
	/// Zero: 0/1
	/// One: 1/1
	/// 
	/// Fractional numbers exhibit no rounding errors as long as they stay 
	/// in their numeric range. In other cases a System.OverflowException is
	/// raised (coming from the underlying Int64 computation).
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
  [System.ComponentModel.TypeConverter(typeof(Fraction.TypeConverter))]
	public struct Fraction : IComparable, IConvertible, IFormattable
	{
    class TypeConverter : System.ComponentModel.TypeConverter
    {
      public override bool CanConvertFrom(System.ComponentModel.ITypeDescriptorContext context, Type sourceType)
      {
        if (sourceType == typeof(Fraction))
        {
          return true;
        }
        switch (Type.GetTypeCode(sourceType))
        {
          case TypeCode.Boolean:
          case TypeCode.DateTime:
          case TypeCode.DBNull:
          case TypeCode.Empty:
          case TypeCode.Object:
            return false;
          default:
            return true;
        }
      }

      public override object ConvertFrom(System.ComponentModel.ITypeDescriptorContext context, CultureInfo culture, object value)
      {
        if (value != null)
        {
          Type vt = value.GetType();
          if (vt == typeof(Fraction))
          {
            return value;
          }
          return Fraction.ToFraction(Convert.ToDecimal(value));
        }
        return base.ConvertFrom(context, culture, value);
      }
    }
		#region Declarations

		private Int64 numerator;
		private Int64 denominator;

		#endregion

		#region Constructors

		public Fraction(Int64 newNumerator, Int64 newDenominator)
		{
			Int64 gcd;

			if (newDenominator == 0)
				throw new DivideByZeroException("Illegal fraction");
			if (newNumerator == Int64.MinValue)
				throw new OverflowException("Fraction nominator may not be Int64.MinValue");
			if (newDenominator == Int64.MinValue)
				throw new OverflowException("Fraction denominator may not be Int64.MinValue");

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

		public Int64 Numerator
		{
			get
			{
				return numerator;
			}
		}

		public Int64 Denominator
		{
			get
			{
				return denominator;
			}
		}

		public readonly static Fraction Zero = new Fraction(0,1);
    public readonly static Fraction MinValue =new Fraction(-Int64.MaxValue, 1);
    public readonly static Fraction MaxValue = new Fraction(Int64.MaxValue, 1);
    public readonly static Fraction SmallestNegativeValue = new Fraction(-1, Int64.MaxValue);
    public readonly static Fraction SmallestValue = new Fraction(1, Int64.MaxValue);



		#endregion

		#region GCD

		/// <summary>
		/// The classic GCD algorithm of Euclid
		/// </summary>
    /// <param name="smaller"></param>
    /// <param name="larger"></param>
		/// <returns></returns>
		internal Int64 Gcd(Int64 smaller, Int64 larger)
		{
			Int64 rest;

			smaller = Math.Abs(smaller);
			larger = Math.Abs(larger);

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
				larger = smaller; smaller = rest;
			}
			return smaller;
		}

		#endregion

		#region Decimal conversion

		/// <summary>
		/// A lossy conversion from decimal to fraction.
		/// If the integer part of the decimal exceeds the available 64 bits (96 is possible)
		/// we truncate the least significant part of these bits.
		/// If the scale factor is too large we scale down the integer part at the expense of precision.
		/// </summary>
    /// <param name="convertedDecimal"></param>
		/// <returns></returns>
		/// <remarks>
		/// Note the comments on precision issues when the number of digits used in the Decimal
		/// exceed the precision of Int64 significantly.
		/// </remarks>
		private static Fraction ToFraction(Decimal convertedDecimal)
		{
			unchecked
			{
				Int32[] bits = Decimal.GetBits(convertedDecimal);
				UInt32 low32 = (UInt32)bits[0];
				UInt32 middle32 = (UInt32)bits[1];
				UInt32 high32 = (UInt32)bits[2];
				Int32 scaleAndSign = bits[3];
				Boolean negative = scaleAndSign < 0;
				Int32 scaleFactor = (Int32)(Math.Abs(scaleAndSign) >> 16);

				Decimal scale = (Decimal)Math.Pow(10, (Double)scaleFactor);

				// now we construct the scaled long integer
				// if high32 is not zero then the overall number would be too large for the numeric range of long.
				// we determine how many significant bits of high32 need to be shifted down to the lower two ints
				// and correct the scale accordingly. 
				// this step will loose precision for two reason: bits are truncated in low32 and the scale value
				// might overflow its precision by too many divisions by two. This second reason is aggravated
				// significantly if the decimal is very much larger than 1. 
				while (high32 > 0)
				{
					low32 >>= 1;
					if ((middle32 & 1) > 0)
						low32 |= 0x80000000;
					middle32 >>= 1;
					if ((high32 & 1) > 0)
						middle32 |= 0x80000000;
					high32 >>= 1;
					scale /= 2;
				}
				UInt64 scaledInt = ((UInt64)middle32 << 32) + low32;

				// bad things would happen if the highest bit of scaledInt is not zero. 
				// Truncate the ulong integer to stay within the numeric range of long by
				// dividing it by two and correspondingly divide the scale by two as well.
				// Again this step can loose precision by overflowing the precision of scale.
				if ((scaledInt & 0x80000000) > 0)
				{
					scaledInt >>= 1;
					scale /= 2;
				}

				// the scale may be in the range 1- 10^28 which would throw us out of the valid
				// numeric range for the denominator:  10^18
				// Note: we could have done this step initially before computing scale and save us from
				// computing the realScaleFactor (which would be equal to scale if done initially). However
				// this would aggravate the precision problems of scale even further. 
				Int32 realScaleFactor = (Int32)Math.Log10((Double)scale);
				if (realScaleFactor > 18)
				{
					UInt64 scaleCorrection = (UInt64)Math.Pow(10, (Double)realScaleFactor - 18);
					scaledInt /= scaleCorrection;
					scale /= scaleCorrection;
				}
			
				return new Fraction((Int64)scaledInt * (negative ? -1 : 1), (Int64)scale);
			}
		}

		#endregion

		#region 128Bit Comparison
		// this is a simple schoolbook algorithm for the comparison of the product
		// of two 64bit integers
		// replace its use in CompareTo as soon as you have better 128bit precision arithmetic at hand.

		/// <summary>
		/// Multiplication the schoolbook way, based on 32-bit steps.
		/// This can be certainly optimized but it is ok for the moment.
		/// </summary>
    /// <param name="ab"></param>
    /// <param name="cd"></param>
		/// <returns></returns>
		internal UInt32[] UMult128(UInt64 ab, UInt64 cd)
		{
			UInt64 a = ab >> 32;
			UInt64 b = ab & 0xFFFFFFFF;
			UInt64 c = cd >> 32;
			UInt64 d = cd & 0xFFFFFFFF;
			UInt32[] result = {0, 0, 0, 0};
			UInt64 tmp;
			
			UInt64 step1 = b*d;
			UInt64 step2 = a*d;
			UInt64 step3 = b*c;
			UInt64 step4 = a*c;

			result[3] = (UInt32)step1;
			tmp = (step1 >> 32) + (step2 & 0xFFFFFFFF) + (step3 & 0xFFFFFFFF);
			result[2] = (UInt32)tmp;
			tmp >>= 32;
			tmp += (step2 >> 32) + (step3 >> 32) + (step4 & 0xFFFFFFFF);
			result[1] = (UInt32)tmp;
			result[0] = (UInt32)(tmp >>32) + (UInt32)(step4 >> 32);
			return result;
		}

		/// <summary>
		/// Simple &lt; comparison of the 128bit multiplication result
		/// </summary>
		/// <param name="first"></param>
		/// <param name="second"></param>
		/// <returns></returns>
		internal Boolean Less128(UInt32[] first, UInt32[] second)
		{
			for (int i = 0; i < 4; i++)
			{
				if (first[i] < second[i])
					return true;
				if (first[i] > second[i])
					return false;
			}
			return false;
		}

		#endregion

		#region Conversions

		public static explicit operator Decimal(Fraction fraction)
		{
			return (Decimal)fraction.numerator / (Decimal)fraction.denominator;
		}

		public static explicit operator Double(Fraction fraction)
		{
			return (Double)fraction.numerator / (Double)fraction.denominator;
		}

		public static explicit operator Int64(Fraction fraction)
		{
			return (Int64)fraction.numerator / (Int64)fraction.denominator;
		}

		[method:CLSCompliant(false)]
		public static explicit operator UInt64(Fraction fraction)
		{
			return (UInt64)(Int64)fraction;
		}

		public static explicit operator Int32(Fraction fraction)
		{
			return (Int32)(Int64)fraction;
		}

		[method:CLSCompliant(false)]
		public static explicit operator UInt32(Fraction fraction)
		{
			return (UInt32)(Int64)fraction;
		}

		public static explicit operator Int16(Fraction fraction)
		{
			return (Int16)(Int64)fraction;
		}

		[method:CLSCompliant(false)]
		public static explicit operator UInt16(Fraction fraction)
		{
			return (UInt16)(Int64)fraction;
		}

		public static explicit operator Byte(Fraction fraction)
		{
			return (Byte)(Int64)fraction;
		}

		public static explicit operator Single(Fraction fraction)
		{
			return (Single)(Int64)fraction;
		}

		public static implicit operator Fraction(Int64 number)
		{
			return new Fraction(number, 1);
		}

		/// <summary>
		/// This is a potentially lossy conversion since the decimal may use more than the available
		/// 64 bits for the nominator. In this case the least significant bits of the decimal are 
		/// truncated.
		/// </summary>
    /// <param name="number"></param>
		/// <returns></returns>
		public static implicit operator Fraction(Decimal number)
		{
			return Fraction.ToFraction(number);
		}

		#endregion

		#region IConvertible Members

		[method:CLSCompliant(false)]
		public ulong ToUInt64(IFormatProvider provider)
		{
			return (UInt64)this;
		}

		[method:CLSCompliant(false)]
		public sbyte ToSByte(IFormatProvider provider)
		{
			return (SByte)this;
		}

		public double ToDouble(IFormatProvider provider)
		{
			return (Double)this;
		}

		public DateTime ToDateTime(IFormatProvider provider)
		{
			throw new InvalidCastException("Cannot convert fraction value to DateTime");
		}

		public float ToSingle(IFormatProvider provider)
		{
			return (Single)this;
		}

		public bool ToBoolean(IFormatProvider provider)
		{
			throw new InvalidCastException("Cannot convert fraction value to Boolean");
		}

		public Int32 ToInt32(IFormatProvider provider)
		{
			return (Int32)this;
		}

		[method:CLSCompliant(false)]
		public ushort ToUInt16(IFormatProvider provider)
		{
			return (UInt16)this;
		}

		public short ToInt16(IFormatProvider provider)
		{
			return (Int16)this;
		}

		public string ToString(IFormatProvider provider)
		{
			throw new InvalidCastException("Cannot convert fraction value to String");
		}

		public byte ToByte(IFormatProvider provider)
		{
			return (Byte)this;
		}

		public char ToChar(IFormatProvider provider)
		{
			throw new InvalidCastException("Cannot convert fraction value to Char");
		}

		public long ToInt64(IFormatProvider provider)
		{
			return (Int64)this;
		}

		public System.TypeCode GetTypeCode()
		{
			return TypeCode.Decimal;
		}

		public decimal ToDecimal(IFormatProvider provider)
		{
			return (Decimal)this;
		}

		public object ToType(Type conversionType, IFormatProvider provider)
		{
			if (this.denominator == 1)
				return Convert.ChangeType((Int64)this, conversionType, provider);
			else
				return Convert.ChangeType((Decimal)this, conversionType, provider);
		}

		[method:CLSCompliant(false)]
		public UInt32 ToUInt32(IFormatProvider provider)
		{
			return (UInt32)this;
		}

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

			try
			{
				checked
				{
					if (this.numerator * arg.denominator < this.denominator * arg.numerator)
						return -1;
					else
						return 1;
				}
			} 
			catch (OverflowException)
			{
				// we need to resort to 128bit precision multiplication here
				UInt32[] product1 = this.UMult128((ulong)Math.Abs(this.numerator), (ulong)Math.Abs(arg.denominator));
				UInt32[] product2 = this.UMult128((ulong)Math.Abs(this.denominator), (ulong)Math.Abs(arg.numerator));
				Boolean less = this.Less128(product1, product2);
				if (thisNegative)
					less = !less;

				return less ? -1 : 1;
			}
		}

		#endregion

		#region IFormattable Members

		string System.IFormattable.ToString(string format, IFormatProvider formatProvider)
		{
			return this.numerator.ToString(format, formatProvider) + "/" + this.denominator.ToString(format, formatProvider);
		}

		#endregion

		#region Operators

		public static Boolean operator < (Fraction fraction1, Fraction fraction2)
		{
			return fraction1.CompareTo(fraction2) < 0;
		}

		public static Boolean operator > (Fraction fraction1, Fraction fraction2)
		{
			return fraction1.CompareTo(fraction2) > 0;
		}

		public static Boolean operator == (Fraction fraction1, Fraction fraction2)
		{
			return fraction1.CompareTo(fraction2) == 0;
		}

		public static Boolean operator != (Fraction fraction1, Fraction fraction2)
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
			return (Int32)this;
		}

		public static Boolean operator <= (Fraction fraction1, Fraction fraction2)
		{
			return fraction1.CompareTo(fraction2) <= 0;
		}

		public static Boolean operator >= (Fraction fraction1, Fraction fraction2)
		{
			return fraction1.CompareTo(fraction2) >= 0;
		}

		// **********************************************************************************

		public static Fraction operator + (Fraction fraction1, Fraction fraction2)
		{
			return new Fraction(
				checked((fraction1.numerator * fraction2.denominator) + (fraction2.numerator * fraction1.denominator)), 
				checked(fraction1.denominator * fraction2.denominator));
		}

		public static Fraction operator - (Fraction fraction1, Fraction fraction2)
		{
			return new Fraction(
				checked((fraction1.numerator * fraction2.denominator) - (fraction2.numerator * fraction1.denominator)), 
				checked(fraction1.denominator * fraction2.denominator));
		}

		public static Fraction operator / (Fraction fraction1, Fraction fraction2)
		{
			return new Fraction(
				checked(fraction1.numerator * fraction2.denominator), 
				checked(fraction1.denominator * fraction2.numerator));
		}

		public static Fraction operator * (Fraction fraction1, Fraction fraction2)
		{
			return new Fraction(
				checked(fraction1.numerator * fraction2.numerator), 
				checked(fraction1.denominator * fraction2.denominator));
		}

		public static Fraction operator % (Fraction fraction1, Fraction fraction2)
		{
			Int64 quo = (Int64)checked((fraction1 / fraction2));
			return fraction1 - new Fraction(
				checked(fraction2.numerator * quo), 
				checked(fraction2.denominator));
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

		public override string ToString()
		{
      if (denominator == 1)
      {
        return numerator.ToString(CultureInfo.CurrentCulture);
      }

      if (numerator % denominator == 0)
      {
        return (numerator / denominator).ToString();
      }

			return numerator.ToString(CultureInfo.CurrentCulture) + "/" + denominator.ToString(CultureInfo.CurrentCulture);
		}
	}
}
