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

using System;
using System.Globalization;
using Microsoft.Scripting.Math;


namespace IronScheme.Runtime
{
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
  [System.ComponentModel.TypeConverter(typeof(Fraction.TypeConverter))]
	public class Fraction : IComparable, IConvertible
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
          return (Fraction)Convert.ToDouble(value, CultureInfo.InvariantCulture);
        }
        return base.ConvertFrom(context, culture, value);
      }
    }
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

		public static explicit operator Decimal(Fraction fraction)
		{
			return (Decimal)fraction.numerator / (Decimal)fraction.denominator;
		}

    public static explicit operator Double(Fraction fraction)
		{
      BigInteger n = fraction.Numerator, d = fraction.Denominator;
      double r = (double)n / (double)d;
      return r;
		}

    public static explicit operator BigInteger(Fraction fraction)
		{
			return (BigInteger)fraction.numerator / (BigInteger)fraction.denominator;
		}

		[method:CLSCompliant(false)]
    public static explicit operator UInt64(Fraction fraction)
		{
			return (UInt64)(BigInteger)fraction;
		}

    public static explicit operator Int64(Fraction fraction)
    {
      return (Int64)(BigInteger)fraction;
    }

    public static explicit operator Int32(Fraction fraction)
		{
			return (Int32)(BigInteger)fraction;
		}

		[method:CLSCompliant(false)]
    public static explicit operator UInt32(Fraction fraction)
		{
			return (UInt32)(BigInteger)fraction;
		}

    public static explicit operator Int16(Fraction fraction)
		{
			return (Int16)(BigInteger)fraction;
		}

		[method:CLSCompliant(false)]
    public static explicit operator UInt16(Fraction fraction)
		{
			return (UInt16)(BigInteger)fraction;
		}

    public static explicit operator Byte(Fraction fraction)
		{
			return (Byte)(BigInteger)fraction;
		}

    public static explicit operator Single(Fraction fraction)
		{
			return (Single)(BigInteger)fraction;
		}

    public static implicit operator Fraction(int number)
    {
      return new Fraction(number, 1);
    }

    public static implicit operator Fraction(BigInteger number)
		{
			return new Fraction(number, 1);
		}

    //from: http://www.math.uic.edu/~burgiel/Mtht420.99/5/Rational.java - thanks Google
    public static implicit operator Fraction(double x)
    {
      if (x == 0.0)
      {
        return Fraction.Zero;
      }

      int sgn = 1;
      if (x < 0.0)
      {
        sgn = -1;
        x = -x;
      }
      BigInteger intPart = (BigInteger) Math.Round(x);
      double z = x - intPart;
      if (z != 0)
      {
        z = 1.0 / z;
        BigInteger a = (BigInteger)Math.Round(z);
        z = z - a;
        BigInteger prevNum = 0;
        BigInteger num = 1;
        BigInteger prevDen = 1;
        BigInteger den = a;
        BigInteger tmp;
        double approxAns = ((double)(den * intPart + num)) / den;
        while (Math.Abs((x - approxAns)) > 0.0)
        {
          z = 1.0 / z;
          a = (BigInteger)Math.Round(z);
          z = z - a;
          if (double.IsNaN(z))
          {
            break;
          }
          tmp = a * num + prevNum;
          prevNum = num;
          num = tmp;
          tmp = a * den + prevDen;
          prevDen = den;
          den = tmp;
          approxAns = ((double)(den * intPart + num)) / den;
        }
        return new Fraction(sgn * (den * intPart + num), den);
      }
      else
      {                    // is integer
        return new Fraction(sgn * intPart, 1);
      }
    }

		#endregion

		#region IConvertible Members

		[method:CLSCompliant(false)]
    public UInt64 ToUInt64(IFormatProvider provider)
		{
			return (UInt64)this;
		}

    public Int64 ToInt64(IFormatProvider provider)
    {
      return (Int64)this;
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
				return Convert.ChangeType((BigInteger)this, conversionType, provider);
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

		public override string ToString()
		{
			return numerator + "/" + denominator;
		}
	}
}
