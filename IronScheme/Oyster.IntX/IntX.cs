/*

Copyright (c) 2005-2010, Andriy Kozachuk a.k.a. Oyster
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice, this
  list of conditions and the following disclaimer in the documentation and/or
  other materials provided with the distribution.

* Neither the name of Andriy Kozachuk nor the names of its contributors may be
  used to endorse or promote products derived from this software without
  specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*/

// TODO: move IronScheme extras to partial file.

using System;
using System.Diagnostics;

namespace Oyster.Math
{
	/// <summary>
	/// Numeric class which represents arbitrary-precision integers.
	/// (c) Andriy Kozachuk a.k.a. Oyster [dev.oyster@gmail.com] 2005-2010
	/// </summary>
  [Serializable]
	sealed public class IntX : IConvertible,
    IEquatable<IntX>, IEquatable<int>, IEquatable<uint>, IEquatable<long>, IEquatable<ulong>,
		IComparable, IComparable<IntX>, IComparable<int>, IComparable<uint>, IComparable<long>, IComparable<ulong>
	{
#if DEBUG

		/// <summary>
		/// Lock for maximal error during FHT rounding (debug-mode only).
		/// </summary>
		static readonly internal object _maxFhtRoundErrorLock = new object();

		/// <summary>
		/// Maximal error during FHT rounding (debug-mode only).
		/// </summary>
		static public double MaxFhtRoundError;

#endif

		#region Static fields

		static IntXGlobalSettings _globalSettings = new IntXGlobalSettings();

		#endregion Static fields

		#region Internal fields

		internal uint[] _digits; // big integer digits
		internal uint _length; // big integer digits length
		internal bool _negative; // big integer sign ("-" if true)

		//IntXSettings _settings = new IntXSettings(GlobalSettings);

		#endregion Internal fields

		#region Constructors

		/// <summary>
		/// Creates new big integer with zero value.
		/// </summary>
		public IntX() : this(0) {}

		/// <summary>
		/// Creates new big integer from integer value.
		/// </summary>
		/// <param name="value">Integer value to create big integer from.</param>
		public IntX(int value)
		{
			if (value == 0)
			{
				// Very specific fast processing for zero values
				InitFromZero();
			}
			else
			{
				// Prepare internal fields
				_digits = new uint[_length = 1];

				// Fill the only big integer digit
				DigitHelper.ToUInt32WithSign(value, out _digits[0], out _negative);
			}
		}

		/// <summary>
		/// Creates new big integer from unsigned integer value.
		/// </summary>
		/// <param name="value">Unsigned integer value to create big integer from.</param>
		[CLSCompliant(false)]
		public IntX(uint value)
		{
			if (value == 0)
			{
				// Very specific fast processing for zero values
				InitFromZero();
			}
			else
			{
				// Prepare internal fields
				_digits = new uint[] { value };
				_length = 1;
			}
		}

		/// <summary>
		/// Creates new big integer from long value.
		/// </summary>
		/// <param name="value">Long value to create big integer from.</param>
		public IntX(long value)
		{
			if (value == 0)
			{
				// Very specific fast processing for zero values
				InitFromZero();
			}
			else
			{
				// Fill the only big integer digit
				ulong newValue;
				DigitHelper.ToUInt64WithSign(value, out newValue, out _negative);
				InitFromUlong(newValue);
			}
		}

		/// <summary>
		/// Creates new big integer from unsigned long value.
		/// </summary>
		/// <param name="value">Unsigned long value to create big integer from.</param>
		[CLSCompliant(false)]
		public IntX(ulong value)
		{
			if (value == 0)
			{
				// Very specific fast processing for zero values
				InitFromZero();
			}
			else
			{
				InitFromUlong(value);
			}
		}

		/// <summary>
		/// Creates new big integer from array of it's "digits".
		/// Digit with lower index has less weight.
		/// </summary>
		/// <param name="digits">Array of <see cref="IntX" /> digits.</param>
		/// <param name="negative">True if this number is negative.</param>
		/// <exception cref="ArgumentNullException"><paramref name="digits" /> is a null reference.</exception>
		[CLSCompliant(false)]
		public IntX(uint[] digits, bool negative)
		{
			// Exceptions
			if (digits == null)
			{
				throw new ArgumentNullException("values");
			}

			InitFromDigits(digits, negative, DigitHelper.GetRealDigitsLength(digits, (uint)digits.LongLength));
		}


		/// <summary>
		/// Creates new <see cref="IntX" /> from string.
		/// </summary>
		/// <param name="value">Number as string.</param>
		public IntX(string value)
		{
			IntX intX = Parse(value);
			InitFromIntX(intX);
		}

		/// <summary>
		/// Creates new <see cref="IntX" /> from string.
		/// </summary>
		/// <param name="value">Number as string.</param>
		/// <param name="numberBase">Number base.</param>
		[CLSCompliant(false)]
		public IntX(string value, uint numberBase)
		{
			IntX intX = Parse(value, numberBase);
			InitFromIntX(intX);
		}


		/// <summary>
		/// Copy constructor.
		/// </summary>
		/// <param name="value">Value to copy from.</param>
		/// <exception cref="ArgumentNullException"><paramref name="value" /> is a null reference.</exception>
		public IntX(IntX value)
		{
			// Exceptions
			if (value == null)
			{
				throw new ArgumentNullException("value");
			}

			InitFromIntX(value);
		}


		/// <summary>
		/// Creates new empty big integer with desired sign and length.
		/// 
		/// For internal use.
		/// </summary>
		/// <param name="length">Desired digits length.</param>
		/// <param name="negative">Desired integer sign.</param>
		internal IntX(uint length, bool negative)
		{
			_digits = new uint[_length = length];
			_negative = negative;
		}

		/// <summary>
		/// Creates new big integer from array of it's "digits" but with given length.
		/// Digit with lower index has less weight.
		/// 
		/// For internal use.
		/// </summary>
		/// <param name="digits">Array of <see cref="IntX" /> digits.</param>
		/// <param name="negative">True if this number is negative.</param>
		/// <param name="length">Length to use for internal digits array.</param>
		/// <exception cref="ArgumentNullException"><paramref name="digits" /> is a null reference.</exception>
		internal IntX(uint[] digits, bool negative, uint length)
		{
			// Exceptions
			if (digits == null)
			{
				throw new ArgumentNullException("values");
			}

			InitFromDigits(digits, negative, length);
		}

		#endregion Constructors

		#region Static public properties

		/// <summary>
		/// <see cref="IntX" /> global settings.
		/// </summary>
		static public IntXGlobalSettings GlobalSettings
		{
			get { return _globalSettings; }
		}

		#endregion Static public properties

		#region Public properties

		/// <summary>
		/// <see cref="IntX" /> instance settings.
		/// </summary>
		public IntXSettings Settings
		{
			get { return new IntXSettings(GlobalSettings); }
		}

		#endregion Public properties

		#region Operators

		#region operator==

		/// <summary>
		/// Compares two <see cref="IntX" /> objects and returns true if their internal state is equal.
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second big integer.</param>
		/// <returns>True if equals.</returns>
		static public bool operator ==(IntX int1, IntX int2)
		{
			return OpHelper.Cmp(int1, int2, false) == 0;
		}

		/// <summary>
		/// Compares <see cref="IntX" /> object with integer and returns true if their internal state is equal.
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second integer.</param>
		/// <returns>True if equals.</returns>
		static public bool operator ==(IntX int1, int int2)
		{
			return OpHelper.Cmp(int1, int2) == 0;
		}

		/// <summary>
		/// Compares integer with <see cref="IntX" /> object and returns true if their internal state is equal.
		/// </summary>
		/// <param name="int1">First integer.</param>
		/// <param name="int2">Second big integer.</param>
		/// <returns>True if equals.</returns>
		static public bool operator ==(int int1, IntX int2)
		{
			return OpHelper.Cmp(int2, int1) == 0;
		}

		/// <summary>
		/// Compares <see cref="IntX" /> object with unsinged integer and returns true if their internal state is equal.
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second unsinged integer.</param>
		/// <returns>True if equals.</returns>
		[CLSCompliant(false)]
		static public bool operator ==(IntX int1, uint int2)
		{
			return OpHelper.Cmp(int1, int2) == 0;
		}

		/// <summary>
		/// Compares unsinged integer with <see cref="IntX" /> object and returns true if their internal state is equal.
		/// </summary>
		/// <param name="int1">First unsinged integer.</param>
		/// <param name="int2">Second big integer.</param>
		/// <returns>True if equals.</returns>
		[CLSCompliant(false)]
		static public bool operator ==(uint int1, IntX int2)
		{
			return OpHelper.Cmp(int2, int1) == 0;
		}

		#endregion operator==

		#region operator!=

		/// <summary>
		/// Compares two <see cref="IntX" /> objects and returns true if their internal state is not equal.
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second big integer.</param>
		/// <returns>True if not equals.</returns>
		static public bool operator !=(IntX int1, IntX int2)
		{
			return OpHelper.Cmp(int1, int2, false) != 0;
		}

		/// <summary>
		/// Compares <see cref="IntX" /> object with integer and returns true if their internal state is not equal.
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second integer.</param>
		/// <returns>True if not equals.</returns>
		static public bool operator !=(IntX int1, int int2)
		{
			return OpHelper.Cmp(int1, int2) != 0;
		}

		/// <summary>
		/// Compares integer with <see cref="IntX" /> object and returns true if their internal state is not equal.
		/// </summary>
		/// <param name="int1">First integer.</param>
		/// <param name="int2">Second big integer.</param>
		/// <returns>True if not equals.</returns>
		static public bool operator !=(int int1, IntX int2)
		{
			return OpHelper.Cmp(int2, int1) != 0;
		}

		/// <summary>
		/// Compares <see cref="IntX" /> object with unsigned integer and returns true if their internal state is not equal.
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second unsigned integer.</param>
		/// <returns>True if not equals.</returns>
		[CLSCompliant(false)]
		static public bool operator !=(IntX int1, uint int2)
		{
			return OpHelper.Cmp(int1, int2) != 0;
		}

		/// <summary>
		/// Compares unsigned integer with <see cref="IntX" /> object and returns true if their internal state is not equal.
		/// </summary>
		/// <param name="int1">First unsigned integer.</param>
		/// <param name="int2">Second big integer.</param>
		/// <returns>True if not equals.</returns>
		[CLSCompliant(false)]
		static public bool operator !=(uint int1, IntX int2)
		{
			return OpHelper.Cmp(int2, int1) != 0;
		}

		#endregion operator!=

		#region operator>

		/// <summary>
		/// Compares two <see cref="IntX" /> objects and returns true if first is greater.
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second big integer.</param>
		/// <returns>True if first is greater.</returns>
		static public bool operator >(IntX int1, IntX int2)
		{
			return OpHelper.Cmp(int1, int2, true) > 0;
		}

		/// <summary>
		/// Compares <see cref="IntX" /> object with integer and returns true if first is greater.
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second integer.</param>
		/// <returns>True if first is greater.</returns>
		static public bool operator >(IntX int1, int int2)
		{
			return OpHelper.Cmp(int1, int2) > 0;
		}

		/// <summary>
		/// Compares integer with <see cref="IntX" /> object and returns true if first is greater.
		/// </summary>
		/// <param name="int1">First integer.</param>
		/// <param name="int2">Second big integer.</param>
		/// <returns>True if first is greater.</returns>
		static public bool operator >(int int1, IntX int2)
		{
			return OpHelper.Cmp(int2, int1) < 0;
		}

		/// <summary>
		/// Compares <see cref="IntX" /> object with unsigned integer and returns true if first is greater.
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second unsigned integer.</param>
		/// <returns>True if first is greater.</returns>
		[CLSCompliant(false)]
		static public bool operator >(IntX int1, uint int2)
		{
			return OpHelper.Cmp(int1, int2) > 0;
		}

		/// <summary>
		/// Compares unsigned integer with <see cref="IntX" /> object and returns true if first is greater.
		/// </summary>
		/// <param name="int1">First unsigned integer.</param>
		/// <param name="int2">Second big integer.</param>
		/// <returns>True if first is greater.</returns>
		[CLSCompliant(false)]
		static public bool operator >(uint int1, IntX int2)
		{
			return OpHelper.Cmp(int2, int1) < 0;
		}

		#endregion operator>

		#region operator>=

		/// <summary>
		/// Compares two <see cref="IntX" /> objects and returns true if first is greater or equal.
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second big integer.</param>
		/// <returns>True if first is greater or equal.</returns>
		static public bool operator >=(IntX int1, IntX int2)
		{
			return OpHelper.Cmp(int1, int2, true) >= 0;
		}

		/// <summary>
		/// Compares <see cref="IntX" /> object with integer and returns true if first is greater or equal.
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second integer.</param>
		/// <returns>True if first is greater or equal.</returns>
		static public bool operator >=(IntX int1, int int2)
		{
			return OpHelper.Cmp(int1, int2) >= 0;
		}

		/// <summary>
		/// Compares integer with <see cref="IntX" /> object and returns true if first is greater or equal.
		/// </summary>
		/// <param name="int1">First integer.</param>
		/// <param name="int2">Second big integer.</param>
		/// <returns>True if first is greater or equal.</returns>
		static public bool operator >=(int int1, IntX int2)
		{
			return OpHelper.Cmp(int2, int1) <= 0;
		}

		/// <summary>
		/// Compares <see cref="IntX" /> object with unsinged integer and returns true if first is greater or equal.
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second unsinged integer.</param>
		/// <returns>True if first is greater or equal.</returns>
		[CLSCompliant(false)]
		static public bool operator >=(IntX int1, uint int2)
		{
			return OpHelper.Cmp(int1, int2) >= 0;
		}

		/// <summary>
		/// Compares unsinged integer with <see cref="IntX" /> object and returns true if first is greater or equal.
		/// </summary>
		/// <param name="int1">First unsinged integer.</param>
		/// <param name="int2">Second big integer.</param>
		/// <returns>True if first is greater or equal.</returns>
		[CLSCompliant(false)]
		static public bool operator >=(uint int1, IntX int2)
		{
			return OpHelper.Cmp(int2, int1) <= 0;
		}

		#endregion operator>=

		#region operator<

		/// <summary>
		/// Compares two <see cref="IntX" /> objects and returns true if first is lighter.
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second big integer.</param>
		/// <returns>True if first is lighter.</returns>
		static public bool operator <(IntX int1, IntX int2)
		{
			return OpHelper.Cmp(int1, int2, true) < 0;
		}

		/// <summary>
		/// Compares <see cref="IntX" /> object with integer and returns true if first is lighter.
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second integer.</param>
		/// <returns>True if first is lighter.</returns>
		static public bool operator <(IntX int1, int int2)
		{
			return OpHelper.Cmp(int1, int2) < 0;
		}

		/// <summary>
		/// Compares integer with <see cref="IntX" /> object and returns true if first is lighter.
		/// </summary>
		/// <param name="int1">First integer.</param>
		/// <param name="int2">Second big integer.</param>
		/// <returns>True if first is lighter.</returns>
		static public bool operator <(int int1, IntX int2)
		{
			return OpHelper.Cmp(int2, int1) > 0;
		}

		/// <summary>
		/// Compares <see cref="IntX" /> object with unsinged integer and returns true if first is lighter.
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second unsinged integer.</param>
		/// <returns>True if first is lighter.</returns>
		[CLSCompliant(false)]
		static public bool operator <(IntX int1, uint int2)
		{
			return OpHelper.Cmp(int1, int2) < 0;
		}

		/// <summary>
		/// Compares unsinged integer with <see cref="IntX" /> object and returns true if first is lighter.
		/// </summary>
		/// <param name="int1">First unsinged integer.</param>
		/// <param name="int2">Second big integer.</param>
		/// <returns>True if first is lighter.</returns>
		[CLSCompliant(false)]
		static public bool operator <(uint int1, IntX int2)
		{
			return OpHelper.Cmp(int2, int1) > 0;
		}

		#endregion operator<

		#region operator<=

		/// <summary>
		/// Compares two <see cref="IntX" /> objects and returns true if first is lighter or equal.
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second big integer.</param>
		/// <returns>True if first is lighter or equal.</returns>
		static public bool operator <=(IntX int1, IntX int2)
		{
			return OpHelper.Cmp(int1, int2, true) <= 0;
		}

		/// <summary>
		/// Compares <see cref="IntX" /> object with integer and returns true if first is lighter or equal.
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second integer.</param>
		/// <returns>True if first is lighter or equal.</returns>
		static public bool operator <=(IntX int1, int int2)
		{
			return OpHelper.Cmp(int1, int2) <= 0;
		}

		/// <summary>
		/// Compares integer with <see cref="IntX" /> object and returns true if first is lighter or equal.
		/// </summary>
		/// <param name="int1">First integer.</param>
		/// <param name="int2">Second big integer.</param>
		/// <returns>True if first is lighter or equal.</returns>
		static public bool operator <=(int int1, IntX int2)
		{
			return OpHelper.Cmp(int2, int1) >= 0;
		}

		/// <summary>
		/// Compares <see cref="IntX" /> object with unsinged integer and returns true if first is lighter or equal.
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second unsinged integer.</param>
		/// <returns>True if first is lighter or equal.</returns>
		[CLSCompliant(false)]
		static public bool operator <=(IntX int1, uint int2)
		{
			return OpHelper.Cmp(int1, int2) <= 0;
		}

		/// <summary>
		/// Compares unsinged integer with <see cref="IntX" /> object and returns true if first is lighter or equal.
		/// </summary>
		/// <param name="int1">First unsinged integer.</param>
		/// <param name="int2">Second big integer.</param>
		/// <returns>True if first is lighter or equal.</returns>
		[CLSCompliant(false)]
		static public bool operator <=(uint int1, IntX int2)
		{
			return OpHelper.Cmp(int2, int1) >= 0;
		}

		#endregion operator<=

		#region operator+ and operator-

		/// <summary>
		/// Adds one <see cref="IntX" /> object to another.
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second big integer.</param>
		/// <returns>Addition result.</returns>
		static public IntX operator +(IntX int1, IntX int2)
		{
			return OpHelper.AddSub(int1, int2, false);
		}

		/// <summary>
		/// Subtracts one <see cref="IntX" /> object from another.
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second big integer.</param>
		/// <returns>Subtraction result.</returns>
		static public IntX operator -(IntX int1, IntX int2)
		{
			return OpHelper.AddSub(int1, int2, true);
		}

		#endregion operator+ and operator-

		#region operator*

		/// <summary>
		/// Multiplies one <see cref="IntX" /> object on another.
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second big integer.</param>
		/// <returns>Multiply result.</returns>
		static public IntX operator *(IntX int1, IntX int2)
		{
			return MultiplyManager.GetCurrentMultiplier().Multiply(int1, int2);
		}

		#endregion operator*

		#region operator/ and operator%

		/// <summary>
		/// Divides one <see cref="IntX" /> object by another.
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second big integer.</param>
		/// <returns>Division result.</returns>
		static public IntX operator /(IntX int1, IntX int2)
		{
			IntX modRes;
			return DivideManager.GetCurrentDivider().DivMod(int1, int2, out modRes, DivModResultFlags.Div);
		}

		/// <summary>
		/// Divides one <see cref="IntX" /> object by another and returns division modulo.
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second big integer.</param>
		/// <returns>Modulo result.</returns>
		static public IntX operator %(IntX int1, IntX int2)
		{
			IntX modRes;
			DivideManager.GetCurrentDivider().DivMod(int1, int2, out modRes, DivModResultFlags.Mod);
			return modRes;
		}

		#endregion operator/ and operator%

		#region operator<< and operator>>

		/// <summary>
		/// Shifts <see cref="IntX" /> object on selected bits count to the left.
		/// </summary>
		/// <param name="intX">Big integer.</param>
		/// <param name="shift">Bits count.</param>
		/// <returns>Shifting result.</returns>
		static public IntX operator <<(IntX intX, int shift)
		{
			return OpHelper.Sh(intX, shift, true);
		}

		/// <summary>
		/// Shifts <see cref="IntX" /> object on selected bits count to the right.
		/// </summary>
		/// <param name="intX">Big integer.</param>
		/// <param name="shift">Bits count.</param>
		/// <returns>Shifting result.</returns>
		static public IntX operator >>(IntX intX, int shift)
		{
			return OpHelper.Sh(intX, shift, false);
		}

		#endregion operator<< and operator>>

		#region +, -, ++, -- unary operators

		/// <summary>
		/// Returns the same <see cref="IntX" /> value.
		/// </summary>
		/// <param name="value">Initial value.</param>
		/// <returns>The same value, but new object.</returns>
		/// <exception cref="ArgumentNullException"><paramref name="value" /> is a null reference.</exception>
		static public IntX operator +(IntX value)
		{
			// Exception
			if (ReferenceEquals(value, null))
			{
				throw new ArgumentNullException("value");
			}

			return new IntX(value);
		}

		/// <summary>
		/// Returns the same <see cref="IntX" /> value, but with other sign.
		/// </summary>
		/// <param name="value">Initial value.</param>
		/// <returns>The same value, but with other sign.</returns>
		/// <exception cref="ArgumentNullException"><paramref name="value" /> is a null reference.</exception>
		static public IntX operator -(IntX value)
		{
			// Exception
			if (ReferenceEquals(value, null))
			{
				throw new ArgumentNullException("value");
			}

			IntX newValue = new IntX(value);
			if (newValue._length != 0)
			{
				newValue._negative = !newValue._negative;
			}
			return newValue;
		}

		/// <summary>
		/// Returns increased <see cref="IntX" /> value.
		/// </summary>
		/// <param name="value">Initial value.</param>
		/// <returns>Increased value.</returns>
		/// <exception cref="ArgumentNullException"><paramref name="value" /> is a null reference.</exception>
		static public IntX operator ++(IntX value)
		{
			// Exception
			if (ReferenceEquals(value, null))
			{
				throw new ArgumentNullException("value");
			}

			return value + 1U;
		}

		/// <summary>
		/// Returns decreased <see cref="IntX" /> value.
		/// </summary>
		/// <param name="value">Initial value.</param>
		/// <returns>Decreased value.</returns>
		/// <exception cref="ArgumentNullException"><paramref name="value" /> is a null reference.</exception>
		static public IntX operator --(IntX value)
		{
			// Exception
			if (ReferenceEquals(value, null))
			{
				throw new ArgumentNullException("value");
			}

			return value - 1U;
		}

		#endregion +, -, ++, -- unary operators

		#region Conversion operators

		#region To IntX (Implicit)

		/// <summary>
		/// Implicitly converts <see cref="Int32" /> to <see cref="IntX" />.
		/// </summary>
		/// <param name="value">Value to convert.</param>
		/// <returns>Conversion result.</returns>
		static public implicit operator IntX(int value)
		{
			return new IntX(value);
		}

		/// <summary>
		/// Implicitly converts <see cref="UInt32" /> to <see cref="IntX" />.
		/// </summary>
		/// <param name="value">Value to convert.</param>
		/// <returns>Conversion result.</returns>
		[CLSCompliant(false)]
		static public implicit operator IntX(uint value)
		{
			return new IntX(value);
		}

		/// <summary>
		/// Implicitly converts <see cref="UInt16" /> to <see cref="IntX" />.
		/// </summary>
		/// <param name="value">Value to convert.</param>
		/// <returns>Conversion result.</returns>
		[CLSCompliant(false)]
		static public implicit operator IntX(ushort value)
		{
			return new IntX(value);
		}

		/// <summary>
		/// Implicitly converts <see cref="Int64" /> to <see cref="IntX" />.
		/// </summary>
		/// <param name="value">Value to convert.</param>
		/// <returns>Conversion result.</returns>
		static public implicit operator IntX(long value)
		{
			return new IntX(value);
		}

		/// <summary>
		/// Implicitly converts <see cref="UInt64" /> to <see cref="IntX" />.
		/// </summary>
		/// <param name="value">Value to convert.</param>
		/// <returns>Conversion result.</returns>
		[CLSCompliant(false)]
		static public implicit operator IntX(ulong value)
		{
			return new IntX(value);
		}

		#endregion To IntX (Implicit)

		#region From IntX (Explicit)

		/// <summary>
		/// Explicitly converts <see cref="IntX" /> to <see cref="int" />.
		/// </summary>
		/// <param name="value">Value to convert.</param>
		/// <returns>Conversion result.</returns>
    static public explicit operator int(IntX value)
    {
			int res = (int)(uint)value;
			return value._negative ? -res : res;
    }

		/// <summary>
		/// Explicitly converts <see cref="IntX" /> to <see cref="uint" />.
		/// </summary>
		/// <param name="value">Value to convert.</param>
		/// <returns>Conversion result.</returns>
    [CLSCompliant(false)]
    static public explicit operator uint(IntX value)
    {
			if (value == null)
      {
				throw new ArgumentNullException("value");
      }

			if (value._length == 0) return 0;
      return value._digits[0];
    }

		/// <summary>
		/// Explicitly converts <see cref="IntX" /> to <see cref="long" />.
		/// </summary>
		/// <param name="value">Value to convert.</param>
		/// <returns>Conversion result.</returns>
    static public explicit operator long(IntX value)
    {
			long res = (long)(ulong)value;
			return value._negative ? -res : res;
    }

		/// <summary>
		/// Explicitly converts <see cref="IntX" /> to <see cref="ulong" />.
		/// </summary>
		/// <param name="value">Value to convert.</param>
		/// <returns>Conversion result.</returns>
    [CLSCompliant(false)]
    static public explicit operator ulong(IntX value)
    {
      ulong res = (uint)value;
      if (value._length > 1)
      {
        res |= (ulong)value._digits[1] << Constants.DigitBitCount;
      }
      return res;
    }

		#endregion Conversion operators

    #region bitwise

    public static IntX operator ~(IntX x)
    {
      if (object.ReferenceEquals(x, null))
      {
        throw new ArgumentNullException("x");
      }
      return -(x + 1);
    }


    private static uint extend(uint v, ref bool seenNonZero)
    {
      if (seenNonZero)
      {
        return ~v;
      }
      else
      {
        if (v == 0)
        {
          return 0;
        }
        else
        {
          seenNonZero = true;
          return ~v + 1;
        }
      }
    }

    private static uint getOne(bool isNeg, uint[] data, int i, ref bool seenNonZero)
    {
      if (i < data.Length)
      {
        uint ret = data[i];
        return isNeg ? extend(ret, ref seenNonZero) : ret;
      }
      else
      {
        return isNeg ? uint.MaxValue : 0;
      }
    }

    private static uint[] resize(uint[] v, int len)
    {
      if (v.Length == len) return v;
      uint[] ret = new uint[len];
      int n = System.Math.Min(v.Length, len);
      for (int i = 0; i < n; i++)
      {
        ret[i] = v[i];
      }
      return ret;
    }

    /// <summary>
    /// Do an in-place twos complement of d and also return the result.
    /// </summary>
    private static uint[] makeTwosComplement(uint[] d)
    {
      // first do complement and +1 as long as carry is needed
      int i = 0;
      uint v = 0;
      for (; i < d.Length; i++)
      {
        v = ~d[i] + 1;
        d[i] = v;
        if (v != 0) { i++; break; }
      }

      if (v != 0)
      {
        // now ones complement is sufficient
        for (; i < d.Length; i++)
        {
          d[i] = ~d[i];
        }
      }
      else
      {
        //??? this is weird
        d = resize(d, d.Length + 1);
        d[d.Length - 1] = 1;
      }
      return d;
    }

    public static IntX BitwiseAnd(IntX x, IntX y)
    {
      return x & y;
    }

    public static IntX operator &(IntX x, IntX y)
    {
      if (object.ReferenceEquals(x, null))
      {
        throw new ArgumentNullException("x");
      }
      if (object.ReferenceEquals(y, null))
      {
        throw new ArgumentNullException("y");
      }
      uint xl = x._length, yl = y._length;
      uint[] xd = x._digits, yd = y._digits;

      uint zl = System.Math.Max(xl, yl);
      uint[] zd = new uint[zl];

      bool negx = x._negative, negy = y._negative;
      bool seenNonZeroX = false, seenNonZeroY = false;
      for (int i = 0; i < zl; i++)
      {
        uint xu = getOne(negx, xd, i, ref seenNonZeroX);
        uint yu = getOne(negy, yd, i, ref seenNonZeroY);
        zd[i] = xu & yu;
      }

      if (negx && negy)
      {

        return new IntX(makeTwosComplement(zd), true);
      }
      else if (negx || negy)
      {
        return new IntX(zd, false);
      }
      else
      {
        return new IntX(zd, false);
      }
    }

    public static IntX BitwiseOr(IntX x, IntX y)
    {
      return x | y;
    }

    public static IntX operator |(IntX x, IntX y)
    {
      if (object.ReferenceEquals(x, null))
      {
        throw new ArgumentNullException("x");
      }
      if (object.ReferenceEquals(y, null))
      {
        throw new ArgumentNullException("y");
      }
      uint xl = x._length, yl = y._length;
      uint[] xd = x._digits, yd = y._digits;

      uint zl = System.Math.Max(xl, yl);
      uint[] zd = new uint[zl];

      bool negx = x._negative, negy = y._negative;
      bool seenNonZeroX = false, seenNonZeroY = false;
      for (int i = 0; i < zl; i++)
      {
        uint xu = getOne(negx, xd, i, ref seenNonZeroX);
        uint yu = getOne(negy, yd, i, ref seenNonZeroY);
        zd[i] = xu | yu;
      }

      if (negx && negy)
      {
        return new IntX(makeTwosComplement(zd), true);
      }
      else if (negx || negy)
      {
        return new IntX(makeTwosComplement(zd), true);
      }
      else
      {
        return new IntX(zd, false);
      }
    }

    public static IntX Xor(IntX x, IntX y)
    {
      return x ^ y;
    }

    public static IntX operator ^(IntX x, IntX y)
    {
      if (object.ReferenceEquals(x, null))
      {
        throw new ArgumentNullException("x");
      }
      if (object.ReferenceEquals(y, null))
      {
        throw new ArgumentNullException("y");
      }
      uint xl = x._length, yl = y._length;
      uint[] xd = x._digits, yd = y._digits;

      uint zl = System.Math.Max(xl, yl);
      uint[] zd = new uint[zl];

      bool negx = x._negative, negy = y._negative;
      bool seenNonZeroX = false, seenNonZeroY = false;
      for (int i = 0; i < zl; i++)
      {
        uint xu = getOne(negx, xd, i, ref seenNonZeroX);
        uint yu = getOne(negy, yd, i, ref seenNonZeroY);
        zd[i] = xu ^ yu;
      }

      if (negx && negy)
      {
        return new IntX(zd, false);
      }
      else if (negx || negy)
      {
        return new IntX(makeTwosComplement(zd), true);
      }
      else
      {
        return new IntX(zd, false);
      }
    }

    #endregion

    #endregion Operators

    #endregion

    #region Math static methods

    #region Multiply

    /// <summary>
		/// Multiplies one <see cref="IntX" /> object on another.
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second big integer.</param>
		/// <param name="mode">Multiply mode set explicitly.</param>
		/// <returns>Multiply result.</returns>
		static public IntX Multiply(IntX int1, IntX int2, MultiplyMode mode)
		{
			return MultiplyManager.GetMultiplier(mode).Multiply(int1, int2);
		}

		#endregion Multiply

		#region Divide/modulo

		/// <summary>
		/// Divides one <see cref="IntX" /> object by another.
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second big integer.</param>
		/// <param name="mode">Divide mode.</param>
		/// <returns>Division result.</returns>
		static public IntX Divide(IntX int1, IntX int2, DivideMode mode)
		{
			IntX modRes;
			return DivideManager.GetDivider(mode).DivMod(int1, int2, out modRes, DivModResultFlags.Div);
		}

		/// <summary>
		/// Divides one <see cref="IntX" /> object by another and returns division modulo.
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second big integer.</param>
		/// <param name="mode">Divide mode.</param>
		/// <returns>Modulo result.</returns>
		static public IntX Modulo(IntX int1, IntX int2, DivideMode mode)
		{
			IntX modRes;
			DivideManager.GetDivider(mode).DivMod(int1, int2, out modRes, DivModResultFlags.Mod);
			return modRes;
		}

		/// <summary>
		/// Divides one <see cref="IntX" /> object on another.
		/// Returns both divident and remainder
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second big integer.</param>
		/// <param name="modRes">Remainder big integer.</param>
		/// <returns>Division result.</returns>
		static public IntX DivideModulo(IntX int1, IntX int2, out IntX modRes)
		{
			return DivideManager.GetCurrentDivider().DivMod(int1, int2, out modRes, DivModResultFlags.Div | DivModResultFlags.Mod);
		}

		/// <summary>
		/// Divides one <see cref="IntX" /> object on another.
		/// Returns both divident and remainder
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second big integer.</param>
		/// <param name="modRes">Remainder big integer.</param>
		/// <param name="mode">Divide mode.</param>
		/// <returns>Division result.</returns>
		static public IntX DivideModulo(IntX int1, IntX int2, out IntX modRes, DivideMode mode)
		{
			return DivideManager.GetDivider(mode).DivMod(int1, int2, out modRes, DivModResultFlags.Div | DivModResultFlags.Mod);
		}

		#endregion Divide/modulo

		#region Pow

		/// <summary>
		/// Returns a specified big integer raised to the specified power.
		/// </summary>
		/// <param name="value">Number to raise.</param>
		/// <param name="power">Power.</param>
		/// <returns>Number in given power.</returns>
		[CLSCompliant(false)]
		static public IntX Pow(IntX value, uint power)
		{
			return OpHelper.Pow(value, power, GlobalSettings.MultiplyMode);
		}

		/// <summary>
		/// Returns a specified big integer raised to the specified power.
		/// </summary>
		/// <param name="value">Number to raise.</param>
		/// <param name="power">Power.</param>
		/// <param name="multiplyMode">Multiply mode set explicitly.</param>
		/// <returns>Number in given power.</returns>
		[CLSCompliant(false)]
		static public IntX Pow(IntX value, uint power, MultiplyMode multiplyMode)
		{
			return OpHelper.Pow(value, power, multiplyMode);
		}

		#endregion Pow

		#endregion Math static methods

		#region ToString override

		/// <summary>
		/// Returns decimal string representation of this <see cref="IntX" /> object.
		/// </summary>
		/// <returns>Decimal number in string.</returns>
		override public string ToString()
		{
			return ToString(10U, true);
		}

		/// <summary>
		/// Returns string representation of this <see cref="IntX" /> object in given base.
		/// </summary>
		/// <param name="numberBase">Base of system in which to do output.</param>
		/// <returns>Object string representation.</returns>
		[CLSCompliant(false)]
		public string ToString(uint numberBase)
		{
			return ToString(numberBase, true);
		}

		/// <summary>
		/// Returns string representation of this <see cref="IntX" /> object in given base.
		/// </summary>
		/// <param name="numberBase">Base of system in which to do output.</param>
		/// <param name="upperCase">Use uppercase for bases from 11 to 16 (which use letters A-F).</param>
		/// <returns>Object string representation.</returns>
		[CLSCompliant(false)]
		public string ToString(uint numberBase, bool upperCase)
		{
			return StringConvertManager.GetStringConverter(Settings.ToStringMode)
				.ToString(this, numberBase, upperCase ? Constants.BaseUpperChars : Constants.BaseLowerChars);
		}

		/// <summary>
		/// Returns string representation of this <see cref="IntX" /> object in given base using custom alphabet.
		/// </summary>
		/// <param name="numberBase">Base of system in which to do output.</param>
		/// <param name="alphabet">Alphabet which contains chars used to represent big integer, char position is coresponding digit value.</param>
		/// <returns>Object string representation.</returns>
		[CLSCompliant(false)]
		public string ToString(uint numberBase, string alphabet)
		{
			StrRepHelper.AssertAlphabet(alphabet, numberBase);
			return StringConvertManager.GetStringConverter(Settings.ToStringMode)
				.ToString(this, numberBase, alphabet.ToCharArray());
		}

		#endregion ToString override

		#region Parsing methods

		/// <summary>
		/// Parses provided string representation of <see cref="IntX" /> object in decimal base.
		/// If number starts from "0" then it's treated as octal; if number starts fropm "0x"
		/// then it's treated as hexadecimal.
		/// </summary>
		/// <param name="value">Number as string.</param>
		/// <returns>Parsed object.</returns>
		static public IntX Parse(string value)
		{
			return ParseManager.GetCurrentParser().Parse(value, 10U, Constants.BaseCharToDigits, true);
		}

		/// <summary>
		/// Parses provided string representation of <see cref="IntX" /> object.
		/// </summary>
		/// <param name="value">Number as string.</param>
		/// <param name="numberBase">Number base.</param>
		/// <returns>Parsed object.</returns>
		[CLSCompliant(false)]
		static public IntX Parse(string value, uint numberBase)
		{
			return ParseManager.GetCurrentParser().Parse(value, numberBase, Constants.BaseCharToDigits, false);
		}

		/// <summary>
		/// Parses provided string representation of <see cref="IntX" /> object using custom alphabet.
		/// </summary>
		/// <param name="value">Number as string.</param>
		/// <param name="numberBase">Number base.</param>
		/// <param name="alphabet">Alphabet which contains chars used to represent big integer, char position is coresponding digit value.</param>
		/// <returns>Parsed object.</returns>
		[CLSCompliant(false)]
		static public IntX Parse(string value, uint numberBase, string alphabet)
		{
			return ParseManager.GetCurrentParser()
				.Parse(value, numberBase, StrRepHelper.CharDictionaryFromAlphabet(alphabet, numberBase), false);
		}

		/// <summary>
		/// Parses provided string representation of <see cref="IntX" /> object in decimal base.
		/// If number starts from "0" then it's treated as octal; if number starts fropm "0x"
		/// then it's treated as hexadecimal.
		/// </summary>
		/// <param name="value">Number as string.</param>
		/// <param name="mode">Parse mode.</param>
		/// <returns>Parsed object.</returns>
		static public IntX Parse(string value, ParseMode mode)
		{
			return ParseManager.GetParser(mode).Parse(value, 10U, Constants.BaseCharToDigits, true);
		}

		/// <summary>
		/// Parses provided string representation of <see cref="IntX" /> object.
		/// </summary>
		/// <param name="value">Number as string.</param>
		/// <param name="numberBase">Number base.</param>
		/// <param name="mode">Parse mode.</param>
		/// <returns>Parsed object.</returns>
		[CLSCompliant(false)]
		static public IntX Parse(string value, uint numberBase, ParseMode mode)
		{
			return ParseManager.GetParser(mode).Parse(value, numberBase, Constants.BaseCharToDigits, false);
		}

		/// <summary>
		/// Parses provided string representation of <see cref="IntX" /> object using custom alphabet.
		/// </summary>
		/// <param name="value">Number as string.</param>
		/// <param name="numberBase">Number base.</param>
		/// <param name="alphabet">Alphabet which contains chars used to represent big integer, char position is coresponding digit value.</param>
		/// <param name="mode">Parse mode.</param>
		/// <returns>Parsed object.</returns>
		[CLSCompliant(false)]
		static public IntX Parse(string value, uint numberBase, string alphabet, ParseMode mode)
		{
			return ParseManager.GetParser(mode)
				.Parse(value, numberBase, StrRepHelper.CharDictionaryFromAlphabet(alphabet, numberBase), false);
		}

		#endregion Parsing methods

		#region IEquatable/Equals/GetHashCode implementation/overrides

		/// <summary>
		/// Returns equality of this <see cref="IntX" /> with another big integer.
		/// </summary>
		/// <param name="n">Big integer to compare with.</param>
		/// <returns>True if equals.</returns>
		public bool Equals(IntX n)
		{
			return base.Equals(n) || this == n;
		}

		/// <summary>
		/// Returns equality of this <see cref="IntX" /> with another integer.
		/// </summary>
		/// <param name="n">Integer to compare with.</param>
		/// <returns>True if equals.</returns>
		public bool Equals(int n)
		{
			return this == n;
		}

		/// <summary>
		/// Returns equality of this <see cref="IntX" /> with another unsigned integer.
		/// </summary>
		/// <param name="n">Unsigned integer to compare with.</param>
		/// <returns>True if equals.</returns>
		[CLSCompliant(false)]
		public bool Equals(uint n)
		{
			return this == n;
		}

		/// <summary>
		/// Returns equality of this <see cref="IntX" /> with another long integer.
		/// </summary>
		/// <param name="n">Long integer to compare with.</param>
		/// <returns>True if equals.</returns>
		public bool Equals(long n)
		{
			return this == n;
		}

		/// <summary>
		/// Returns equality of this <see cref="IntX" /> with another unsigned long integer.
		/// </summary>
		/// <param name="n">Unsigned long integer to compare with.</param>
		/// <returns>True if equals.</returns>
		[CLSCompliant(false)]
		public bool Equals(ulong n)
		{
			return this == n;
		}


		/// <summary>
		/// Returns equality of this <see cref="IntX" /> with another object.
		/// </summary>
		/// <param name="obj">Object to compare with.</param>
		/// <returns>True if equals.</returns>
		override public bool Equals(object obj)
		{
			return obj is IntX && Equals((IntX)obj);
		}

		/// <summary>
		/// Returns hash code for this <see cref="IntX" /> object.
		/// </summary>
		/// <returns>Object hash code.</returns>
		override public int GetHashCode()
		{
			switch (_length)
			{
				case 0:
					return 0;
				case 1:
					return (int)(_digits[0] ^ _length ^ (_negative ? 1 : 0));
				default:
					return (int)(_digits[0] ^ _digits[_length - 1] ^ _length ^ (_negative ? 1 : 0));
			}
		}

		#endregion Equals/GetHashCode implementation/overrides

		#region IComparable implementation

		/// <summary>
		/// Compares current object with another big integer.
		/// </summary>
		/// <param name="n">Big integer to compare with.</param>
		/// <returns>1 if object is bigger than <paramref name="n" />, -1 if object is smaller than <paramref name="n" />, 0 if they are equal.</returns>
		public int CompareTo(IntX n)
		{
			return OpHelper.Cmp(this, n, true);
		}

		/// <summary>
		/// Compares current object with another integer.
		/// </summary>
		/// <param name="n">Integer to compare with.</param>
		/// <returns>1 if object is bigger than <paramref name="n" />, -1 if object is smaller than <paramref name="n" />, 0 if they are equal.</returns>
		public int CompareTo(int n)
		{
			return OpHelper.Cmp(this, n);
		}

		/// <summary>
		/// Compares current object with another unsigned integer.
		/// </summary>
		/// <param name="n">Unsigned integer to compare with.</param>
		/// <returns>1 if object is bigger than <paramref name="n" />, -1 if object is smaller than <paramref name="n" />, 0 if they are equal.</returns>
		[CLSCompliant(false)]
		public int CompareTo(uint n)
		{
			return OpHelper.Cmp(this, n);
		}

		/// <summary>
		/// Compares current object with another long integer.
		/// </summary>
		/// <param name="n">Long integer to compare with.</param>
		/// <returns>1 if object is bigger than <paramref name="n" />, -1 if object is smaller than <paramref name="n" />, 0 if they are equal.</returns>
		public int CompareTo(long n)
		{
			return OpHelper.Cmp(this, n, true);
		}

		/// <summary>
		/// Compares current object with another unsigned long integer.
		/// </summary>
		/// <param name="n">Unsigned long integer to compare with.</param>
		/// <returns>1 if object is bigger than <paramref name="n" />, -1 if object is smaller than <paramref name="n" />, 0 if they are equal.</returns>
		[CLSCompliant(false)]
		public int CompareTo(ulong n)
		{
			return OpHelper.Cmp(this, n, true);
		}

		/// <summary>
		/// Compares current object with another object.
		/// </summary>
		/// <param name="obj">Object to compare with.</param>
		/// <returns>1 if object is bigger than <paramref name="obj" />, -1 if object is smaller than <paramref name="obj" />, 0 if they are equal.</returns>
		public int CompareTo(object obj)
		{
			if (obj is IntX)
			{
				return CompareTo((IntX)obj);
			}
			else if (obj is int)
			{
				return CompareTo((int)obj);
			}
			else if (obj is uint)
			{
				return CompareTo((uint)obj);
			}
			else if (obj is long)
			{
				return CompareTo((long)obj);
			}
			else if (obj is ulong)
			{
				return CompareTo((ulong)obj);
			}

			throw new ArgumentException(Strings.CantCmp, "obj");
		}

		#endregion IComparable implementation

		#region Other public methods

		/// <summary>
		/// Frees extra space not used by digits.
		/// </summary>
		public void Normalize()
		{
			if (_digits.LongLength > _length)
			{
				uint[] newDigits = new uint[_length];
				Array.Copy(_digits, newDigits, _length);
				_digits = newDigits;
			}

			if (_length == 0)
			{
				_negative = false;
			}
		}

		/// <summary>
		/// Retrieves this <see cref="IntX" /> internal state as digits array and sign.
		/// Can be used for serialization and other purposes.
		/// Note: please use constructor instead to clone <see cref="IntX" /> object.
		/// </summary>
		/// <param name="digits">Digits array.</param>
		/// <param name="negative">Is negative integer.</param>
		[CLSCompliant(false)]
		public void GetInternalState(out uint[] digits, out bool negative)
		{
			digits = new uint[_length];
			Array.Copy(_digits, digits, _length);

			negative = _negative;
		}

		#endregion Other public methods

		#region Init utilitary methods

		/// <summary>
		/// Initializes class instance from zero.
		/// For internal use.
		/// </summary>
		void InitFromZero()
		{
			_length = 0;
			_digits = new uint[0];
		}

		/// <summary>
		/// Initializes class instance from <see cref="UInt64" /> value.
		/// Doesn't initialize sign.
		/// For internal use.
		/// </summary>
		/// <param name="value">Unsigned long value.</param>
		void InitFromUlong(ulong value)
		{
			// Divide ulong into 2 uint values
			uint low = (uint)value;
			uint high = (uint)(value >> Constants.DigitBitCount);

			// Prepare internal fields
			if (high == 0)
			{
				_digits = new uint[] { low };
			}
			else
			{
				_digits = new uint[] { low, high };
			}
			_length = (uint)_digits.Length;
		}

		/// <summary>
		/// Initializes class instance from another <see cref="IntX" /> value.
		/// For internal use.
		/// </summary>
		/// <param name="value">Big integer value.</param>
		void InitFromIntX(IntX value)
		{
			_digits = value._digits;
			_length = value._length;
			_negative = value._negative;
		}

		/// <summary>
		/// Initializes class instance from digits array.
		/// For internal use.
		/// </summary>
		/// <param name="digits">Big integer digits.</param>
		/// <param name="negative">Big integer sign.</param>
		/// <param name="length">Big integer length.</param>
		void InitFromDigits(uint[] digits, bool negative, uint length)
		{
			_digits = new uint[_length = length];
			Array.Copy(digits, _digits, System.Math.Min((uint)digits.LongLength, length));
			if (length != 0)
			{
				_negative = negative;
			}
		}

		#endregion Init utilitary methods

		#region Other utilitary methods

		/// <summary>
		/// Frees extra space not used by digits only if auto-normalize is set for the instance.
		/// </summary>
		internal void TryNormalize()
		{
			if (Settings.AutoNormalize)
			{
				Normalize();
			}
		}

		#endregion Other utilitary methods


    public IntX Abs()
    {
      uint[] newdigits = new uint[_digits.Length];
      Array.Copy(_digits, newdigits, newdigits.Length);
      return new IntX(newdigits, false);
    }

    public double ToFloat64()
    {
      double d = 0;
      double f = 1;

      for (int i = 0; i < _length && !double.IsInfinity(d); i++)
      {
        d = d + _digits[i] * f;
        f *= (double)uint.MaxValue + 1;
      }

      if (_negative)
      {
        return -d;
      }

      return d;
    }

    public int ToInt32()
    {
      return (int)this;
    }

    public bool Negative
    {
      get { return _negative; }
    }
    
    [CLSCompliant(false)]
    public uint[] GetBits()
    {
      return _digits;
    }

    public static IntX Create(int n)
    {
      return new IntX(n);
    }

    public IntX Power(int n)
    {
      return Pow(this, (uint)n);
    }

    /// <summary>
    /// Create a BigInteger from a little-endian twos-complement byte array
    /// (inverse of ToByteArray())
    /// </summary>
    public static IntX Create(byte[] v)
    {
      if (v.Length == 0) return Create(0);

      int byteCount = v.Length;
      int unalignedBytes = byteCount % 4;
      int dwordCount = byteCount / 4 + (unalignedBytes == 0 ? 0 : 1);
      uint[] data = new uint[dwordCount];

      bool isNegative = (v[byteCount - 1] & 0x80) == 0x80;

      bool isZero = true;

      // Copy all dwords, except but don't do the last one if it's not a full four bytes
      int curDword, curByte, byteInDword;
      curByte = 3;
      for (curDword = 0; curDword < dwordCount - (unalignedBytes == 0 ? 0 : 1); curDword++)
      {
        byteInDword = 0;
        while (byteInDword < 4)
        {
          if (v[curByte] != 0x00) isZero = false;
          data[curDword] <<= 8;
          data[curDword] |= v[curByte];
          curByte--;
          byteInDword++;
        }
        curByte += 8;
      }

      // Copy the last dword specially if it's not aligned
      if (unalignedBytes != 0)
      {
        if (isNegative) data[dwordCount - 1] = 0xffffffff;
        for (curByte = byteCount - 1; curByte >= byteCount - unalignedBytes; curByte--)
        {
          if (v[curByte] != 0x00) isZero = false;
          data[curDword] <<= 8;
          data[curDword] |= v[curByte];
        }
      }

      if (isZero) return 0;

      if (isNegative)
      {
        makeTwosComplement(data);
      }
      return new IntX(data, isNegative);
    }

    static bool IsNegative(byte[] v)
    {
      return ((v[7] & 0x80) != 0);
    }

    static ushort Exponent(byte[] v)
    {
      return (ushort)((((ushort)(v[7] & 0x7F)) << (ushort)4) | (((ushort)(v[6] & 0xF0)) >> 4));
    }

    static ulong Mantissa(byte[] v)
    {
      uint i1 = ((uint)v[0] | ((uint)v[1] << 8) | ((uint)v[2] << 16) | ((uint)v[3] << 24));
      uint i2 = ((uint)v[4] | ((uint)v[5] << 8) | ((uint)(v[6] & 0xF) << 16));

      return (ulong)((ulong)i1 | ((ulong)i2 << 32));
    }

    private static int bias = 1075;

    public static IntX Create(double v)
    {
      byte[] bytes = System.BitConverter.GetBytes(v);
      ulong mantissa = Mantissa(bytes);
      if (mantissa == 0)
      {
        // 1.0 * 2**exp, we have a power of 2
        int exponent = Exponent(bytes);
        if (exponent == 0) return 0;

        IntX res = IsNegative(bytes) ? -1 : 1;
        res = res << (exponent - 0x3ff);
        return res;
      }
      else
      {
        // 1.mantissa * 2**exp
        int exponent = Exponent(bytes);
        mantissa |= 0x10000000000000ul;
        IntX res = IntX.Create(mantissa);
        res = exponent > bias ? res << (exponent - bias) : res >> (bias - exponent);
        return IsNegative(bytes) ? res * (-1) : res;
      }
    }


    /// <summary>
    /// Return the value of this BigInteger as a little-endian twos-complement
    /// byte array, using the fewest number of bytes possible. If the value is zero,
    /// return an array of one byte whose element is 0x00.
    /// </summary>
    public byte[] ToByteArray()
    {
      // We could probably make this more efficient by eliminating one of the passes.
      // The current code does one pass for uint array -> byte array conversion,
      // and then a another pass to remove unneeded bytes at the top of the array.
      
      uint[] dwords;
      byte highByte;

      if (_negative)
      {
        dwords = (uint[])_digits.Clone();
        makeTwosComplement(dwords);
        highByte = 0xff;
      }
      else
      {
        dwords = _digits;
        highByte = 0x00;
      }

      byte[] bytes = new byte[4 * dwords.Length];
      int curByte = 0;
      uint dword;
      for (int i = 0; i < dwords.Length; i++)
      {
        dword = dwords[i];
        for (int j = 0; j < 4; j++)
        {
          bytes[curByte++] = (byte)(dword & 0xff);
          dword >>= 8;
        }
      }

      // find highest significant byte
      int msb;
      for (msb = bytes.Length - 1; msb > 0; msb--)
      {
        if (bytes[msb] != highByte) break;
      }
      // ensure high bit is 0 if positive, 1 if negative
      bool needExtraByte = true;// (bytes[msb] & 0x80) != (highByte & 0x80);

      byte[] trimmedBytes = new byte[msb + 1 + (needExtraByte ? 1 : 0)];
      Array.Copy(bytes, trimmedBytes, msb + 1);

      if (needExtraByte) trimmedBytes[trimmedBytes.Length - 1] = highByte;

      return trimmedBytes;
    }

    public int BitLength
    {
      get 
      {
        int bc = 0;
        int l = (int)_length - 1;

        if (l < 0)
        {
          return 0;
        }
        uint i = _digits[l];
        while (i > 0)
        {
          bc++;
          i >>= 1;
        }
        return l * 32 + bc;
      }
    }

    #region IConvertible Members

    TypeCode IConvertible.GetTypeCode()
    {
      return TypeCode.Object;
    }

    bool IConvertible.ToBoolean(IFormatProvider provider)
    {
      throw new NotImplementedException();
    }

    byte IConvertible.ToByte(IFormatProvider provider)
    {
      return (byte)(int)this;
    }

    char IConvertible.ToChar(IFormatProvider provider)
    {
      return (char)(int)this;
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
      return ToFloat64();
    }

    short IConvertible.ToInt16(IFormatProvider provider)
    {
      return (Int16)(int)this;
    }

    int IConvertible.ToInt32(IFormatProvider provider)
    {
      return (int)this;
    }

    long IConvertible.ToInt64(IFormatProvider provider)
    {
      return (long)this;
    }

    sbyte IConvertible.ToSByte(IFormatProvider provider)
    {
      return (sbyte)(int)this;
    }

    float IConvertible.ToSingle(IFormatProvider provider)
    {
      return (float)ToFloat64();
    }

    string IConvertible.ToString(IFormatProvider provider)
    {
      return ToString();
    }

    object IConvertible.ToType(Type conversionType, IFormatProvider provider)
    {
      throw new NotImplementedException();
    }

    ushort IConvertible.ToUInt16(IFormatProvider provider)
    {
      return (UInt16)(int)this;
    }

    uint IConvertible.ToUInt32(IFormatProvider provider)
    {
      return (uint)this;
    }

    ulong IConvertible.ToUInt64(IFormatProvider provider)
    {
      return (ulong)this;
    }

    #endregion

    public bool AsInt32(out int ival)
    {
      if (this > int.MaxValue || this < int.MinValue)
      {
        ival = 0;
        return false;
      }
      ival = (int)this;
      return true;
    }

    public bool AsInt64(out long lval)
    {
      if (this > long.MaxValue || this < long.MinValue)
      {
        lval = 0;
        return false;
      }
      lval = (long)this;
      return true;
    }
  }
}
