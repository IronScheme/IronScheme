using System;

namespace Oyster.Math
{
	/// <summary>
	/// Contains helping methods for operations over <see cref="IntX" />.
	/// </summary>
	static internal class OpHelper
	{
		#region Add operation

		/// <summary>
		/// Adds two big integers.
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second big integer.</param>
		/// <returns>Resulting big integer.</returns>
		/// <exception cref="ArgumentException"><paramref name="int1" /> or <paramref name="int2" /> is too big for add operation.</exception>
		static public IntX Add(IntX int1, IntX int2)
		{
			// Process zero values in special way
			if (int2._length == 0) return new IntX(int1);
			if (int1._length == 0)
			{
				IntX x = new IntX(int2);
				x._negative = int1._negative; // always get sign of the first big integer
				return x;
			}

			// Determine big int with lower length
			IntX smallerInt;
			IntX biggerInt;
			DigitHelper.GetMinMaxLengthObjects(int1, int2, out smallerInt, out biggerInt);

			// Check for add operation possibility
			if (biggerInt._length == uint.MaxValue)
			{
				throw new ArgumentException(Strings.IntegerTooBig);
			}

			// Create new big int object of needed length
			IntX newInt = new IntX(biggerInt._length + 1, int1._negative);

			// Do actual addition
			newInt._length = DigitOpHelper.Add(
				biggerInt._digits,
				biggerInt._length,
				smallerInt._digits,
				smallerInt._length,
				newInt._digits);

			// Normalization may be needed
			newInt.TryNormalize();

			return newInt;
		}

		#endregion Add operation

		#region Subtract operation

		/// <summary>
		/// Subtracts two big integers.
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second big integer.</param>
		/// <returns>Resulting big integer.</returns>
		static public IntX Sub(IntX int1, IntX int2)
		{
			// Process zero values in special way
			if (int2._length == 0) return new IntX(int1);
			if (int1._length == 0) return new IntX(-int2);

			// Determine lower big int (without sign)
			IntX smallerInt;
			IntX biggerInt;
			int compareResult = DigitOpHelper.Cmp(int1._digits, int1._length, int2._digits, int2._length);
			if (compareResult == 0) return new IntX(); // integers are equal
			if (compareResult < 0)
			{
				smallerInt = int1;
				biggerInt = int2;
			}
			else
			{
				smallerInt = int2;
				biggerInt = int1;
			}

			// Create new big int object
			IntX newInt = new IntX(biggerInt._length, ReferenceEquals(int1, smallerInt) ^ int1._negative);

			// Do actual subtraction
			newInt._length = DigitOpHelper.Sub(
				biggerInt._digits,
				biggerInt._length,
				smallerInt._digits,
				smallerInt._length,
				newInt._digits);

			// Normalization may be needed
			newInt.TryNormalize();

			return newInt;
		}

		#endregion Subtract operation

		#region Add/Subtract operation - common methods

		/// <summary>
		/// Adds/subtracts one <see cref="IntX" /> to/from another.
		/// Determines which operation to use basing on operands signs.
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second big integer.</param>
		/// <param name="subtract">Was subtraction initially.</param>
		/// <returns>Add/subtract operation result.</returns>
		/// <exception cref="ArgumentNullException"><paramref name="int1" /> or <paramref name="int2" /> is a null reference.</exception>
		static public IntX AddSub(IntX int1, IntX int2, bool subtract)
		{
			// Exceptions
			if (ReferenceEquals(int1, null))
			{
				throw new ArgumentNullException("int1", Strings.CantBeNull);
			}
			else if (ReferenceEquals(int2, null))
			{
				throw new ArgumentNullException("int2", Strings.CantBeNull);
			}

			// Determine real operation type and result sign
			return subtract ^ int1._negative == int2._negative ? Add(int1, int2) : Sub(int1, int2);
		}

		#endregion Add/Subtract operation - common methods

		#region Divide/modulo operation

		/// <summary>
		/// Divides one <see cref="IntX" /> by another.
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second big integer.</param>
		/// <param name="modRes">Remainder big integer.</param>
		/// <param name="resultFlags">Which operation results to return.</param>
		/// <returns>Divident big integer.</returns>
		/// <exception cref="ArgumentNullException"><paramref name="int1" /> or <paramref name="int2" /> is a null reference.</exception>
		/// <exception cref="DivideByZeroException"><paramref name="int2" /> equals zero.</exception>
		static public IntX DivMod(IntX int1, IntX int2, out IntX modRes, DivModResultFlags resultFlags)
		{
			// Null reference exceptions
			if (ReferenceEquals(int1, null))
			{
				throw new ArgumentNullException("int1", Strings.CantBeNull);
			}
			else if (ReferenceEquals(int2, null))
			{
				throw new ArgumentNullException("int2", Strings.CantBeNull);
			}

			// Check if int2 equals zero
			if (int2._length == 0)
			{
				throw new DivideByZeroException();
			}

			// Get flags
			bool divNeeded = (resultFlags & DivModResultFlags.Div) != 0;
			bool modNeeded = (resultFlags & DivModResultFlags.Mod) != 0;

			// Special situation: check if int1 equals zero; in this case zero is always returned
			if (int1._length == 0)
			{
				modRes = modNeeded ? new IntX() : null;
				return divNeeded ? new IntX() : null;
			}

			// Special situation: check if int2 equals one - nothing to divide in this case
			if (int2._length == 1 && int2._digits[0] == 1)
			{
				modRes = modNeeded ? new IntX() : null;
				return divNeeded ? int2._negative ? -int1 : +int1 : null;
			}

			// Get resulting sign
			bool resultNegative = int1._negative ^ int2._negative;

			// Check if int1 > int2
			int compareResult = DigitOpHelper.Cmp(int1._digits, int1._length, int2._digits, int2._length);
			if (compareResult < 0)
			{
				modRes = modNeeded ? new IntX(int1) : null;
				return divNeeded ? new IntX() : null;
			}
			else if (compareResult == 0)
			{
				modRes = modNeeded ? new IntX() : null;
				return divNeeded ? new IntX(resultNegative ? -1 : 1) : null;
			}

			//
			// Actually divide here (by Knuth algorithm)
			//

			// Prepare divident (if needed)
			IntX divRes = null;
			if (divNeeded)
			{
				divRes = new IntX(int1._length - int2._length + 1U, resultNegative);
			}

			// Prepare mod (if needed)
			if (modNeeded)
			{
				modRes = new IntX(int1._length + 1U, int1._negative);
			}
			else
			{
				modRes = null;
			}

			// Call procedure itself
			uint modLength = int1._length;
			uint divLength = DigitOpHelper.DivMod(
				int1._digits,
				modNeeded ? modRes._digits : null,
				ref modLength,
				int2._digits,
				null,
				int2._length,
				divNeeded ? divRes._digits : null,
				resultFlags,
				compareResult);

			// Maybe set new lengths and perform normalization
			if (divNeeded)
			{
				divRes._length = divLength;
				divRes.TryNormalize();
			}
			if (modNeeded)
			{
				modRes._length = modLength;
				modRes.TryNormalize();
			}

			// Return div
			return divRes;
		}

		#endregion Divide/modulo operation

		#region Power operation

		/// <summary>
		/// Returns a specified big integer raised to the specified power.
		/// </summary>
		/// <param name="value">Number to raise.</param>
		/// <param name="power">Power.</param>
		/// <param name="multiplyMode">Multiply mode set explicitly.</param>
		/// <returns>Number in given power.</returns>
		/// <exception cref="ArgumentNullException"><paramref name="value" /> is a null reference.</exception>
		static public IntX Pow(IntX value, uint power, MultiplyMode multiplyMode)
		{
			// Exception
			if (ReferenceEquals(value, null))
			{
				throw new ArgumentNullException("value");
			}

			// Return zero for a zero
			if (value._length == 0) return new IntX();

			// Return one for zero pow
			if (power == 0) return 1;

      if (power == 1) return value;

			// Get first one bit
			int msb = Bits.Msb(power);

			// Get multiplier
			IMultiplier multiplier = MultiplyManager.GetMultiplier(multiplyMode);

			// Do actual raising
			IntX res = value;
			for (uint powerMask = 1U << (msb - 1); powerMask != 0; powerMask >>= 1)
			{
				// Always square
				res = multiplier.Multiply(res, res);

				// Maybe mul
				if ((power & powerMask) != 0)
				{
					res = multiplier.Multiply(res, value);
				}
			}
			return res;
		}

		#endregion Power operation

		#region Compare operation

		/// <summary>
		/// Compares 2 <see cref="IntX" /> objects.
		/// Returns "-2" if any argument is null, "-1" if <paramref name="int1" /> &lt; <paramref name="int2" />,
		/// "0" if equal and "1" if &gt;.
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second big integer.</param>
		/// <param name="throwNullException">Raises or not <see cref="NullReferenceException" />.</param>
		/// <returns>Comparsion result.</returns>
		/// <exception cref="ArgumentNullException"><paramref name="int1" /> or <paramref name="int2" /> is a null reference and <paramref name="throwNullException" /> is set to true.</exception>
		static public int Cmp(IntX int1, IntX int2, bool throwNullException)
		{
			// If one of the operands is null, throw exception or return -2
			bool isNull1 = ReferenceEquals(int1, null);
			bool isNull2 = ReferenceEquals(int2, null);
			if (isNull1 || isNull2)
			{
				if (throwNullException)
				{
					throw new ArgumentNullException(isNull1 ? "int1" : "int2", Strings.CantBeNullCmp);
				}
				else
				{
					return isNull1 && isNull2 ? 0 : -2;
				}
			}

			// Compare sign
			if (int1._negative && !int2._negative) return -1;
			if (!int1._negative && int2._negative) return 1;

			// Compare presentation
			return DigitOpHelper.Cmp(int1._digits, int1._length, int2._digits, int2._length) * (int1._negative ? -1 : 1);
		}

		/// <summary>
		/// Compares <see cref="IntX" /> object to int.
		/// Returns "-1" if <paramref name="int1" /> &lt; <paramref name="int2" />, "0" if equal and "1" if &gt;.
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second integer.</param>
		/// <returns>Comparsion result.</returns>
		static public int Cmp(IntX int1, int int2)
		{
			// Special processing for zero
			if (int2 == 0) return int1._length == 0 ? 0 : (int1._negative ? -1 : 1);

			// Compare presentation
      if (int1._length == 0) return 0.CompareTo(int2);
			if (int1._length > 1) return int1._negative ? -1 : 1;
			uint digit2;
			bool negative2;
			DigitHelper.ToUInt32WithSign(int2, out digit2, out negative2);

			// Compare sign
			if (int1._negative && !negative2) return -1;
			if (!int1._negative && negative2) return 1;

			return int1._digits[0] == digit2 ? 0 : (int1._digits[0] < digit2 ^ negative2 ? -1 : 1);
		}

		/// <summary>
		/// Compares <see cref="IntX" /> object to unsigned int.
		/// Returns "-1" if <paramref name="int1" /> &lt; <paramref name="int2" />, "0" if equal and "1" if &gt;.
		/// For internal use.
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second unsigned integer.</param>
		/// <returns>Comparsion result.</returns>
		static public int Cmp(IntX int1, uint int2)
		{
			// Special processing for zero
			if (int2 == 0) return int1._length == 0 ? 0 : (int1._negative ? -1 : 1);

			// Compare presentation
			if (int1._negative) return -1;
      if (int1._length == 0) return 0.CompareTo(int2);
			if (int1._length > 1) return 1;
			return int1._digits[0] == int2 ? 0 : (int1._digits[0] < int2 ? -1 : 1);
		}

		#endregion Compare operation

		#region Shift operation

		/// <summary>
		/// Shifts <see cref="IntX" /> object.
		/// Determines which operation to use basing on shift sign.
		/// </summary>
		/// <param name="intX">Big integer.</param>
		/// <param name="shift">Bits count to shift.</param>
		/// <param name="toLeft">If true the shifting to the left.</param>
		/// <returns>Bitwise shift operation result.</returns>
		/// <exception cref="ArgumentNullException"><paramref name="intX" /> is a null reference.</exception>
		static public IntX Sh(IntX intX, long shift, bool toLeft)
		{
			// Exceptions
			if (ReferenceEquals(intX, null))
			{
				throw new ArgumentNullException("intX", Strings.CantBeNullOne);
			}

			// Zero can't be shifted
			if (intX._length == 0) return new IntX();

			// Can't shift on zero value
			if (shift == 0) return new IntX(intX);

			// Determine real bits count and direction
			ulong bitCount;
			bool negativeShift;
			DigitHelper.ToUInt64WithSign(shift, out bitCount, out negativeShift);
			toLeft ^= negativeShift;

			// Get position of the most significant bit in intX and amount of bits in intX
			int msb = Bits.Msb(intX._digits[intX._length - 1]);
			ulong intXBitCount = (ulong)(intX._length - 1) * Constants.DigitBitCount + (ulong)msb + 1UL;

			// If shifting to the right and shift is too big then return zero
			if (!toLeft && bitCount >= intXBitCount) return new IntX();

			// Calculate new bit count
			ulong newBitCount = toLeft ? intXBitCount + bitCount : intXBitCount - bitCount;

			// If shifting to the left and shift is too big to fit in big integer, throw an exception
			if (toLeft && newBitCount > Constants.MaxBitCount)
			{
				throw new ArgumentException(Strings.IntegerTooBig, "intX");
			}

			// Get exact length of new big integer (no normalize is ever needed here).
			// Create new big integer with given length
			uint newLength = (uint)(newBitCount / Constants.DigitBitCount + (newBitCount % Constants.DigitBitCount == 0 ? 0UL : 1UL));
			IntX newInt = new IntX(newLength, intX._negative);

			// Get full and small shift values
			uint fullDigits = (uint)(bitCount / Constants.DigitBitCount);
			int smallShift = (int)(bitCount % Constants.DigitBitCount);

			// We can just copy (no shift) if small shift is zero
			if (smallShift == 0)
			{
				if (toLeft)
				{
					Array.Copy(intX._digits, 0, newInt._digits, fullDigits, intX._length);
				}
				else
				{
					Array.Copy(intX._digits, fullDigits, newInt._digits, 0, newLength);
				}
			}
			else
			{
				// Do copy with real shift in the needed direction
				if (toLeft)
				{
					DigitOpHelper.Shr(intX._digits, 0, intX._length, newInt._digits, fullDigits + 1, Constants.DigitBitCount - smallShift);
				}
				else
				{
          uint length = newLength < intX._length ? newLength + 1 : newLength;
          DigitOpHelper.Shr(intX._digits, fullDigits, length, newInt._digits, 0, smallShift);
				}
			}

			return newInt;
		}

		#endregion Shift operation
	}
}
