using System;

namespace Oyster.Math
{
	/// <summary>
	/// Base class for multipliers.
	/// Contains default implementation of multiply operation over <see cref="IntX" /> instances.
	/// </summary>
	abstract internal class MultiplierBase : IMultiplier
	{
		/// <summary>
		/// Multiplies two big integers.
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second big integer.</param>
		/// <returns>Resulting big integer.</returns>
		/// <exception cref="ArgumentNullException"><paramref name="int1" /> or <paramref name="int2" /> is a null reference.</exception>
		/// <exception cref="ArgumentException"><paramref name="int1" /> or <paramref name="int2" /> is too big for multiply operation.</exception>
		virtual public IntX Multiply(IntX int1, IntX int2)
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

			// Special behavior for zero cases
			if (int1._length == 0 || int2._length == 0) return new IntX();

			// Get new big integer length and check it
			ulong newLength = (ulong)int1._length + int2._length;
			if (newLength >> 32 != 0)
			{
				throw new ArgumentException(Strings.IntegerTooBig);
			}

			// Create resulting big int
			IntX newInt = new IntX((uint)newLength, int1._negative ^ int2._negative);

			// Perform actual digits multiplication
			newInt._length = Multiply(int1._digits, int1._length, int2._digits, int2._length, newInt._digits);

			// Normalization may be needed
			//newInt.TryNormalize();

			return newInt;
		}

		/// <summary>
		/// Multiplies two big integers represented by their digits.
		/// </summary>
		/// <param name="digits1">First big integer digits.</param>
		/// <param name="length1">First big integer real length.</param>
		/// <param name="digits2">Second big integer digits.</param>
		/// <param name="length2">Second big integer real length.</param>
		/// <param name="digitsRes">Where to put resulting big integer.</param>
		/// <returns>Resulting big integer real length.</returns>
		virtual unsafe public uint Multiply(uint[] digits1, uint length1, uint[] digits2, uint length2, uint[] digitsRes)
		{
			fixed (uint* digitsPtr1 = digits1, digitsPtr2 = digits2, digitsResPtr = digitsRes)
			{
				return Multiply(digitsPtr1, length1, digitsPtr2, length2, digitsResPtr);
			}
		}

		/// <summary>
		/// Multiplies two big integers using pointers.
		/// </summary>
		/// <param name="digitsPtr1">First big integer digits.</param>
		/// <param name="length1">First big integer length.</param>
		/// <param name="digitsPtr2">Second big integer digits.</param>
		/// <param name="length2">Second big integer length.</param>
		/// <param name="digitsResPtr">Resulting big integer digits.</param>
		/// <returns>Resulting big integer length.</returns>
		abstract unsafe public uint Multiply(uint* digitsPtr1, uint length1, uint* digitsPtr2, uint length2, uint* digitsResPtr);
	}
}
