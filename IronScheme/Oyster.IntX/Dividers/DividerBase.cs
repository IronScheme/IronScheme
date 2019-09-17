using System;

namespace Oyster.Math
{
	/// <summary>
	/// Base class for dividers.
	/// Contains default implementation of divide operation over <see cref="IntX" /> instances.
	/// </summary>
	abstract internal class DividerBase : IDivider
	{
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
		virtual public IntX DivMod(IntX int1, IntX int2, out IntX modRes, DivModResultFlags resultFlags)
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
			uint divLength = DivMod(
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
				//divRes.TryNormalize();
			}
			if (modNeeded)
			{
				modRes._length = modLength;
				//modRes.TryNormalize();
			}

			// Return div
			return divRes;
		}

		/// <summary>
		/// Divides two big integers.
		/// Also modifies <paramref name="digits1" /> and <paramref name="length1"/> (it will contain remainder).
		/// </summary>
		/// <param name="digits1">First big integer digits.</param>
		/// <param name="digitsBuffer1">Buffer for first big integer digits. May also contain remainder. Can be null - in this case it's created if necessary.</param>
		/// <param name="length1">First big integer length.</param>
		/// <param name="digits2">Second big integer digits.</param>
		/// <param name="digitsBuffer2">Buffer for second big integer digits. Only temporarily used. Can be null - in this case it's created if necessary.</param>
		/// <param name="length2">Second big integer length.</param>
		/// <param name="digitsRes">Resulting big integer digits.</param>
		/// <param name="resultFlags">Which operation results to return.</param>
		/// <param name="cmpResult">Big integers comparsion result (pass -2 if omitted).</param>
		/// <returns>Resulting big integer length.</returns>
		abstract public uint DivMod(
			uint[] digits1,
			uint[] digitsBuffer1,
			ref uint length1,
			uint[] digits2,
			uint[] digitsBuffer2,
			uint length2,
			uint[] digitsRes,
			DivModResultFlags resultFlags,
			int cmpResult);

		/// <summary>
		/// Divides two big integers.
		/// Also modifies <paramref name="digitsPtr1" /> and <paramref name="length1"/> (it will contain remainder).
		/// </summary>
		/// <param name="digitsPtr1">First big integer digits.</param>
		/// <param name="digitsBufferPtr1">Buffer for first big integer digits. May also contain remainder.</param>
		/// <param name="length1">First big integer length.</param>
		/// <param name="digitsPtr2">Second big integer digits.</param>
		/// <param name="digitsBufferPtr2">Buffer for second big integer digits. Only temporarily used.</param>
		/// <param name="length2">Second big integer length.</param>
		/// <param name="digitsResPtr">Resulting big integer digits.</param>
		/// <param name="resultFlags">Which operation results to return.</param>
		/// <param name="cmpResult">Big integers comparsion result (pass -2 if omitted).</param>
		/// <returns>Resulting big integer length.</returns>
		virtual unsafe public uint DivMod(
			uint* digitsPtr1,
			uint* digitsBufferPtr1,
			ref uint length1,
			uint* digitsPtr2,
			uint* digitsBufferPtr2,
			uint length2,
			uint* digitsResPtr,
			DivModResultFlags resultFlags,
			int cmpResult)
		{
			// Base implementation covers some special cases

			bool divNeeded = (resultFlags & DivModResultFlags.Div) != 0;
			bool modNeeded = (resultFlags & DivModResultFlags.Mod) != 0;

			//
			// Special cases
			//

			// Case when length1 == 0
			if (length1 == 0) return 0;

			// Case when both lengths are 1
			if (length1 == 1 && length2 == 1)
			{
				if (divNeeded)
				{
					*digitsResPtr = *digitsPtr1 / *digitsPtr2;
					if (*digitsResPtr == 0)
					{
						length2 = 0;
					}
				}
				if (modNeeded)
				{
					*digitsBufferPtr1 = *digitsPtr1 % *digitsPtr2;
					if (*digitsBufferPtr1 == 0)
					{
						length1 = 0;
					}
				}

				return length2;
			}

			// Compare digits first (if was not previously compared)
			if (cmpResult == -2)
			{
				cmpResult = DigitOpHelper.Cmp(digitsPtr1, length1, digitsPtr2, length2);
			}

			// Case when first value is smaller then the second one - we will have remainder only
			if (cmpResult < 0)
			{
				// Maybe we should copy first digits into remainder (if remainder is needed at all)
				if (modNeeded)
				{
					DigitHelper.DigitsBlockCopy(digitsPtr1, digitsBufferPtr1, length1);
				}

				// Zero as division result
				return 0;
			}

			// Case when values are equal
			if (cmpResult == 0)
			{
				// Maybe remainder must be marked as empty
				if (modNeeded)
				{
					length1 = 0;
				}

				// One as division result
				if (divNeeded)
				{
					*digitsResPtr = 1;
				}

				return 1;
			}

			// Case when second length equals to 1
			if (length2 == 1)
			{
				// Call method basing on fact if div is needed
				uint modRes;
				if (divNeeded)
				{
					length2 = DigitOpHelper.DivMod(digitsPtr1, length1, *digitsPtr2, digitsResPtr, out modRes);
				}
				else
				{
					modRes = DigitOpHelper.Mod(digitsPtr1, length1, *digitsPtr2);
				}

				// Maybe save mod result
				if (modNeeded)
				{
					if (modRes != 0)
					{
						length1 = 1;
						*digitsBufferPtr1 = modRes;
					}
					else
					{
						length1 = 0;
					}
				}

				return length2;
			}


			// This is regular case, not special
			return uint.MaxValue;
		}
	}
}
