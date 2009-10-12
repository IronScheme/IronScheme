namespace Oyster.Math
{
	/// <summary>
	/// Contains helping methods for operations over <see cref="IntX" /> digits as arrays.
	/// </summary>
	static internal class DigitOpHelper
	{
		#region Add operation

		/// <summary>
		/// Adds two big integers.
		/// </summary>
		/// <param name="digits1">First big integer digits.</param>
		/// <param name="length1">First big integer length.</param>
		/// <param name="digits2">Second big integer digits.</param>
		/// <param name="length2">Second big integer length.</param>
		/// <param name="digitsRes">Resulting big integer digits.</param>
		/// <returns>Resulting big integer length.</returns>
		static unsafe public uint Add(uint[] digits1, uint length1, uint[] digits2, uint length2, uint[] digitsRes)
		{
			fixed (uint* digitsPtr1 = digits1, digitsPtr2 = digits2, digitsResPtr = digitsRes)
			{
				return Add(digitsPtr1, length1, digitsPtr2, length2, digitsResPtr);
			}
		}

		/// <summary>
		/// Adds two big integers using pointers.
		/// </summary>
		/// <param name="digitsPtr1">First big integer digits.</param>
		/// <param name="length1">First big integer length.</param>
		/// <param name="digitsPtr2">Second big integer digits.</param>
		/// <param name="length2">Second big integer length.</param>
		/// <param name="digitsResPtr">Resulting big integer digits.</param>
		/// <returns>Resulting big integer length.</returns>
		static unsafe public uint Add(uint* digitsPtr1, uint length1, uint* digitsPtr2, uint length2, uint* digitsResPtr)
		{
			ulong c = 0;

			if (length1 < length2)
			{
				// First must be bigger - swap
				uint lengthTemp = length1;
				length1 = length2;
				length2 = lengthTemp;

				uint* ptrTemp = digitsPtr1;
				digitsPtr1 = digitsPtr2;
				digitsPtr2 = ptrTemp;
			}

			// Perform digits adding
			for (uint i = 0; i < length2; ++i)
			{
				c += (ulong)digitsPtr1[i] + digitsPtr2[i];
				digitsResPtr[i] = (uint)c;
				c >>= 32;
			}

			// Perform digits + carry moving
			for (uint i = length2; i < length1; ++i)
			{
				c += digitsPtr1[i];
				digitsResPtr[i] = (uint)c;
				c >>= 32;
			}

			// Account last carry
			if (c != 0)
			{
				digitsResPtr[length1++] = (uint)c;
			}

			return length1;
		}

		#endregion Add operation

		#region Subtract operation

		/// <summary>
		/// Subtracts two big integers.
		/// </summary>
		/// <param name="digits1">First big integer digits.</param>
		/// <param name="length1">First big integer length.</param>
		/// <param name="digits2">Second big integer digits.</param>
		/// <param name="length2">Second big integer length.</param>
		/// <param name="digitsRes">Resulting big integer digits.</param>
		/// <returns>Resulting big integer length.</returns>
		static public uint Sub(uint[] digits1, uint length1, uint[] digits2, uint length2, uint[] digitsRes)
		{
			ulong c = 0;

			// Perform digits subtraction
			for (uint i = 0; i < length2; ++i)
			{
				c = (ulong)digits1[i] - digits2[i] - c;
				digitsRes[i] = (uint)c;
				c >>= 63;
			}

			// Perform digits + carry moving
			for (uint i = length2; i < length1; ++i)
			{
				c = digits1[i] - c;
				digitsRes[i] = (uint)c;
				c >>= 63;
			}

			return DigitHelper.GetRealDigitsLength(digitsRes, length1);
		}

		#endregion Subtract operation

		#region Divide/modulo operation

		/// <summary>
		/// Divides two big integers (using Knuth algorithm).
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
		static unsafe public uint DivMod(
			uint[] digits1,
			uint[] digitsBuffer1,
			ref uint length1,
			uint[] digits2,
			uint[] digitsBuffer2,
			uint length2,
			uint[] digitsRes,
			DivModResultFlags resultFlags,
			int cmpResult)
		{
			// Create some buffers if necessary
			if (digitsBuffer1 == null)
			{
				digitsBuffer1 = new uint[length1 + 1];
			}
			if (digitsBuffer2 == null)
			{
				digitsBuffer2 = new uint[length2];
			}

			fixed (uint* digitsPtr1 = digits1, digitsBufferPtr1 = digitsBuffer1, digitsPtr2 = digits2, digitsBufferPtr2 = digitsBuffer2, digitsResPtr = digitsRes != null ? digitsRes : digits1)
			{
				return DivMod(
					digitsPtr1,
					digitsBufferPtr1,
					ref length1,
					digitsPtr2,
					digitsBufferPtr2,
					length2,
					digitsResPtr == digitsPtr1 ? null : digitsResPtr,
					resultFlags,
					cmpResult);
			}
		}

		/// <summary>
		/// Divides two big integers (using Knuth algorithm).
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
		static unsafe public uint DivMod(
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
				cmpResult = Cmp(digitsPtr1, length1, digitsPtr2, length2);
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
					length2 = DivMod(digitsPtr1, length1, *digitsPtr2, digitsResPtr, out modRes);
				}
				else
				{
					modRes = Mod(digitsPtr1, length1, *digitsPtr2);
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

			//
			// Prepare digitsBufferPtr1 and digitsBufferPtr2
			//

			int shift1 = 31 - Bits.Msb(digitsPtr2[length2 - 1]);
			if (shift1 == 0)
			{
				// We don't need to shift - just copy
				DigitHelper.DigitsBlockCopy(digitsPtr1, digitsBufferPtr1, length1);

				// We also don't need to shift second digits
				digitsBufferPtr2 = digitsPtr2;
			}
			else
			{
				int rightShift1 = Constants.DigitBitCount - shift1;

				// We do need to shift here - so copy with shift - suppose we have enough storage for this operation
				length1 = Shr(digitsPtr1, length1, digitsBufferPtr1 + 1, rightShift1, true) + 1U;

				// Second digits also must be shifted
				Shr(digitsPtr2, length2, digitsBufferPtr2 + 1, rightShift1, true);
			}

			//
			// Division main algorithm implementation
			//

			ulong longDigit;
			ulong divEst;
			ulong modEst;

			ulong mulRes;
			uint divRes;
			long k, t;

			// Some digits2 cached digits
			uint lastDigit2 = digitsBufferPtr2[length2 - 1];
			uint preLastDigit2 = digitsBufferPtr2[length2 - 2];

			// Main divide loop
			bool isMaxLength;
			uint maxLength = length1 - length2;
			for (uint i = maxLength, iLen2 = length1, j, ji; i <= maxLength; --i, --iLen2)
			{
				isMaxLength = iLen2 == length1;

				// Calculate estimates
				if (isMaxLength)
				{
					longDigit = digitsBufferPtr1[iLen2 - 1];
				}
				else
				{
					longDigit = (ulong)digitsBufferPtr1[iLen2] << Constants.DigitBitCount | digitsBufferPtr1[iLen2 - 1];
				}

				divEst = longDigit / lastDigit2;
				modEst = longDigit - divEst * lastDigit2;

				// Check estimate (maybe correct it)
				for (;;)
				{
					if (divEst == Constants.BitCountStepOf2 || divEst * preLastDigit2 > (modEst << Constants.DigitBitCount) + digitsBufferPtr1[iLen2 - 2])
					{
						--divEst;
						modEst += lastDigit2;
						if (modEst < Constants.BitCountStepOf2) continue;
					}
					break;
				}
				divRes = (uint)divEst;

				// Multiply and subtract
				k = 0;
				for (j = 0, ji = i; j < length2; ++j, ++ji)
				{
					mulRes = (ulong)divRes * digitsBufferPtr2[j];
					t = digitsBufferPtr1[ji] - k - (long)(mulRes & 0xFFFFFFFF);
					digitsBufferPtr1[ji] = (uint)t;
					k = (long)(mulRes >> Constants.DigitBitCount) - (t >> Constants.DigitBitCount);
				}

				if (!isMaxLength)
				{
					t = digitsBufferPtr1[iLen2] - k;
					digitsBufferPtr1[iLen2] = (uint)t;
				}
				else
				{
					t = -k;
				}

				// Correct result if subtracted too much
				if (t < 0)
				{
					--divRes;

					k = 0;
					for (j = 0, ji = i; j < length2; ++j, ++ji)
					{
						t = (long)digitsBufferPtr1[ji] + digitsBufferPtr2[j] + k;
						digitsBufferPtr1[ji] = (uint)t;
						k = t >> Constants.DigitBitCount;
					}

					if (!isMaxLength)
					{
						digitsBufferPtr1[iLen2] = (uint)(k + digitsBufferPtr1[iLen2]);
					}
				}

				// Maybe save div result
				if (divNeeded)
				{
					digitsResPtr[i] = divRes;
				}
			}

			if (modNeeded)
			{
				// First set correct mod length
				length1 = DigitHelper.GetRealDigitsLength(digitsBufferPtr1, length2);

				// Next maybe shift result back to the right
				if (shift1 != 0 && length1 != 0)
				{
					length1 = Shr(digitsBufferPtr1, length1, digitsBufferPtr1, shift1, false);
				}
			}

			// Finally return length
			return !divNeeded ? 0 : (digitsResPtr[maxLength] == 0 ? maxLength : ++maxLength);
		}

		#endregion Divide/modulo operation

		#region Divide/modulo operation - when second length == 1

		/// <summary>
		/// Divides one big integer represented by it's digits on another one big ingeter.
		/// Reminder is always filled (but not the result).
		/// </summary>
		/// <param name="digits1">First big integer digits.</param>
		/// <param name="length1">First big integer length.</param>
		/// <param name="int2">Second integer.</param>
		/// <param name="divRes">Div result (can be null - not filled in this case).</param>
		/// <param name="modRes">Remainder (always filled).</param>
		/// <returns>Result length (0 if result is null).</returns>
		static unsafe public uint DivMod(uint[] digits1, uint length1, uint int2, uint[] divRes, out uint modRes)
		{
			fixed (uint* digits1Ptr = digits1, divResPtr = divRes)
			{
				return DivMod(digits1Ptr, length1, int2, divResPtr, out modRes);
			}
		}

		/// <summary>
		/// Divides one big integer represented by it's digits on another one big ingeter.
		/// Reminder is always filled (but not the result).
		/// </summary>
		/// <param name="digitsPtr1">First big integer digits.</param>
		/// <param name="length1">First big integer length.</param>
		/// <param name="int2">Second integer.</param>
		/// <param name="divResPtr">Div result (can be null - not filled in this case).</param>
		/// <param name="modRes">Remainder (always filled).</param>
		/// <returns>Result length (0 if result is null).</returns>
		static unsafe public uint DivMod(uint* digitsPtr1, uint length1, uint int2, uint* divResPtr, out uint modRes)
		{
			ulong c = 0;
			uint res;
			for (uint index = length1 - 1; index < length1; --index)
			{
				c = (c << Constants.DigitBitCount) + digitsPtr1[index];
				res = (uint)(c / int2);
				c -= (ulong)res * int2;

				divResPtr[index] = res;
			}
			modRes = (uint)c;

			return length1 - (divResPtr[length1 - 1] == 0 ? 1U : 0U);
		}


		/// <summary>
		/// Divides one big integer represented by it's digits on another one big ingeter.
		/// Only remainder is filled.
		/// </summary>
		/// <param name="digits1">First big integer digits.</param>
		/// <param name="length1">First big integer length.</param>
		/// <param name="int2">Second integer.</param>
		/// <returns>Remainder.</returns>
		static unsafe public uint Mod(uint[] digits1, uint length1, uint int2)
		{
			fixed (uint* digitsPtr1 = digits1)
			{
				return Mod(digitsPtr1, length1, int2);
			}
		}

		/// <summary>
		/// Divides one big integer represented by it's digits on another one big ingeter.
		/// Only remainder is filled.
		/// </summary>
		/// <param name="digitsPtr1">First big integer digits.</param>
		/// <param name="length1">First big integer length.</param>
		/// <param name="int2">Second integer.</param>
		/// <returns>Remainder.</returns>
		static unsafe public uint Mod(uint* digitsPtr1, uint length1, uint int2)
		{
			ulong c = 0;
			uint res;
			for (uint* ptr1 = digitsPtr1 + length1 - 1; ptr1 >= digitsPtr1; --ptr1)
			{
				c = (c << Constants.DigitBitCount) + *ptr1;
				res = (uint)(c / int2);
				c -= (ulong)res * int2;
			}

			return (uint)c;
		}

		#endregion Divide/modulo operation - when second length == 1

		#region Compare operation

		/// <summary>
		/// Compares 2 <see cref="IntX" /> objects represented by digits only (not taking sign into account).
		/// Returns "-1" if <paramref name="digits1" /> &lt; <paramref name="digits2" />, "0" if equal and "1" if &gt;.
		/// </summary>
		/// <param name="digits1">First big integer digits.</param>
		/// <param name="length1">First big integer length.</param>
		/// <param name="digits2">Second big integer digits.</param>
		/// <param name="length2">Second big integer length.</param>
		/// <returns>Comparsion result.</returns>
		static unsafe public int Cmp(uint[] digits1, uint length1, uint[] digits2, uint length2)
		{
			// Always compare length if one of the integers has zero length
			if (length1 == 0 || length2 == 0) return CmpLen(length1, length2);

			fixed (uint* digitsPtr1 = digits1, digitsPtr2 = digits2)
			{
				return Cmp(digitsPtr1, length1, digitsPtr2, length2);
			}
		}

		/// <summary>
		/// Compares 2 <see cref="IntX" /> objects represented by pointers only (not taking sign into account).
		/// Returns "-1" if <paramref name="digitsPtr1" /> &lt; <paramref name="digitsPtr2" />, "0" if equal and "1" if &gt;.
		/// </summary>
		/// <param name="digitsPtr1">First big integer digits.</param>
		/// <param name="length1">First big integer length.</param>
		/// <param name="digitsPtr2">Second big integer digits.</param>
		/// <param name="length2">Second big integer length.</param>
		/// <returns>Comparsion result.</returns>
		static unsafe public int Cmp(uint* digitsPtr1, uint length1, uint* digitsPtr2, uint length2)
		{
			// Maybe length comparing will be enough
			int res = CmpLen(length1, length2);
			if (res != -2) return res;

			for (uint index = length1 - 1; index < length1; --index)
			{
				if (digitsPtr1[index] != digitsPtr2[index]) return digitsPtr1[index] < digitsPtr2[index] ? -1 : 1;
			}
			return 0;
		}

		/// <summary>
		/// Compares two integers lengths. Returns -2 if further comparing is needed.
		/// </summary>
		/// <param name="length1">First big integer length.</param>
		/// <param name="length2">Second big integer length.</param>
		/// <returns>Comparsion result.</returns>
		static int CmpLen(uint length1, uint length2)
		{
			if (length1 < length2) return -1;
			if (length1 > length2) return 1;
			return length1 == 0 ? 0 : -2;
		}

		#endregion Compare operation

		#region Shift operation

		/// <summary>
		/// Shifts big integer.
		/// </summary>
		/// <param name="digits">Big integer digits.</param>
		/// <param name="offset">Big integer digits offset.</param>
		/// <param name="length">Big integer length.</param>
		/// <param name="digitsRes">Resulting big integer digits.</param>
		/// <param name="resOffset">Resulting big integer digits offset.</param>
		/// <param name="rightShift">Shift to the right (always between 1 an 31).</param>
		static unsafe public void Shr(
			uint[] digits,
			uint offset,
			uint length,
			uint[] digitsRes,
			uint resOffset,
			int rightShift)
		{
			fixed (uint* digitsPtr = digits, digitsResPtr = digitsRes)
			{
				Shr(digitsPtr + offset, length, digitsResPtr + resOffset, rightShift, resOffset != 0);
			}
		}

		/// <summary>
		/// Shifts big integer.
		/// </summary>
		/// <param name="digitsPtr">Big integer digits.</param>
		/// <param name="length">Big integer length.</param>
		/// <param name="digitsResPtr">Resulting big integer digits.</param>
		/// <param name="rightShift">Shift to the right (always between 1 an 31).</param>
		/// <param name="resHasOffset">True if <paramref name="digitsResPtr" /> has offset.</param>
		/// <returns>Resulting big integer length.</returns>
		static unsafe public uint Shr(uint* digitsPtr, uint length, uint* digitsResPtr, int rightShift, bool resHasOffset)
		{
			int rightShiftRev = Constants.DigitBitCount - rightShift;

			// Shift first digit in special way
			if (resHasOffset)
			{
				digitsResPtr[-1] = digitsPtr[0] << rightShiftRev;
			}

			// Shift all digits except last one
			uint* digitsPtrEndPrev = digitsPtr + length - 1;
			uint* digitsPtrNext = digitsPtr + 1;
			for (; digitsPtr < digitsPtrEndPrev; ++digitsPtr, ++digitsPtrNext, ++digitsResPtr)
			{
				*digitsResPtr = *digitsPtr >> rightShift | *digitsPtrNext << rightShiftRev;
			}

			// Shift last digit in special way
			uint lastValue = *digitsPtr >> rightShift;
			if (lastValue != 0)
			{
				*digitsResPtr = lastValue;
				return length;
			}
			return length - 1;
		}

		#endregion Shift operation
	}
}
