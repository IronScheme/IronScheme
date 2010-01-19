namespace Oyster.Math
{
	/// <summary>
	/// Divider using "classic" algorithm.
	/// </summary>
	sealed internal class ClassicDivider : DividerBase
	{
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
		override unsafe public uint DivMod(
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
		override unsafe public uint DivMod(
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
			// Call base (for special cases)
			uint resultLength = base.DivMod(
				digitsPtr1,
				digitsBufferPtr1,
				ref length1,
				digitsPtr2,
				digitsBufferPtr2,
				length2,
				digitsResPtr,
				resultFlags,
				cmpResult);
			if (resultLength != uint.MaxValue) return resultLength;

			bool divNeeded = (resultFlags & DivModResultFlags.Div) != 0;
			bool modNeeded = (resultFlags & DivModResultFlags.Mod) != 0;

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
				length1 = DigitOpHelper.Shr(digitsPtr1, length1, digitsBufferPtr1 + 1, rightShift1, true) + 1U;

				// Second digits also must be shifted
				DigitOpHelper.Shr(digitsPtr2, length2, digitsBufferPtr2 + 1, rightShift1, true);
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
					length1 = DigitOpHelper.Shr(digitsBufferPtr1, length1, digitsBufferPtr1, shift1, false);
				}
			}

			// Finally return length
			return !divNeeded ? 0 : (digitsResPtr[maxLength] == 0 ? maxLength : ++maxLength);
		}
	}
}
