namespace Oyster.Math
{
	/// <summary>
	/// Contains helping methods for fast division
	/// using Newton approximation approach and fast multiplication.
	/// </summary>
	static internal class NewtonHelper
	{
		/// <summary>
		/// Generates integer opposite to the given one using approximation.
		/// Uses algorithm from Khuth vol. 2 3rd Edition (4.3.3).
		/// </summary>
		/// <param name="digitsPtr">Initial big integer digits.</param>
		/// <param name="length">Initial big integer length.</param>
		/// <param name="maxLength">Precision length.</param>
		/// <param name="bufferPtr">Buffer in which shifted big integer may be stored.</param>
		/// <param name="newLength">Resulting big integer length.</param>
		/// <param name="rightShift">How much resulting big integer is shifted to the left (or: must be shifted to the right).</param>
		/// <returns>Resulting big integer digits.</returns>
		static unsafe public uint[] GetIntegerOpposite(
			uint* digitsPtr,
			uint length,
			uint maxLength,
			uint* bufferPtr,
			out uint newLength,
			out ulong rightShift)
		{
			// Maybe initially shift original digits a bit to the left
			// (it must have MSB on 2nd position in the highest digit)
			int msb = Bits.Msb(digitsPtr[length - 1]);
			rightShift = (ulong)(length - 1) * Constants.DigitBitCount + (ulong)msb + 1U;

			if (msb != 2)
			{
				// Shift to the left (via actually right shift)
				int leftShift = (2 - msb + Constants.DigitBitCount) % Constants.DigitBitCount;
				length = DigitOpHelper.Shr(digitsPtr, length, bufferPtr + 1, Constants.DigitBitCount - leftShift, true) + 1U;
			}
			else
			{
				// Simply use the same digits without any shifting
				bufferPtr = digitsPtr;
			}

			// Calculate possible result length
			int lengthLog2 = Bits.CeilLog2(maxLength);
			uint newLengthMax = 1U << (lengthLog2 + 1);
			int lengthLog2Bits = lengthLog2 + Bits.Msb(Constants.DigitBitCount);

			// Create result digits
			uint[] resultDigits = ArrayPool<uint>.Instance.GetArray(newLengthMax); //new uint[newLengthMax];
			uint resultLength;

			// Create temporary digits for squared result (twice more size)
			uint[] resultDigitsSqr = ArrayPool<uint>.Instance.GetArray(newLengthMax); //new uint[newLengthMax];
			uint resultLengthSqr;

			// Create temporary digits for squared result * buffer
			uint[] resultDigitsSqrBuf = new uint[newLengthMax + length];
			uint resultLengthSqrBuf;

			// We will always use current multiplier
			IMultiplier multiplier = MultiplyManager.GetCurrentMultiplier();

			// Fix some digits
			fixed (uint* resultPtrFixed = resultDigits, resultSqrPtrFixed = resultDigitsSqr, resultSqrBufPtr = resultDigitsSqrBuf)
			{
				uint* resultPtr = resultPtrFixed;
				uint* resultSqrPtr = resultSqrPtrFixed;

				// Cache two first digits
				uint bufferDigitN1 = bufferPtr[length - 1];
				uint bufferDigitN2 = bufferPtr[length - 2];

				// Prepare result.
				// Initially result = floor(32 / (4*v1 + 2*v2 + v3)) / 4
				// (last division is not floored - here we emulate fixed point)
				resultDigits[0] = 32 / bufferDigitN1;
				resultLength = 1;

				// Prepare variables
				uint nextBufferTempStorage = 0;
				int nextBufferTempShift;
				uint nextBufferLength = 1U;
				uint* nextBufferPtr = &nextBufferTempStorage;

				ulong bitsAfterDotResult;
				ulong bitsAfterDotResultSqr;
				ulong bitsAfterDotNextBuffer;
				ulong bitShift;
				uint shiftOffset;

				uint* tempPtr;
				uint[] tempDigits;

				// Iterate 'till result will be precise enough
				for (int k = 0; k < lengthLog2Bits; ++k)
				{
					// Get result squared
					resultLengthSqr = multiplier.Multiply(
						resultPtr,
						resultLength,
						resultPtr,
						resultLength,
						resultSqrPtr);

					// Calculate current result bits after dot
					bitsAfterDotResult = (1UL << k) + 1UL;
					bitsAfterDotResultSqr = bitsAfterDotResult << 1;

					// Here we will get the next portion of data from bufferPtr
					if (k < 4)
					{
						// For now buffer intermediate has length 1 and we will use this fact
						nextBufferTempShift = 1 << (k + 1);
						nextBufferTempStorage =
							bufferDigitN1 << nextBufferTempShift |
							bufferDigitN2 >> (Constants.DigitBitCount - nextBufferTempShift);

						// Calculate amount of bits after dot (simple formula here)
						bitsAfterDotNextBuffer = (ulong)nextBufferTempShift + 3UL;
					}
					else
					{
						// Determine length to get from bufferPtr
						nextBufferLength = System.Math.Min((1U << (k - 4)) + 1U, length);
						nextBufferPtr = bufferPtr + (length - nextBufferLength);

						// Calculate amount of bits after dot (simple formula here)
						bitsAfterDotNextBuffer = (ulong)(nextBufferLength - 1U) * Constants.DigitBitCount + 3UL;
					}

					// Multiply result ^ 2 and nextBuffer + calculate new amount of bits after dot
					resultLengthSqrBuf = multiplier.Multiply(
						resultSqrPtr,
						resultLengthSqr,
						nextBufferPtr,
						nextBufferLength,
						resultSqrBufPtr);

					bitsAfterDotNextBuffer += bitsAfterDotResultSqr;

					// Now calculate 2 * result - resultSqrBufPtr
					--bitsAfterDotResult;
					--bitsAfterDotResultSqr;

					// Shift result on a needed amount of bits to the left
					bitShift = bitsAfterDotResultSqr - bitsAfterDotResult;
					shiftOffset = (uint)(bitShift / Constants.DigitBitCount);
					resultLength =
						shiftOffset + 1U +
						DigitOpHelper.Shr(
							resultPtr,
							resultLength,
							resultSqrPtr + shiftOffset + 1U,
							Constants.DigitBitCount - (int)(bitShift % Constants.DigitBitCount),
							true);

					// Swap resultPtr and resultSqrPtr pointers
					tempPtr = resultPtr;
					resultPtr = resultSqrPtr;
					resultSqrPtr = tempPtr;

					tempDigits = resultDigits;
					resultDigits = resultDigitsSqr;
					resultDigitsSqr = tempDigits;

					DigitHelper.SetBlockDigits(resultPtr, shiftOffset, 0U);

					bitShift = bitsAfterDotNextBuffer - bitsAfterDotResultSqr;
					shiftOffset = (uint)(bitShift / Constants.DigitBitCount);

					if (shiftOffset < resultLengthSqrBuf)
					{
						// Shift resultSqrBufPtr on a needed amount of bits to the right
						resultLengthSqrBuf = DigitOpHelper.Shr(
							resultSqrBufPtr + shiftOffset,
							resultLengthSqrBuf - shiftOffset,
							resultSqrBufPtr,
							(int)(bitShift % Constants.DigitBitCount),
							false);

						// Now perform actual subtraction
						resultLength = DigitOpHelper.Sub(
							resultPtr,
							resultLength,
							resultSqrBufPtr,
							resultLengthSqrBuf,
							resultPtr);
					}
					else
					{
						// Actually we can assume resultSqrBufPtr == 0 here and have nothing to do
					}
				}
			}

			// Return some arrays to pool
			ArrayPool<uint>.Instance.AddArray(resultDigitsSqr);

			rightShift += (1UL << lengthLog2Bits) + 1UL;
			newLength = resultLength;
			return resultDigits;
		}
	}
}
