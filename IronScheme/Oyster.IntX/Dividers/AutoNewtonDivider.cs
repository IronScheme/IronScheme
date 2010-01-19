namespace Oyster.Math
{
	/// <summary>
	/// Divides using Newton approximation approach.
	/// </summary>
	sealed internal class AutoNewtonDivider : DividerBase
	{
		#region Private fields

		IDivider _classicDivider; // divider to use if Newton approach is unapplicatible

		#endregion Private fields

		#region Constructor

		/// <summary>
		/// Creates new <see cref="AutoNewtonDivider" /> instance.
		/// </summary>
		/// <param name="classicDivider">Divider to use if Newton approach is unapplicatible.</param>
		public AutoNewtonDivider(IDivider classicDivider)
		{
			_classicDivider = classicDivider;
		}

		#endregion Constructor

		#region Utilitary methods

		/// <summary>
		/// Returns true if it's better to use classic algorithm for given big integers.
		/// </summary>
		/// <param name="length1">First big integer length.</param>
		/// <param name="length2">Second big integer length.</param>
		/// <returns>True if classic algorithm is better.</returns>
		bool IsClassicAlgorithmNeeded(uint length1, uint length2)
		{
			return
				length1 < Constants.AutoNewtonLengthLowerBound || length2 < Constants.AutoNewtonLengthLowerBound ||
				length1 > Constants.AutoNewtonLengthUpperBound || length2 > Constants.AutoNewtonLengthUpperBound;
		}

		#endregion Utilitary methods

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
			// Maybe immediately use classic algorithm here
			if (IsClassicAlgorithmNeeded(length1, length2))
			{
				return _classicDivider.DivMod(
					digits1,
					digitsBuffer1,
					ref length1,
					digits2,
					digitsBuffer2,
					length2,
					digitsRes,
					resultFlags,
					cmpResult);
			}

			// Create some buffers if necessary
			if (digitsBuffer1 == null)
			{
				digitsBuffer1 = new uint[length1 + 1];
			}

			fixed (uint* digitsPtr1 = digits1, digitsBufferPtr1 = digitsBuffer1, digitsPtr2 = digits2, digitsBufferPtr2 = digitsBuffer2 != null ? digitsBuffer2 : digits1, digitsResPtr = digitsRes != null ? digitsRes : digits1)
			{
				return DivMod(
					digitsPtr1,
					digitsBufferPtr1,
					ref length1,
					digitsPtr2,
					digitsBufferPtr2 == digitsPtr1 ? null : digitsBufferPtr2,
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
			// Maybe immediately use classic algorithm here
			if (IsClassicAlgorithmNeeded(length1, length2))
			{
				return _classicDivider.DivMod(
					digitsPtr1,
					digitsBufferPtr1,
					ref length1,
					digitsPtr2,
					digitsBufferPtr2,
					length2,
					digitsResPtr,
					resultFlags,
					cmpResult);
			}

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


			// First retrieve opposite for the divider
			uint int2OppositeLength;
			ulong int2OppositeRightShift;
			uint[] int2OppositeDigits = NewtonHelper.GetIntegerOpposite(
				digitsPtr2,
				length2,
				length1,
				digitsBufferPtr1,
				out int2OppositeLength,
				out int2OppositeRightShift);

			// We will need to muptiply it by divident now to receive quotient.
			// Prepare digits for multiply result
			uint quotLength;
			uint[] quotDigits = new uint[length1 + int2OppositeLength];

			IMultiplier multiplier = MultiplyManager.GetCurrentMultiplier();

			// Fix some arrays
			fixed (uint* oppositePtr = int2OppositeDigits, quotPtr = quotDigits)
			{
				// Multiply
				quotLength = multiplier.Multiply(
					oppositePtr,
					int2OppositeLength,
					digitsPtr1,
					length1,
					quotPtr);

				// Calculate shift
				uint shiftOffset = (uint)(int2OppositeRightShift / Constants.DigitBitCount);
				int shiftCount = (int)(int2OppositeRightShift % Constants.DigitBitCount);

				// Get the very first bit of the shifted part
				uint highestLostBit;
				if (shiftCount == 0)
				{
					highestLostBit = quotPtr[shiftOffset - 1] >> 31;
				}
				else
				{
					highestLostBit = quotPtr[shiftOffset] >> (shiftCount - 1) & 1U;
				}

				// After this result must be shifted to the right - this is required
				quotLength = DigitOpHelper.Shr(
					quotPtr + shiftOffset,
					quotLength - shiftOffset,
					quotPtr,
					shiftCount,
					false);

				// Maybe quotient must be corrected
				if (highestLostBit == 1U)
				{
					quotLength = DigitOpHelper.Add(quotPtr, quotLength, &highestLostBit, 1U, quotPtr);
				}

				// Check quotient - finally it might be too big.
				// For this we must multiply quotient by divider
				uint quotDivLength;
				uint[] quotDivDigits = new uint[quotLength + length2];
				fixed (uint* quotDivPtr = quotDivDigits)
				{
					quotDivLength = multiplier.Multiply(quotPtr, quotLength, digitsPtr2, length2, quotDivPtr);

					int cmpRes = DigitOpHelper.Cmp(quotDivPtr, quotDivLength, digitsPtr1, length1);
					if (cmpRes > 0)
					{
						highestLostBit = 1;
						quotLength = DigitOpHelper.Sub(quotPtr, quotLength, &highestLostBit, 1U, quotPtr);
						quotDivLength = DigitOpHelper.Sub(quotDivPtr, quotDivLength, digitsPtr2, length2, quotDivPtr);
					}

					// Now everything is ready and prepared to return results

					// First maybe fill remainder
					if ((resultFlags & DivModResultFlags.Mod) != 0)
					{
						length1 = DigitOpHelper.Sub(digitsPtr1, length1, quotDivPtr, quotDivLength, digitsBufferPtr1);
					}

					// And finally fill quotient
					if ((resultFlags & DivModResultFlags.Div) != 0)
					{
						DigitHelper.DigitsBlockCopy(quotPtr, digitsResPtr, quotLength);
						return quotLength;
					}
					else
					{
						quotLength = 0;
					}

					// Return some arrays to pool
					ArrayPool<uint>.Instance.AddArray(int2OppositeDigits);

					return quotLength;
				}
			}
		}
	}
}
