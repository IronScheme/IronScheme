namespace Oyster.Math
{
	/// <summary>
	/// Multiplies using "classic" algorithm.
	/// </summary>
	sealed internal class ClassicMultiplier : MultiplierBase
	{
		/// <summary>
		/// Multiplies two big integers using pointers.
		/// </summary>
		/// <param name="digitsPtr1">First big integer digits.</param>
		/// <param name="length1">First big integer length.</param>
		/// <param name="digitsPtr2">Second big integer digits.</param>
		/// <param name="length2">Second big integer length.</param>
		/// <param name="digitsResPtr">Resulting big integer digits.</param>
		/// <returns>Resulting big integer length.</returns>
		override unsafe public uint Multiply(uint* digitsPtr1, uint length1, uint* digitsPtr2, uint length2, uint* digitsResPtr)
		{
			ulong c;

			// External cycle must be always smaller
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

			// Prepare end pointers
			uint* digitsPtr1End = digitsPtr1 + length1;
			uint* digitsPtr2End = digitsPtr2 + length2;

			// We must always clear first "length1" digits in result
			DigitHelper.SetBlockDigits(digitsResPtr, length1, 0U);

			// Perform digits multiplication
			uint* ptr1, ptrRes = null;
			for (; digitsPtr2 < digitsPtr2End; ++digitsPtr2, ++digitsResPtr)
			{
				// Check for zero (sometimes may help). There is no sense to make this check in internal cycle -
				// it would give performance gain only here
				if (*digitsPtr2 == 0) continue;

				c = 0;
				for (ptr1 = digitsPtr1, ptrRes = digitsResPtr; ptr1 < digitsPtr1End; ++ptr1, ++ptrRes)
				{
					c += (ulong)*digitsPtr2 * *ptr1 + *ptrRes;
					*ptrRes = (uint)c;
					c >>= 32;
				}
				*ptrRes = (uint)c;
			}

			uint newLength = length1 + length2;
			if (newLength > 0 && (ptrRes == null || *ptrRes == 0))
			{
				--newLength;
			}
			return newLength;
		}
	}
}
