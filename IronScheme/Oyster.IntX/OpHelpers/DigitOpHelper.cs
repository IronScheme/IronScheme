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
		static unsafe public uint Sub(uint[] digits1, uint length1, uint[] digits2, uint length2, uint[] digitsRes)
		{
			fixed (uint* digitsPtr1 = digits1, digitsPtr2 = digits2, digitsResPtr = digitsRes)
			{
				return Sub(digitsPtr1, length1, digitsPtr2, length2, digitsResPtr);
			}
		}

		/// <summary>
		/// Subtracts two big integers using pointers.
		/// </summary>
		/// <param name="digitsPtr1">First big integer digits.</param>
		/// <param name="length1">First big integer length.</param>
		/// <param name="digitsPtr2">Second big integer digits.</param>
		/// <param name="length2">Second big integer length.</param>
		/// <param name="digitsResPtr">Resulting big integer digits.</param>
		/// <returns>Resulting big integer length.</returns>
		static unsafe public uint Sub(uint* digitsPtr1, uint length1, uint* digitsPtr2, uint length2, uint* digitsResPtr)
					{
			ulong c = 0;

			// Perform digits subtraction
			for (uint i = 0; i < length2; ++i)
				{
				c = (ulong)digitsPtr1[i] - digitsPtr2[i] - c;
				digitsResPtr[i] = (uint)c;
				c >>= 63;
			}

			// Perform digits + carry moving
			for (uint i = length2; i < length1; ++i)
				{
				c = digitsPtr1[i] - c;
				digitsResPtr[i] = (uint)c;
				c >>= 63;
			}

			return DigitHelper.GetRealDigitsLength(digitsResPtr, length1);
		}

		#endregion Subtract operation

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
			int rightShift,
      bool zerolast)
		{
			fixed (uint* digitsPtr = digits, digitsResPtr = digitsRes)
			{
				Shr(digitsPtr + offset, length, digitsResPtr + resOffset, rightShift, resOffset != 0, zerolast);
			}
		}

    unsafe static public uint Shr(uint* digitsPtr, uint length, uint* digitsResPtr, int rightShift, bool resHasOffset)
    {
      return Shr(digitsPtr, length, digitsResPtr, rightShift, resHasOffset, false);
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
		unsafe static public uint Shr(uint* digitsPtr, uint length, uint* digitsResPtr, int rightShift, bool resHasOffset, bool zerolast)
		{
      int rightShiftRev = (Constants.DigitBitCount - rightShift);

			// Shift first digit in special way
			if (resHasOffset)
			{
				digitsResPtr[-1] = digitsPtr[0] << rightShiftRev;
			}

      if (rightShift == 0)
      {
        // Handle special situation here - only memcpy is needed (maybe)
        if (digitsPtr != digitsResPtr)
        {
          DigitHelper.DigitsBlockCopy(digitsPtr, digitsResPtr, length);
        }
      }
      else
      {
        // Shift all digits except last one
        uint* digitsPtrEndPrev = digitsPtr + length - 1;
        uint* digitsPtrNext = digitsPtr + 1;
        for (; digitsPtr < digitsPtrEndPrev; ++digitsPtr, ++digitsPtrNext, ++digitsResPtr)
        {
          *digitsResPtr = *digitsPtr >> rightShift | ((zerolast && digitsPtrNext == digitsPtrEndPrev) ? 0 : *digitsPtrNext << rightShiftRev);
        }

        // Shift last digit in special way
        uint lastValue = *digitsPtr >> rightShift;
        if (lastValue != 0)
        {
          *digitsResPtr = lastValue;
        }
        else
        {
          --length;
        }
      }

			return length;
		}

		#endregion Shift operation
	}
}
