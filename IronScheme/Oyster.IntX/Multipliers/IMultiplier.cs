namespace Oyster.Math
{
	/// <summary>
	/// Multiplier class interface.
	/// </summary>
	internal interface IMultiplier
	{
		/// <summary>
		/// Multiplies two big integers.
		/// </summary>
		/// <param name="int1">First big integer.</param>
		/// <param name="int2">Second big integer.</param>
		/// <returns>Resulting big integer.</returns>
		IntX Multiply(IntX int1, IntX int2);
		
		/// <summary>
		/// Multiplies two big integers represented by their digits.
		/// </summary>
		/// <param name="digits1">First big integer digits.</param>
		/// <param name="length1">First big integer real length.</param>
		/// <param name="digits2">Second big integer digits.</param>
		/// <param name="length2">Second big integer real length.</param>
		/// <param name="digitsRes">Where to put resulting big integer.</param>
		/// <returns>Resulting big integer real length.</returns>
		uint Multiply(uint[] digits1, uint length1, uint[] digits2, uint length2, uint[] digitsRes);
		
		/// <summary>
		/// Multiplies two big integers using pointers.
		/// </summary>
		/// <param name="digitsPtr1">First big integer digits.</param>
		/// <param name="length1">First big integer length.</param>
		/// <param name="digitsPtr2">Second big integer digits.</param>
		/// <param name="length2">Second big integer length.</param>
		/// <param name="digitsResPtr">Resulting big integer digits.</param>
		/// <returns>Resulting big integer length.</returns>
		unsafe uint Multiply(uint* digitsPtr1, uint length1, uint* digitsPtr2, uint length2, uint* digitsResPtr);
	}
}
