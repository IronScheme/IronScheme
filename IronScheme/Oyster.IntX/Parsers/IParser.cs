using System.Collections.Generic;

namespace Oyster.Math
{
	/// <summary>
	/// Parser class interface.
	/// </summary>
	internal interface IParser
	{
		/// <summary>
		/// Parses provided string representation of <see cref="IntX" /> object.
		/// </summary>
		/// <param name="value">Number as string.</param>
		/// <param name="numberBase">Number base.</param>
		/// <param name="charToDigits">Char->digit dictionary.</param>
		/// <param name="checkFormat">Check actual format of number (0 or 0x at start).</param>
		/// <returns>Parsed object.</returns>
		IntX Parse(string value, uint numberBase, IDictionary<char, uint> charToDigits, bool checkFormat);
		
		/// <summary>
		/// Parses provided string representation of <see cref="IntX" /> object.
		/// </summary>
		/// <param name="value">Number as string.</param>
		/// <param name="startIndex">Index inside string from which to start.</param>
		/// <param name="endIndex">Index inside string on which to end.</param>
		/// <param name="numberBase">Number base.</param>
		/// <param name="charToDigits">Char->digit dictionary.</param>
		/// <param name="digitsRes">Resulting digits.</param>
		/// <returns>Parsed integer length.</returns>
		uint Parse(string value, int startIndex, int endIndex, uint numberBase, IDictionary<char, uint> charToDigits, uint[] digitsRes);
	}
}
