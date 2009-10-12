using System;

namespace Oyster.Math
{
	/// <summary>
	/// Helps to work with <see cref="IntX" /> string representations.
	/// </summary>
	static internal class StrRepHelper
	{
		/// <summary>
		/// Returns digit for given char.
		/// </summary>
		/// <param name="ch">Char which represents big integer digit.</param>
		/// <param name="numberBase">String representation number base.</param>
		/// <returns>Digit.</returns>
		/// <exception cref="FormatException"><paramref name="ch" /> is not in valid format.</exception>
		static public uint GetDigit(char ch, uint numberBase)
		{
			// Try to identify this digit using array
			int searchIndex = Array.BinarySearch(Constants.BaseLowerChars, char.ToLower(ch));
			if (searchIndex < 0)
			{
				throw new FormatException(Strings.ParseInvalidChar);
			}
			if (searchIndex >= numberBase)
			{
				throw new FormatException(Strings.ParseTooBigDigit);
			}

			return (uint)searchIndex;
		}
	}
}
