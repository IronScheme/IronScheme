using System;
using System.Collections.Generic;

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
		/// <param name="charToDigits">Char->digit dictionary.</param>
		/// <param name="ch">Char which represents big integer digit.</param>
		/// <param name="numberBase">String representation number base.</param>
		/// <returns>Digit.</returns>
		/// <exception cref="FormatException"><paramref name="ch" /> is not in valid format.</exception>
		static public uint GetDigit(IDictionary<char, uint> charToDigits, char ch, uint numberBase)
		{
			if (charToDigits == null)
			{
				throw new ArgumentNullException("charToDigits");
			}

			// Try to identify this digit
			uint digit;
			if (!charToDigits.TryGetValue(ch, out digit))
			{
				throw new FormatException(Strings.ParseInvalidChar);
			}
			if (digit >= numberBase)
			{
				throw new FormatException(Strings.ParseTooBigDigit);
			}
			return digit;
		}

		/// <summary>
		/// Verfies string alphabet provider by user for validity.
		/// </summary>
		/// <param name="alphabet">Alphabet.</param>
		/// <param name="numberBase">String representation number base.</param>
		static public void AssertAlphabet(string alphabet, uint numberBase)
		{
			if (alphabet == null)
			{
				throw new ArgumentNullException("alphabet");
			}

			// Ensure that alphabet has enough characters to represent numbers in given base
			if (alphabet.Length < numberBase)
			{
				throw new ArgumentException(string.Format(Strings.AlphabetTooSmall, numberBase), "alphabet");
			}

			// Ensure that all the characters in alphabet are unique
			char[] sortedChars = alphabet.ToCharArray();
			Array.Sort(sortedChars);
			for (int i = 0; i < sortedChars.Length; i++)
			{
				if (i > 0 && sortedChars[i] == sortedChars[i - 1])
				{
					throw new ArgumentException(Strings.AlphabetRepeatingChars, "alphabet");
				}
			}
		}

		/// <summary>
		/// Generates char->digit dictionary from alphabet.
		/// </summary>
		/// <param name="alphabet">Alphabet.</param>
		/// <param name="numberBase">String representation number base.</param>
		/// <returns>Char->digit dictionary.</returns>
		static public IDictionary<char, uint> CharDictionaryFromAlphabet(string alphabet, uint numberBase)
		{
			AssertAlphabet(alphabet, numberBase);
			Dictionary<char, uint> charToDigits = new Dictionary<char, uint>((int)numberBase);
			for (int i = 0; i < numberBase; i++)
			{
				charToDigits.Add(alphabet[i], (uint)i);
			}
			return charToDigits;
		}
	}
}
