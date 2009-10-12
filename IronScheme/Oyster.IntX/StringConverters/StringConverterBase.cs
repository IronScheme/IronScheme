using System;
using System.Text;

namespace Oyster.Math
{
	/// <summary>
	/// Base class for ToString converters.
	/// Contains default implementations of convert operation over <see cref="IntX" /> instances.
	/// </summary>
	abstract internal class StringConverterBase : IStringConverter
	{
		#region Private fields

		IStringConverter _pow2StringConverter; // converter for pow2 case

		#endregion Private fields

		#region Constructor

		/// <summary>
		/// Creates new <see cref="StringConverterBase" /> instance.
		/// </summary>
		/// <param name="pow2StringConverter">Converter for pow2 case.</param>
		public StringConverterBase(IStringConverter pow2StringConverter)
		{
			_pow2StringConverter = pow2StringConverter;
		}

		#endregion Constructor

		/// <summary>
		/// Returns string representation of <see cref="IntX" /> object in given base.
		/// </summary>
		/// <param name="intX">Big integer to convert.</param>
		/// <param name="numberBase">Base of system in which to do output.</param>
		/// <param name="upperCase">Use uppercase for bases from 11 to 16 (which use letters A-F).</param>
		/// <returns>Object string representation.</returns>
		/// <exception cref="ArgumentException"><paramref name="numberBase" /> is less then 2 or <paramref name="intX" /> is too big to fit in string.</exception>
		virtual public string ToString(IntX intX, uint numberBase, bool upperCase)
		{
			// Test base
			if (numberBase < 2 || numberBase > 65536)
			{
				throw new ArgumentException(Strings.ToStringSmallBase, "numberBase");
			}

			// Special processing for zero values
			if (intX._length == 0) return "0";

			// Calculate output array length
			uint outputLength = (uint)System.Math.Ceiling(Constants.DigitBaseLog / System.Math.Log(numberBase) * intX._length);

			// Define length coefficient for string builder
			bool isBigBase = numberBase > 16;
			uint lengthCoef = isBigBase ? (uint)System.Math.Ceiling(System.Math.Log10(numberBase)) + 2U : 1U;

			// Determine maximal possible length of string
			ulong maxBuilderLength = (ulong)outputLength * lengthCoef + 1UL;
			if (maxBuilderLength > int.MaxValue)
			{
				// This big integer can't be transformed to string
				throw new ArgumentException(Strings.IntegerTooBig, "intX");
			}

			// Transform digits into another base
			uint[] outputArray = ToString(intX._digits, intX._length, numberBase, ref outputLength);

			// Output everything to the string builder
			char[] usedChars = upperCase ? Constants.BaseUpperChars : Constants.BaseLowerChars;
			StringBuilder outputBuilder = new StringBuilder((int)(outputLength * lengthCoef + 1));

			// Maybe append minus sign
			if (intX._negative)
			{
				outputBuilder.Append(Constants.DigitsMinusChar);
			}

			// Output all digits
			for (uint i = outputLength - 1; i < outputLength; --i)
			{
				if (!isBigBase)
				{
					// Output char-by-char for bases up to 16
					outputBuilder.Append(usedChars[(int)outputArray[i]]);
				}
				else
				{
					// Output digits in bracets for bigger bases
					outputBuilder.Append(Constants.DigitOpeningBracet);
					outputBuilder.Append(outputArray[i].ToString());
					outputBuilder.Append(Constants.DigitClosingBracet);
				}
			}

			return outputBuilder.ToString();
		}

		/// <summary>
		/// Converts digits from internal representaion into given base.
		/// </summary>
		/// <param name="digits">Big integer digits.</param>
		/// <param name="length">Big integer length.</param>
		/// <param name="numberBase">Base to use for output.</param>
		/// <param name="outputLength">Calculated output length (will be corrected inside).</param>
		/// <returns>Conversion result (later will be transformed to string).</returns>
		virtual public uint[] ToString(uint[] digits, uint length, uint numberBase, ref uint outputLength)
		{
			// Default implementation - always call pow2 converter if numberBase is pow of 2
			return numberBase == 1U << Bits.Msb(numberBase)
				? _pow2StringConverter.ToString(digits, length, numberBase, ref outputLength)
				: null;
		}
	}
}
