using System;

namespace Oyster.Math
{
	/// <summary>
	/// Classic ToString converting algorithm using division (O[n^2]).
	/// </summary>
	sealed internal class ClassicStringConverter : StringConverterBase
	{
		#region Constructor

		/// <summary>
		/// Creates new <see cref="ClassicStringConverter" /> instance.
		/// </summary>
		/// <param name="pow2StringConverter">Converter for pow2 case.</param>
		public ClassicStringConverter(IStringConverter pow2StringConverter) : base(pow2StringConverter) {}

		#endregion Constructor

		/// <summary>
		/// Converts digits from internal representaion into given base.
		/// </summary>
		/// <param name="digits">Big integer digits.</param>
		/// <param name="length">Big integer length.</param>
		/// <param name="numberBase">Base to use for output.</param>
		/// <param name="outputLength">Calculated output length (will be corrected inside).</param>
		/// <returns>Conversion result (later will be transformed to string).</returns>
		override public uint[] ToString(uint[] digits, uint length, uint numberBase, ref uint outputLength)
		{
			uint[] outputArray = base.ToString(digits, length, numberBase, ref outputLength);

			// Maybe base method already converted this number
			if (outputArray != null) return outputArray;

			// Create an output array for storing of number in other base
			outputArray = new uint[outputLength + 1];

			// Make a copy of initial data
			uint[] digitsCopy = new uint[length];
			Array.Copy(digits, digitsCopy, length);

			// Calculate output numbers by dividing
			uint outputIndex;
			for (outputIndex = 0; length > 0; ++outputIndex)
			{
				length = DigitOpHelper.DivMod(digitsCopy, length, numberBase, digitsCopy, out outputArray[outputIndex]);
			}

			outputLength = outputIndex;
			return outputArray;
		}
	}
}
