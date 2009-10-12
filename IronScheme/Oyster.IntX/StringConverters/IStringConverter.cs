namespace Oyster.Math
{
	/// <summary>
	/// ToString converter class interface.
	/// </summary>
	internal interface IStringConverter
	{
		/// <summary>
		/// Returns string representation of <see cref="IntX" /> object in given base.
		/// </summary>
		/// <param name="intX">Big integer to convert.</param>
		/// <param name="numberBase">Base of system in which to do output.</param>
		/// <param name="upperCase">Use uppercase for bases from 11 to 16 (which use letters A-F).</param>
		/// <returns>Object string representation.</returns>
		string ToString(IntX intX, uint numberBase, bool upperCase);
		
		/// <summary>
		/// Converts digits from internal representaion into given base.
		/// </summary>
		/// <param name="digits">Big integer digits.</param>
		/// <param name="length">Big integer length.</param>
		/// <param name="numberBase">Base to use for output.</param>
		/// <param name="outputLength">Calculated output length (will be corrected inside).</param>
		/// <returns>Conversion result (later will be transformed to string).</returns>
		uint[] ToString(uint[] digits, uint length, uint numberBase, ref uint outputLength);
	}
}
