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
		/// <param name="alphabet">Alphabet which contains chars used to represent big integer, char position is coresponding digit value.</param>
		/// <returns>Object string representation.</returns>
		string ToString(IntX intX, uint numberBase, char[] alphabet);
		
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
