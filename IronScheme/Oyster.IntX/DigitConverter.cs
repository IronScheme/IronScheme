using System;

namespace Oyster.Math
{
	/// <summary>
	/// Converts <see cref="IntX"/> digits to/from byte array.
	/// </summary>
	[CLSCompliant(false)]
	static public class DigitConverter
	{
		/// <summary>
		/// Converts big integer digits to bytes.
		/// </summary>
		/// <param name="digits"><see cref="IntX" /> digits.</param>
		/// <returns>Resulting bytes.</returns>
		/// <remarks>
		/// Digits can be obtained using <see cref="IntX.GetInternalState" /> method.
		/// </remarks>
		static public byte[] ToBytes(uint[] digits)
		{
			if (digits == null)
			{
				throw new ArgumentNullException("digits");
			}

			byte[] bytes = new byte[digits.Length * 4];
			Buffer.BlockCopy(digits, 0, bytes, 0, bytes.Length);
			return bytes;
		}

		/// <summary>
		/// Converts bytes to big integer digits.
		/// </summary>
		/// <param name="bytes">Bytes.</param>
		/// <returns>Resulting <see cref="IntX" /> digits.</returns>
		/// <remarks>
		/// Big integer can be created from digits using <see cref="IntX(uint[], bool)" /> constructor.
		/// </remarks>
		static public uint[] FromBytes(byte[] bytes)
		{
			if (bytes == null)
			{
				throw new ArgumentNullException("bytes");
			}
			if (bytes.Length % 4 != 0)
			{
				throw new ArgumentException(Strings.DigitBytesLengthInvalid, "bytes");
			}

			uint[] digits = new uint[bytes.Length / 4];
			Buffer.BlockCopy(bytes, 0, digits, 0, bytes.Length);
			return digits;
		}
	}
}
