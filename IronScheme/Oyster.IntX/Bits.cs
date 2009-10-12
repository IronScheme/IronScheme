using System;

namespace Oyster.Math
{
	/// <summary>
	/// Contains helping methods to with with bits in dword (<see cref="UInt32" />).
	/// </summary>
	[CLSCompliant(false)]
	static public class Bits
	{
		/// <summary>
		/// Returns number of leading zero bits in int.
		/// </summary>
		/// <param name="x">Int value.</param>
		/// <returns>Number of leading zero bits.</returns>
		static public int Nlz(uint x)
		{
			if (x == 0) return 32;
			
			int n = 1;
			if ((x >> 16) == 0) { n += 16; x <<= 16; }
			if ((x >> 24) == 0) { n +=  8; x <<=  8; }
			if ((x >> 28) == 0) { n +=  4; x <<=  4; }
			if ((x >> 30) == 0) { n +=  2; x <<=  2; }
			return n - (int)(x >> 31);
		}
		
		/// <summary>
		/// Counts position of the most significant bit in int.
		/// Can also be used as Floor(Log2(<paramref name="x" />)).
		/// </summary>
		/// <param name="x">Int value.</param>
		/// <returns>Position of the most significant one bit (-1 if all zeroes).</returns>
		static public int Msb(uint x)
		{
			return 31 - Nlz(x);
		}
		
		/// <summary>
		/// Ceil(Log2(<paramref name="x" />)).
		/// </summary>
		/// <param name="x">Int value.</param>
		/// <returns>Ceil of the Log2.</returns>
		static public int CeilLog2(uint x)
		{
			int msb = Msb(x);
			if (x != 1U << msb)
			{
				++msb;
			}
			return msb;
		}
	}
}
