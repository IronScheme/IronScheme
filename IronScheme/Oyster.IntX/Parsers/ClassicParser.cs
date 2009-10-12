namespace Oyster.Math
{
	/// <summary>
	/// Classic parsing algorithm using multiplication (O[n^2]).
	/// </summary>
	sealed internal class ClassicParser : ParserBase
	{
		#region Constructor

		/// <summary>
		/// Creates new <see cref="ClassicParser" /> instance.
		/// </summary>
		/// <param name="pow2Parser">Parser for pow2 case.</param>
		public ClassicParser(IParser pow2Parser) : base(pow2Parser) {}

		#endregion Constructor

		/// <summary>
		/// Parses provided string representation of <see cref="IntX" /> object.
		/// </summary>
		/// <param name="value">Number as string.</param>
		/// <param name="startIndex">Index inside string from which to start.</param>
		/// <param name="endIndex">Index inside string on which to end.</param>
		/// <param name="numberBase">Number base.</param>
		/// <param name="digitsRes">Resulting digits.</param>
		/// <returns>Parsed integer length.</returns>
		override public uint Parse(string value, int startIndex, int endIndex, uint numberBase, uint[] digitsRes)
		{
			uint newLength = base.Parse(value, startIndex, endIndex, numberBase, digitsRes);

			// Maybe base method already parsed this number
			if (newLength != 0) return newLength;

			// Do parsing in big cycle
			ulong numberBaseLong = numberBase;
			ulong digit;
			for (int i = startIndex; i <= endIndex; ++i)
			{
				digit = StrRepHelper.GetDigit(value[i], numberBase);

				// Next multiply existing values by base and add this value to them
				if (newLength == 0)
				{
					if (digit != 0)
					{
						digitsRes[0] = (uint)digit;
						newLength = 1;
					}
				}
				else
				{
					for (uint j = 0; j < newLength; ++j)
					{
						digit += digitsRes[j] * numberBaseLong;
						digitsRes[j] = (uint)digit;
						digit >>= 32;
					}
					if (digit != 0)
					{
						digitsRes[newLength++] = (uint)digit;
					}
				}
			}

			return newLength;
		}
	}
}
