namespace Oyster.Math
{
	/// <summary>
	/// Multiplies using FHT.
	/// </summary>
	sealed internal class AutoFhtMultiplier : MultiplierBase
	{
		#region Private fields

		IMultiplier _classicMultiplier; // multiplier to use if FHT is unapplicatible

		#endregion Private fields

		#region Constructor

		/// <summary>
		/// Creates new <see cref="AutoFhtMultiplier" /> instance.
		/// </summary>
		/// <param name="classicMultiplier">Multiplier to use if FHT is unapplicatible.</param>
		public AutoFhtMultiplier(IMultiplier classicMultiplier)
		{
			_classicMultiplier = classicMultiplier;
		}

		#endregion Constructor

		/// <summary>
		/// Multiplies two big integers using pointers.
		/// </summary>
		/// <param name="digitsPtr1">First big integer digits.</param>
		/// <param name="length1">First big integer length.</param>
		/// <param name="digitsPtr2">Second big integer digits.</param>
		/// <param name="length2">Second big integer length.</param>
		/// <param name="digitsResPtr">Resulting big integer digits.</param>
		/// <returns>Resulting big integer real length.</returns>
		override unsafe public uint Multiply(uint* digitsPtr1, uint length1, uint* digitsPtr2, uint length2, uint* digitsResPtr)
		{
			// Check length - maybe use classic multiplier instead
			if (length1 < Constants.AutoFhtLengthLowerBound || length2 < Constants.AutoFhtLengthLowerBound ||
			    length1 > Constants.AutoFhtLengthUpperBound || length2 > Constants.AutoFhtLengthUpperBound)
			{
				return _classicMultiplier.Multiply(digitsPtr1, length1, digitsPtr2, length2, digitsResPtr);
			}

			uint newLength = length1 + length2;

			// Do FHT for first big integer
			double[] data1 = FhtHelper.ConvertDigitsToDouble(digitsPtr1, length1, newLength);
			FhtHelper.Fht(data1, (uint)data1.LongLength);

			// Compare digits
			double[] data2;
			if (digitsPtr1 == digitsPtr2 || DigitOpHelper.Cmp(digitsPtr1, length1, digitsPtr2, length2) == 0)
			{
				// Use the same FHT for equal big integers
				data2 = data1;
			}
			else
			{
				// Do FHT over second digits
				data2 = FhtHelper.ConvertDigitsToDouble(digitsPtr2, length2, newLength);
				FhtHelper.Fht(data2, (uint)data2.LongLength);
			}

			// Perform multiplication and reverse FHT
			FhtHelper.MultiplyFhtResults(data1, data2, (uint)data1.LongLength);
			FhtHelper.ReverseFht(data1, (uint)data1.LongLength);

			// Convert to digits
			fixed (double* slice1 = data1)
			{
				FhtHelper.ConvertDoubleToDigits(slice1, (uint)data1.LongLength, newLength, digitsResPtr);
			}

			// Return double arrays back to pool
			ArrayPool<double>.Instance.AddArray(data1);
			if (data2 != data1)
			{
				ArrayPool<double>.Instance.AddArray(data2);
			}

			return digitsResPtr[newLength - 1] == 0 ? --newLength : newLength;
		}
	}
}
