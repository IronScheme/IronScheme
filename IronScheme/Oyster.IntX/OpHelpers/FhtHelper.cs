namespace Oyster.Math
{
	/// <summary>
	/// Contains helping methods for work with FHT (Fast Hartley Transform).
	/// FHT is a better alternative of FFT (Fast Fourier Transform) - at least for <see cref="IntX" />.
	/// </summary>
	static unsafe internal class FhtHelper
	{
		#region struct TrigValues

		/// <summary>
		/// Trigonometry values.
		/// </summary>
		struct TrigValues
		{
			/// <summary>
			/// Sin value from <see cref="SineTable" />.
			/// </summary>
			public double TableSin;

			/// <summary>
			/// Cos value from <see cref="SineTable" />.
			/// </summary>
			public double TableCos;

			/// <summary>
			/// Sin value.
			/// </summary>
			public double Sin;

			/// <summary>
			/// Cos value.
			/// </summary>
			public double Cos;
		}

		#endregion struct SinCos

		#region Private constants (or static readonly fields)

		// double[] data base
		const int DoubleDataBytes = 1;
		const int DoubleDataLengthShift = 2 - (DoubleDataBytes >> 1);
		const int DoubleDataDigitShift = DoubleDataBytes << 3;
		const long DoubleDataBaseInt = 1L << DoubleDataDigitShift;
		const double DoubleDataBase = DoubleDataBaseInt;
		const double DoubleDataBaseDiv2 = DoubleDataBase / 2.0;

		// SQRT(2) and SQRT(2) / 2
		static readonly double Sqrt2 = System.Math.Sqrt(2.0);
		static readonly double Sqrt2Div2 = Sqrt2 / 2.0;

		// SIN() table
		static readonly double[] SineTable = new double[31];

		#endregion Private constants (or static readonly fields)

		#region Constructors

		// .cctor
		static FhtHelper()
		{
			// Initialize SinTable
			FillSineTable(SineTable);
		}

		#endregion Constructors

		#region Data conversion methods

		/// <summary>
		/// Converts <see cref="IntX" /> digits into real representation (used in FHT).
		/// </summary>
		/// <param name="digits">Big integer digits.</param>
		/// <param name="length"><paramref name="digits" /> length.</param>
		/// <param name="newLength">Multiplication result length (must be pow of 2).</param>
		/// <returns>Double array.</returns>
		static public double[] ConvertDigitsToDouble(uint[] digits, uint length, uint newLength)
		{
			fixed (uint* digitsPtr = digits)
			{
				return ConvertDigitsToDouble(digitsPtr, length, newLength);
			}
		}

		/// <summary>
		/// Converts <see cref="IntX" /> digits into real representation (used in FHT).
		/// </summary>
		/// <param name="digitsPtr">Big integer digits.</param>
		/// <param name="length"><paramref name="digitsPtr" /> length.</param>
		/// <param name="newLength">Multiplication result length (must be pow of 2).</param>
		/// <returns>Double array.</returns>
		static public double[] ConvertDigitsToDouble(uint* digitsPtr, uint length, uint newLength)
		{
			// Maybe fix newLength (make it the nearest bigger pow of 2)
			newLength = 1U << Bits.CeilLog2(newLength);

			// For better FHT accuracy we will choose length smaller then dwords.
			// So new length must be modified accordingly
			newLength <<= DoubleDataLengthShift;
			double[] data = ArrayPool<double>.Instance.GetArray(newLength);

			// Run in unsafe context
			fixed (double* slice = data)
			{
				// Amount of units pointed by digitsPtr
				uint unitCount = length << DoubleDataLengthShift;

				// Copy all words from digits into new double[]
				byte* unitDigitsPtr = (byte*)digitsPtr;
				for (uint i = 0; i < unitCount; ++i)
				{
					slice[i] = unitDigitsPtr[i];
				}

				// FHT (as well as FFT) works more accurate with "balanced" data, so let's balance it
				double carry = 0, dataDigit;
				for (uint i = 0; i < unitCount || i < newLength && carry != 0; ++i)
				{
					dataDigit = slice[i] + carry;
					if (dataDigit >= DoubleDataBaseDiv2)
					{
						dataDigit -= DoubleDataBase;
						carry = 1.0;
					}
					else
					{
						carry = 0;
					}
					slice[i] = dataDigit;
				}

				if (carry > 0)
				{
					slice[0] -= carry;
				}
			}

			return data;
		}

		/// <summary>
		/// Converts real digits representation (result of FHT) into usual <see cref="IntX" /> digits.
		/// </summary>
		/// <param name="array">Real digits representation.</param>
		/// <param name="length"><paramref name="array" /> length.</param>
		/// <param name="digitsLength">New digits array length (we always do know the upper value for this array).</param>
		/// <param name="digitsRes">Big integer digits.</param>
		static unsafe public void ConvertDoubleToDigits(double[] array, uint length, uint digitsLength, uint[] digitsRes)
		{
			fixed (double* slice = array)
			fixed (uint* digitsResPtr = digitsRes)
			{
				ConvertDoubleToDigits(slice, length, digitsLength, digitsResPtr);
			}
		}

		/// <summary>
		/// Converts real digits representation (result of FHT) into usual <see cref="IntX" /> digits.
		/// </summary>
		/// <param name="slice">Real digits representation.</param>
		/// <param name="length"><paramref name="slice" /> length.</param>
		/// <param name="digitsLength">New digits array length (we always do know the upper value for this array).</param>
		/// <param name="digitsResPtr">Resulting digits storage.</param>
		/// <returns>Big integer digits (dword values).</returns>
		static unsafe public void ConvertDoubleToDigits(double* slice, uint length, uint digitsLength, uint* digitsResPtr)
		{
			// Calculate data multiplier (don't forget about additional div 2)
			double normalizeMultiplier = 0.5 / length;

			// Count of units in digits
			uint unitCount = digitsLength << DoubleDataLengthShift;

			// Carry and current digit
			double carry = 0, dataDigit;
			long carryInt = 0, dataDigitInt;

#if DEBUG
			// Rounding error
			double error, maxError = 0;
#endif

			// Walk thru all double digits
			byte* unitDigitsPtr = (byte*)digitsResPtr;
			for (uint i = 0; i < length; ++i)
			{
				// Get data digit (don't forget it might be balanced)
				dataDigit = slice[i] * normalizeMultiplier;

#if DEBUG
				// Round to the nearest
				dataDigitInt = (long)(dataDigit < 0 ? dataDigit - 0.5 : dataDigit + 0.5);

				// Calculate and (maybe) store max error
				error = System.Math.Abs(dataDigit - dataDigitInt);
				if (error > maxError)
				{
					maxError = error;
				}

				// Add carry
				dataDigitInt += carryInt;
#else
				// Round to the nearest
				dataDigitInt = (long)(dataDigit < 0 ? dataDigit - 0.5 : dataDigit + 0.5) + carryInt;
#endif

				// Get next carry floored; maybe modify data digit
				carry = dataDigitInt / DoubleDataBase;
				if (carry < 0)
				{
					carry += carry % 1.0;
				}
				carryInt = (long)carry;

				dataDigitInt -= carryInt << DoubleDataDigitShift;
				if (dataDigitInt < 0)
				{
					dataDigitInt += DoubleDataBaseInt;
					--carryInt;
				}

				// Maybe add to the digits
				if (i < unitCount)
				{
					unitDigitsPtr[i] = (byte)dataDigitInt;
				}
			}

			// Last carry must be accounted
			if (carryInt < 0)
			{
				digitsResPtr[0] -= (uint)-carryInt;
			}
			else if (carryInt > 0)
			{
				uint digitsCarry = (uint)carryInt, oldDigit;
				for (uint i = 0; digitsCarry != 0 && i < digitsLength; ++i)
				{
					oldDigit = digitsResPtr[i];
					digitsResPtr[i] += digitsCarry;

					// Check for an overflow
					digitsCarry = digitsResPtr[i] < oldDigit ? 1U : 0U;
				}
			}

#if DEBUG
			// Finally store max error in public visible field
			lock (IntX._maxFhtRoundErrorLock)
			{
				if (maxError > IntX.MaxFhtRoundError)
				{
					IntX.MaxFhtRoundError = maxError;
				}
			}
#endif
		}

		#endregion Data conversion methods

		#region FHT

		/// <summary>
		/// Performs FHT "in place" for given double[] array.
		/// </summary>
		/// <param name="array">Double array.</param>
		/// <param name="length">Array length.</param>
		static public void Fht(double[] array, uint length)
		{
			fixed (double* slice = array)
			{
				Fht(slice, length, Bits.Msb(length));
			}
		}

		/// <summary>
		/// Performs FHT "in place" for given double[] array slice.
		/// </summary>
		/// <param name="slice">Double array slice.</param>
		/// <param name="length">Slice length.</param>
		/// <param name="lengthLog2">Log2(<paramref name="length" />).</param>
		static public void Fht(double* slice, uint length, int lengthLog2)
		{
			// Special fast processing for length == 4
			if (length == 4)
			{
				Fht4(slice);
				return;
			}

			// Divide data into 2 recursively processed parts
			length >>= 1;
			--lengthLog2;
			double* rightSlice = slice + length;

			uint lengthDiv2 = length >> 1;
			uint lengthDiv4 = length >> 2;

			// Perform initial "butterfly" operations over left and right array parts
			double leftDigit = slice[0];
			double rightDigit = rightSlice[0];
			slice[0] = leftDigit + rightDigit;
			rightSlice[0] = leftDigit - rightDigit;

			leftDigit = slice[lengthDiv2];
			rightDigit = rightSlice[lengthDiv2];
			slice[lengthDiv2] = leftDigit + rightDigit;
			rightSlice[lengthDiv2] = leftDigit - rightDigit;

			// Get initial trig values
			//TrigValues trigValues = GetInitialTrigValues(lengthLog2);
			TrigValues trigValues = new TrigValues();
			GetInitialTrigValues(&trigValues, lengthLog2);

			// Perform "butterfly"
			for (uint i = 1; i < lengthDiv4; ++i)
			{
				FhtButterfly(slice, rightSlice, i, length - i, trigValues.Cos, trigValues.Sin);
				FhtButterfly(slice, rightSlice, lengthDiv2 - i, lengthDiv2 + i, trigValues.Sin, trigValues.Cos);

				// Get next trig values
				NextTrigValues(&trigValues);
			}

			// Final "butterfly"
			FhtButterfly(slice, rightSlice, lengthDiv4, length - lengthDiv4, Sqrt2Div2, Sqrt2Div2);

			// Finally perform recursive run
			Fht(slice, length, lengthLog2);
			Fht(rightSlice, length, lengthLog2);
		}

		/// <summary>
		/// Performs FHT "in place" for given double[] array slice.
		/// Fast version for length == 4.
		/// </summary>
		/// <param name="slice">Double array slice.</param>
		static void Fht4(double* slice)
		{
			// Get 4 digits
			double d0 = slice[0];
			double d1 = slice[1];
			double d2 = slice[2];
			double d3 = slice[3];

			// Perform fast "butterfly" addition/subtraction for them.
			// In case when length == 4 we can do it without trigonometry
			double d02 = d0 + d2;
			double d13 = d1 + d3;
			slice[0] = d02 + d13;
			slice[1] = d02 - d13;

			d02 = d0 - d2;
			d13 = d1 - d3;
			slice[2] = d02 + d13;
			slice[3] = d02 - d13;
		}

		#endregion FHT

		#region FHT results multiplication

		/// <summary>
		/// Multiplies two FHT results and stores multiplication in first one.
		/// </summary>
		/// <param name="data">First FHT result.</param>
		/// <param name="data2">Second FHT result.</param>
		/// <param name="length">FHT results length.</param>
		static public void MultiplyFhtResults(double[] data, double[] data2, uint length)
		{
			fixed (double* slice = data, slice2 = data2)
			{
				MultiplyFhtResults(slice, slice2, length);
			}
		}

		/// <summary>
		/// Multiplies two FHT results and stores multiplication in first one.
		/// </summary>
		/// <param name="slice">First FHT result.</param>
		/// <param name="slice2">Second FHT result.</param>
		/// <param name="length">FHT results length.</param>
		static public void MultiplyFhtResults(double* slice, double* slice2, uint length)
		{
			// Step0 and Step1
			slice[0] *= 2.0 * slice2[0];
			slice[1] *= 2.0 * slice2[1];

			// Perform all other steps
			double d11, d12, d21, d22, ad, sd;
			for (uint stepStart = 2, stepEnd = 4, index1, index2; stepStart < length; stepStart *= 2, stepEnd *= 2)
			{
				for (index1 = stepStart, index2 = stepEnd - 1; index1 < stepEnd; index1 += 2, index2 -= 2)
				{
					d11 = slice[index1];
					d12 = slice[index2];
					d21 = slice2[index1];
					d22 = slice2[index2];

					ad = d11 + d12;
					sd = d11 - d12;

					slice[index1] = d21 * ad + d22 * sd;
					slice[index2] = d22 * ad - d21 * sd;
				}
			}
		}

		#endregion FHT results multiplication

		#region Reverse FHT

		/// <summary>
		/// Performs FHT reverse "in place" for given double[] array.
		/// </summary>
		/// <param name="array">Double array.</param>
		/// <param name="length">Array length.</param>
		static public void ReverseFht(double[] array, uint length)
		{
			fixed (double* slice = array)
			{
				ReverseFht(slice, length, Bits.Msb(length));
			}
		}

		/// <summary>
		/// Performs reverse FHT "in place" for given double[] array slice.
		/// </summary>
		/// <param name="slice">Double array slice.</param>
		/// <param name="length">Slice length.</param>
		/// <param name="lengthLog2">Log2(<paramref name="length" />).</param>
		static public void ReverseFht(double* slice, uint length, int lengthLog2)
		{
			// Special fast processing for length == 8
			if (length == 8)
			{
				ReverseFht8(slice);
				return;
			}

			// Divide data into 2 recursively processed parts
			length >>= 1;
			--lengthLog2;
			double* rightSlice = slice + length;

			uint lengthDiv2 = length >> 1;
			uint lengthDiv4 = length >> 2;

			// Perform recursive run
			ReverseFht(slice, length, lengthLog2);
			ReverseFht(rightSlice, length, lengthLog2);

			// Get initial trig values
			TrigValues trigValues = new TrigValues();
			GetInitialTrigValues(&trigValues, lengthLog2);

			// Perform "butterfly"
			for (uint i = 1; i < lengthDiv4; ++i)
			{
				ReverseFhtButterfly(slice, rightSlice, i, length - i, trigValues.Cos, trigValues.Sin);
				ReverseFhtButterfly(slice, rightSlice, lengthDiv2 - i, lengthDiv2 + i, trigValues.Sin, trigValues.Cos);

				// Get next trig values
				NextTrigValues(&trigValues);
			}

			// Final "butterfly"
			ReverseFhtButterfly(slice, rightSlice, lengthDiv4, length - lengthDiv4, Sqrt2Div2, Sqrt2Div2);
			ReverseFhtButterfly2(slice, rightSlice, 0, 0, 1.0, 0);
			ReverseFhtButterfly2(slice, rightSlice, lengthDiv2, lengthDiv2, 0, 1.0);
		}

		/// <summary>
		/// Performs reverse FHT "in place" for given double[] array slice.
		/// Fast version for length == 8.
		/// </summary>
		/// <param name="slice">Double array slice.</param>
		static void ReverseFht8(double* slice)
		{
			// Get 8 digits	
			double d0 = slice[0];
			double d1 = slice[1];
			double d2 = slice[2];
			double d3 = slice[3];
			double d4 = slice[4];
			double d5 = slice[5];
			double d6 = slice[6];
			double d7 = slice[7];

			// Calculate add and subtract pairs for first 4 digits
			double da01 = d0 + d1;
			double ds01 = d0 - d1;
			double da23 = d2 + d3;
			double ds23 = d2 - d3;

			// Calculate add and subtract pairs for first pairs
			double daa0123 = da01 + da23;
			double dsa0123 = da01 - da23;
			double das0123 = ds01 + ds23;
			double dss0123 = ds01 - ds23;

			// Calculate add and subtract pairs for next 4 digits
			double da45 = d4 + d5;
			double ds45 = (d4 - d5) * Sqrt2;
			double da67 = d6 + d7;
			double ds67 = (d6 - d7) * Sqrt2;

			// Calculate add and subtract pairs for next pairs
			double daa4567 = da45 + da67;
			double dsa4567 = da45 - da67;

			// Store digits values
			slice[0] = daa0123 + daa4567;
			slice[4] = daa0123 - daa4567;
			slice[2] = dsa0123 + dsa4567;
			slice[6] = dsa0123 - dsa4567;
			slice[1] = das0123 + ds45;
			slice[5] = das0123 - ds45;
			slice[3] = dss0123 + ds67;
			slice[7] = dss0123 - ds67;
		}

		#endregion Reverse FHT

		#region "Butterfly" methods for FHT

		/// <summary>
		/// Performs "butterfly" operation for <see cref="Fht(double*, uint, int)" />.
		/// </summary>
		/// <param name="slice1">First data array slice.</param>
		/// <param name="slice2">Second data array slice.</param>
		/// <param name="index1">First slice index.</param>
		/// <param name="index2">Second slice index.</param>
		/// <param name="cos">Cos value.</param>
		/// <param name="sin">Sin value.</param>
		static void FhtButterfly(double* slice1, double* slice2, uint index1, uint index2, double cos, double sin)
		{
			double d11 = slice1[index1];
			double d12 = slice1[index2];

			double temp = slice2[index1];
			slice1[index1] = d11 + temp;
			d11 -= temp;

			temp = slice2[index2];
			slice1[index2] = d12 + temp;
			d12 -= temp;

			slice2[index1] = d11 * cos + d12 * sin;
			slice2[index2] = d11 * sin - d12 * cos;
		}

		/// <summary>
		/// Performs "butterfly" operation for <see cref="ReverseFht(double*, uint, int)" />.
		/// </summary>
		/// <param name="slice1">First data array slice.</param>
		/// <param name="slice2">Second data array slice.</param>
		/// <param name="index1">First slice index.</param>
		/// <param name="index2">Second slice index.</param>
		/// <param name="cos">Cos value.</param>
		/// <param name="sin">Sin value.</param>
		static void ReverseFhtButterfly(double* slice1, double* slice2, uint index1, uint index2, double cos, double sin)
		{
			double d21 = slice2[index1];
			double d22 = slice2[index2];

			double temp = slice1[index1];
			double temp2 = d21 * cos + d22 * sin;
			slice1[index1] = temp + temp2;
			slice2[index1] = temp - temp2;

			temp = slice1[index2];
			temp2 = d21 * sin - d22 * cos;
			slice1[index2] = temp + temp2;
			slice2[index2] = temp - temp2;
		}

		/// <summary>
		/// Performs "butterfly" operation for <see cref="ReverseFht(double*, uint, int)" />.
		/// Another version.
		/// </summary>
		/// <param name="slice1">First data array slice.</param>
		/// <param name="slice2">Second data array slice.</param>
		/// <param name="index1">First slice index.</param>
		/// <param name="index2">Second slice index.</param>
		/// <param name="cos">Cos value.</param>
		/// <param name="sin">Sin value.</param>
		static void ReverseFhtButterfly2(double* slice1, double* slice2, uint index1, uint index2, double cos, double sin)
		{
			double temp = slice1[index1];
			double temp2 = slice2[index1] * cos + slice2[index2] * sin;
			slice1[index1] = temp + temp2;
			slice2[index2] = temp - temp2;
		}

		#endregion "Butterfly" methods for FHT

		#region Trigonometry working methods

		/// <summary>
		/// Fills sine table for FHT.
		/// </summary>
		/// <param name="sineTable">Sine table to fill.</param>
		static void FillSineTable(double[] sineTable)
		{
			for (int i = 0, p = 1; i < sineTable.Length; ++i, p *= 2)
			{
				sineTable[i] = System.Math.Sin(System.Math.PI / p);
			}
		}

		/// <summary>
		/// Initializes trigonometry values for FHT.
		/// </summary>
		/// <param name="valuesPtr">Values to init.</param>
		/// <param name="lengthLog2">Log2(processing slice length).</param>
		static void GetInitialTrigValues(TrigValues* valuesPtr, int lengthLog2)
		{
			valuesPtr->TableSin = SineTable[lengthLog2];
			valuesPtr->TableCos = SineTable[lengthLog2 + 1];
			valuesPtr->TableCos *= -2.0 * valuesPtr->TableCos;

			valuesPtr->Sin = valuesPtr->TableSin;
			valuesPtr->Cos = valuesPtr->TableCos + 1.0;
		}

		/// <summary>
		/// Generates next trigonometry values for FHT basing on previous ones.
		/// </summary>
		/// <param name="valuesPtr">Current trig values.</param>
		static void NextTrigValues(TrigValues* valuesPtr)
		{
			double oldCos = valuesPtr->Cos;
			valuesPtr->Cos = valuesPtr->Cos * valuesPtr->TableCos - valuesPtr->Sin * valuesPtr->TableSin + valuesPtr->Cos;
			valuesPtr->Sin = valuesPtr->Sin * valuesPtr->TableCos + oldCos * valuesPtr->TableSin + valuesPtr->Sin;
		}

		#endregion Trigonometry working methods
	}
}
