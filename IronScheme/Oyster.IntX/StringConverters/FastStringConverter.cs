using System;
using System.Collections;

namespace Oyster.Math
{
	/// <summary>
	/// Fast ToString converting algorithm using divide-by-two.
	/// </summary>
	sealed internal class FastStringConverter : StringConverterBase
	{
		#region Private fields

		IStringConverter _classicStringConverter; // classic converter

		#endregion Private fields

		#region Constructor

		/// <summary>
		/// Creates new <see cref="FastStringConverter" /> instance.
		/// </summary>
		/// <param name="pow2StringConverter">Converter for pow2 case.</param>
		/// <param name="classicStringConverter">Classic converter.</param>
		public FastStringConverter(IStringConverter pow2StringConverter, IStringConverter classicStringConverter) :
			base(pow2StringConverter)
		{
			_classicStringConverter = classicStringConverter;
		}

		#endregion Constructor

		/// <summary>
		/// Converts digits from internal representaion into given base.
		/// </summary>
		/// <param name="digits">Big integer digits.</param>
		/// <param name="length">Big integer length.</param>
		/// <param name="numberBase">Base to use for output.</param>
		/// <param name="outputLength">Calculated output length (will be corrected inside).</param>
		/// <returns>Conversion result (later will be transformed to string).</returns>
		override unsafe public uint[] ToString(uint[] digits, uint length, uint numberBase, ref uint outputLength)
		{
			uint[] outputArray = base.ToString(digits, length, numberBase, ref outputLength);

			// Maybe base method already converted this number
			if (outputArray != null) return outputArray;

			// Check length - maybe use classic converter instead
			if (length < Constants.FastConvertLengthLowerBound || length > Constants.FastConvertLengthUpperBound)
			{
				return _classicStringConverter.ToString(digits, length, numberBase, ref outputLength);
			}

			int resultLengthLog2 = Bits.CeilLog2(outputLength);
			uint resultLength = 1U << resultLengthLog2;

			// Create and initially fill array for transofmed numbers storing
			uint[] resultArray = ArrayPool<uint>.Instance.GetArray(resultLength);
			Array.Copy(digits, resultArray, length);

			// Create and initially fill array with lengths
			uint[] resultArray2 = ArrayPool<uint>.Instance.GetArray(resultLength);
			resultArray2[0] = length;

			IMultiplier multiplier = MultiplyManager.GetCurrentMultiplier();
			IDivider divider = DivideManager.GetCurrentDivider();

			// Generate all needed pows of numberBase in stack
			Stack baseIntStack = new Stack(resultLengthLog2);
			IntX baseInt = null;
			for (int i = 0; i < resultLengthLog2; ++i)
			{
				baseInt = baseInt == null ? numberBase : multiplier.Multiply(baseInt, baseInt);
				baseIntStack.Push(baseInt);
			}

			// Create temporary buffer for second digits when doing div operation
			uint[] tempBuffer = new uint[baseInt._length];

			// We will use unsafe code here
			fixed (uint* resultPtr1Const = resultArray, resultPtr2Const = resultArray2, tempBufferPtr = tempBuffer)
			{
				// Results pointers which will be modified (on swap)
				uint* resultPtr1 = resultPtr1Const;
				uint* resultPtr2 = resultPtr2Const;

				// Temporary variables used on swapping
				uint[] tempArray;
				uint* tempPtr;

				// Variables used in cycle
				uint* ptr1, ptr2, ptr1end;
				uint loLength;

				// Outer cycle instead of recursion
				for (uint innerStep = resultLength >> 1, outerStep = resultLength; innerStep > 0; innerStep >>= 1, outerStep >>= 1)
				{
					// Prepare pointers
					ptr1 = resultPtr1;
					ptr2 = resultPtr2;
					ptr1end = resultPtr1 + resultLength;

					// Get baseInt from stack and fix it too
					baseInt = (IntX)baseIntStack.Pop();
					fixed (uint* baseIntPtr = baseInt._digits)
					{
						// Cycle thru all digits and their lengths
						for (; ptr1 < ptr1end; ptr1 += outerStep, ptr2 += outerStep)
						{
							// Divide ptr1 (with length in *ptr2) by baseIntPtr here.
							// Results are stored in ptr2 & (ptr2 + innerStep), lengths - in *ptr1 and (*ptr1 + innerStep)
							loLength = *ptr2;
							*(ptr1 + innerStep) = divider.DivMod(
								ptr1,
								ptr2,
								ref loLength,
								baseIntPtr,
								tempBufferPtr,
								baseInt._length,
								ptr2 + innerStep,
								DivModResultFlags.Div | DivModResultFlags.Mod,
								-2);
							*ptr1 = loLength;
						}
					}

					// After inner cycle resultArray will contain lengths and resultArray2 will contain actual values
					// so we need to swap them here
					tempArray = resultArray;
					resultArray = resultArray2;
					resultArray2 = tempArray;

					tempPtr = resultPtr1;
					resultPtr1 = resultPtr2;
					resultPtr2 = tempPtr;
				}

				// Retrieve real output length
				outputLength = DigitHelper.GetRealDigitsLength(resultArray2, outputLength);

				// Create output array
				outputArray = new uint[outputLength];

				// Copy each digit but only if length is not null
				fixed (uint* outputPtr = outputArray)
				{
					for (uint i = 0; i < outputLength; ++i)
					{
						if (resultPtr2[i] != 0)
						{
							outputPtr[i] = resultPtr1[i];
						}
					}
				}
			}

			// Return temporary arrays to pool
			ArrayPool<uint>.Instance.AddArray(resultArray);
			ArrayPool<uint>.Instance.AddArray(resultArray2);

			return outputArray;
		}
	}
}
