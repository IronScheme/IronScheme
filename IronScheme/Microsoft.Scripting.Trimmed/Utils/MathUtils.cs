using System;
using System.Collections.Generic;
using System.Text;

namespace Microsoft.Scripting.Utils {
    using Math = System.Math;

    public static class MathUtils {
        /// <summary>
        /// Behaves like Math.Round(value, MidpointRounding.AwayFromZero)
        /// Needed because CoreCLR doesn't support this particular overload of Math.Round
        /// </summary>
        public static double RoundAwayFromZero(double value) {
#if !SILVERLIGHT
            return Math.Round(value, MidpointRounding.AwayFromZero);
#else
            if (value < 0) {
                return -RoundAwayFromZero(-value);
            }
        
            // we can assume positive value
            double result = Math.Floor(value);
            if (value - result >= 0.5) {
                result += 1.0;
            }
            return result;
#endif
        }

        private static readonly double[] _RoundPowersOfTens = new double[] { 1E0, 1E1, 1E2, 1E3, 1E4, 1E5, 1E6, 1E7, 1E8, 1E9, 1E10, 1E11, 1E12, 1E13, 1E14, 1E15 };

        private static double GetPowerOf10(int precision) {
            return (precision < 16) ? _RoundPowersOfTens[precision] : Math.Pow(10, precision);
        }

        /// <summary>
        /// Behaves like Math.Round(value, precision, MidpointRounding.AwayFromZero)
        /// However, it works correctly on negative precisions and cases where precision is
        /// outside of the [-15, 15] range.
        /// 
        /// (This function is also needed because CoreCLR lacks this overload.)
        /// </summary>
        public static double RoundAwayFromZero(double value, int precision) {
            if (precision >= 0) {
                double num = GetPowerOf10(precision);
                return RoundAwayFromZero(value * num) / num;
            } else {
                // Note: this code path could be merged with the precision >= 0 path,
                // (by extending the cache to negative powers of 10)
                // but the results seem to be more precise if we do it this way
                double num = GetPowerOf10(-precision);
                return RoundAwayFromZero(value / num) * num;
            }
        }
    }

}
