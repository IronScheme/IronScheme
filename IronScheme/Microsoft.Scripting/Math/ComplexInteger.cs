/* ****************************************************************************
 *
 * Copyright (c) Microsoft Corporation. 
 *
 * This source code is subject to terms and conditions of the Microsoft Public License. A 
 * copy of the license can be found in the License.html file at the root of this distribution. If 
 * you cannot locate the  Microsoft Public License, please send an email to 
 * dlr@microsoft.com. By using this source code in any fashion, you are agreeing to be bound 
 * by the terms of the Microsoft Public License.
 *
 * You must not remove this notice, or any other, from this software.
 *
 *
 * ***************************************************************************/

using System;
using System.Diagnostics;
using System.Text;
using System.Collections;

namespace Microsoft.Scripting.Math {
    /// <summary>
    /// Implementation of the complex number data type.
    /// </summary>
  [Serializable]
    public struct ComplexInteger {
        private readonly int real, imag;

        public static ComplexInteger MakeImaginary(int imag) {
            return new ComplexInteger(0, imag);
        }

        public static ComplexInteger MakeReal(int real) {
            return new ComplexInteger(real, 0);
        }

        public static ComplexInteger Make(int real, int imag) {
            return new ComplexInteger(real, imag);
        }

        public ComplexInteger(int real)
            : this(real, 0) {
        }

        public ComplexInteger(int real, int imag) {
            this.real = real;
            this.imag = imag;
        }

        public bool IsZero {
            get {
                return real == 0 && imag == 0;
            }
        }

        public int Real {
            get {
                return real;
            }
        }

        public int Imag {
            get {
                return imag;
            }
        }

        public ComplexInteger Conjugate() {
            return new ComplexInteger(real, -imag);
        }


        public override string ToString() {
            if (real == 0) return imag.ToString(System.Globalization.CultureInfo.InvariantCulture.NumberFormat) + "j";
            else if (imag < 0) return string.Format(System.Globalization.CultureInfo.InvariantCulture.NumberFormat, "({0}{1}j)", real, imag);
            else return string.Format(System.Globalization.CultureInfo.InvariantCulture.NumberFormat, "({0}+{1}j)", real, imag);
        }

        public static implicit operator ComplexInteger(int i) {
            return MakeReal(i);
        }



        public static implicit operator ComplexInteger(short i) {
            return MakeReal(i);
        }
        


        [CLSCompliant(false)]
        public static implicit operator ComplexInteger(sbyte i) {
            return MakeReal(i);
        }

        public static implicit operator ComplexInteger(byte i) {
            return MakeReal(i);
        }


        public static implicit operator ComplexInteger(BigInteger i) {
            if (object.ReferenceEquals(i, null)) {
                throw new ArgumentException(MathResources.InvalidArgument, "i");
            }

            // throws an overflow exception if we can't handle the value.
            return MakeReal(i.ToInt32());
        }

        public static bool operator ==(ComplexInteger x, ComplexInteger y) {
            return x.real == y.real && x.imag == y.imag;
        }

        public static bool operator !=(ComplexInteger x, ComplexInteger y) {
            return x.real != y.real || x.imag != y.imag;
        }

        public static ComplexInteger Add(ComplexInteger x, ComplexInteger y) {
            return x + y;
        }

        public static ComplexInteger operator +(ComplexInteger x, ComplexInteger y) {
            return new ComplexInteger(x.real + y.real, x.imag + y.imag);
        }

        public static ComplexInteger Subtract(ComplexInteger x, ComplexInteger y) {
            return x - y;
        }

        public static ComplexInteger operator -(ComplexInteger x, ComplexInteger y) {
            return new ComplexInteger(x.real - y.real, x.imag - y.imag);
        }

        public static ComplexInteger Multiply(ComplexInteger x, ComplexInteger y) {
            return x * y;
        }

        public static ComplexInteger operator *(ComplexInteger x, ComplexInteger y) {
            return new ComplexInteger(x.real * y.real - x.imag * y.imag, x.real * y.imag + x.imag * y.real);
        }

        public static ComplexInteger Divide(ComplexInteger x, ComplexInteger y) {
            return x / y;
        }

        public static ComplexInteger operator /(ComplexInteger a, ComplexInteger b) {
            if (b.IsZero) throw new DivideByZeroException(MathResources.ComplexDivizionByZero);

            int real, imag, den, r;

            if (System.Math.Abs(b.real) >= System.Math.Abs(b.imag)) {
                r = b.imag / b.real;
                den = b.real + r * b.imag;
                real = (a.real + a.imag * r) / den;
                imag = (a.imag - a.real * r) / den;
            } else {
                r = b.real / b.imag;
                den = b.imag + r * b.real;
                real = (a.real * r + a.imag) / den;
                imag = (a.imag * r - a.real) / den;
            }

            return new ComplexInteger(real, imag);
        }

        public static ComplexInteger Mod(ComplexInteger x, ComplexInteger y) {
            return x % y;
        }

        public static ComplexInteger operator %(ComplexInteger x, ComplexInteger y) {
            if (object.ReferenceEquals(x, null)) {
                throw new ArgumentException(MathResources.InvalidArgument, "x");
            }
            if (object.ReferenceEquals(y, null)) {
                throw new ArgumentException(MathResources.InvalidArgument, "y");
            }

            if (y == 0) throw new DivideByZeroException();

            throw new NotImplementedException();
        }

        public static ComplexInteger Negate(ComplexInteger x) {
            return -x;
        }

        public static ComplexInteger operator -(ComplexInteger x) {
            return new ComplexInteger(-x.real, -x.imag);
        }

        public static ComplexInteger Plus(ComplexInteger x) {
            return +x;
        }

        public static ComplexInteger operator +(ComplexInteger x) {
            return x;
        }

        public static int Hypot(int x, int y) {
            //
            // sqrt(x*x + y*y) == sqrt(x*x * (1 + (y*y)/(x*x))) ==
            // sqrt(x*x) * sqrt(1 + (y/x)*(y/x)) ==
            // abs(x) * sqrt(1 + (y/x)*(y/x))
            //

            //  First, get abs
            if (x < 0) x = -x;
            if (y < 0) y = -y;

            // Obvious cases
            if (x == 0) return y;
            if (y == 0) return x;

            // Divide smaller number by bigger number to safeguard the (y/x)*(y/x)
            if (x < y) { int temp = y; y = x; x = temp; }

            y /= x;

            // calculate abs(x) * sqrt(1 + (y/x)*(y/x))
            return (int)( x * System.Math.Sqrt(1 + y * y));
        }

        public int Abs() {
            return Hypot(real, imag);
        }

        public ComplexInteger Power(ComplexInteger y) {
            int c = y.real;
            int d = y.imag;
            int power = (int)c;

            if (power == c && power >= 0 && d == .0) {
                ComplexInteger result = new ComplexInteger(1);
                if (power == 0) return result;
                ComplexInteger factor = this;
                while (power != 0) {
                    if ((power & 1) != 0) {
                        result = result * factor;
                    }
                    factor = factor * factor;
                    power >>= 1;
                }
                return result;
            } else if (IsZero) {
                return y.IsZero ? ComplexInteger.MakeReal(1) : ComplexInteger.MakeReal(0);
            } else {
                int a = real;
                int b = imag;
                int powers = a * a + b * b;
                double arg = System.Math.Atan2(b, a);
                double mul = System.Math.Pow(powers, c / 2) * System.Math.Exp(-d * arg);
                double common = c * arg + .5 * d * System.Math.Log(powers);
                return new ComplexInteger((int)(mul * System.Math.Cos(common)), (int)(mul * System.Math.Sin(common)));
            }
        }

        public override int GetHashCode() {
            return (int)real + (int)imag * 1000003;
        }

        public override bool Equals(object obj) {
            if (!(obj is ComplexInteger)) return false;
            return this == ((ComplexInteger)obj);
        }
    }
}