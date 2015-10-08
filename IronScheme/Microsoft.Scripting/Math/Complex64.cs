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
using Oyster.Math;


namespace Microsoft.Scripting.Math {

  
    /// <summary>
    /// Implementation of the complex number data type.
    /// </summary>
  [Serializable]
    public struct Complex64 {
        readonly double real, imag;

        public static Complex64 MakeImaginary(double imag) {
            return new Complex64(0.0, imag);
        }

        public static Complex64 MakeReal(double real) {
            return new Complex64(real, 0.0);
        }

        public static Complex64 Make(double real, double imag) {
            return new Complex64(real, imag);
        }

        public Complex64(double real)
            : this(real, 0.0) {
        }

        public Complex64(double real, double imag) {
            this.real = real;
            this.imag = imag;
        }

        public bool IsZero {
            get {
                return real == 0.0 && imag == 0.0;
            }
        }

        public double Real {
            get {
                return real;
            }
        }

        public double Imag {
            get {
                return imag;
            }
        }

        public Complex64 Conjugate() {
            return new Complex64(real, -imag);
        }


        public override string ToString() {
            if (real == 0.0) return imag.ToString(System.Globalization.CultureInfo.InvariantCulture.NumberFormat) + "j";
            else if (imag < 0.0) return string.Format(System.Globalization.CultureInfo.InvariantCulture.NumberFormat, "({0}{1}j)", real, imag);
            else return string.Format(System.Globalization.CultureInfo.InvariantCulture.NumberFormat, "({0}+{1}j)", real, imag);
        }

        public static implicit operator Complex64(int i) {
            return MakeReal(i);
        }

        [CLSCompliant(false)]
        public static implicit operator Complex64(uint i) {
            return MakeReal(i);
        }

        public static implicit operator Complex64(short i) {
            return MakeReal(i);
        }
        
        [CLSCompliant(false)]
        public static implicit operator Complex64(ushort i) {
            return MakeReal(i);
        }

        public static implicit operator Complex64(long l) {
            return MakeReal(l);
        }
        [CLSCompliant(false)]
        public static implicit operator Complex64(ulong i) {
            return MakeReal(i);
        }

        [CLSCompliant(false)]
        public static implicit operator Complex64(sbyte i) {
            return MakeReal(i);
        }

        public static implicit operator Complex64(byte i) {
            return MakeReal(i);
        }

        public static implicit operator Complex64(float f) {
            return MakeReal(f);
        }

        public static implicit operator Complex64(double d) {
            return MakeReal(d);
        }

        public static implicit operator Complex64(Oyster.Math.IntX i)
        {
            if (object.ReferenceEquals(i, null)) {
                throw new ArgumentException(MathResources.InvalidArgument, "i");
            }

            // throws an overflow exception if we can't handle the value.
            return MakeReal(i.ToFloat64());
        }

        static bool DoubleEqual(double f1, double f2)
        {
          return f1 == f2 
            || (double.IsNegativeInfinity(f1) && double.IsNegativeInfinity(f2))
            || (double.IsPositiveInfinity(f1) && double.IsPositiveInfinity(f2));
        }

        public static bool operator ==(Complex64 x, Complex64 y) {
          return DoubleEqual(x.real, y.real) && DoubleEqual(x.imag, y.imag);
        }

        public static bool operator !=(Complex64 x, Complex64 y) {
            return !(x == y);
        }

        public static Complex64 Add(Complex64 x, Complex64 y) {
            return x + y;
        }

        public static Complex64 operator +(Complex64 x, Complex64 y) {
            return new Complex64(x.real + y.real, x.imag + y.imag);
        }

        public static Complex64 Subtract(Complex64 x, Complex64 y) {
            return x - y;
        }

        public static Complex64 operator -(Complex64 x, Complex64 y) {
            return new Complex64(x.real - y.real, x.imag - y.imag);
        }

        public static Complex64 Multiply(Complex64 x, Complex64 y) {
            return x * y;
        }

        public static Complex64 operator *(Complex64 x, Complex64 y) {
            return new Complex64(x.real * y.real - x.imag * y.imag, x.real * y.imag + x.imag * y.real);
        }

        public static Complex64 Divide(Complex64 x, Complex64 y) {
            return x / y;
        }

        public static Complex64 operator /(Complex64 a, Complex64 b) {
            if (b.IsZero) throw new DivideByZeroException(MathResources.ComplexDivizionByZero);

            double real, imag, den, r;

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

            return new Complex64(real, imag);
        }

        [Obsolete("WTF? DO NOT USE!", true)]
        public static Complex64 Mod(Complex64 x, Complex64 y) {
            return x % y;
        }

        [Obsolete("WTF? DO NOT USE!", true)]
        public static Complex64 operator %(Complex64 x, Complex64 y) {
            if (object.ReferenceEquals(x, null)) {
                throw new ArgumentException(MathResources.InvalidArgument, "x");
            }
            if (object.ReferenceEquals(y, null)) {
                throw new ArgumentException(MathResources.InvalidArgument, "y");
            }

            if (y == 0) throw new DivideByZeroException();

            return 0;
        }

        public static Complex64 Negate(Complex64 x) {
            return -x;
        }

        public static Complex64 operator -(Complex64 x) {
            return new Complex64(-x.real, -x.imag);
        }

        public static Complex64 Plus(Complex64 x) {
            return +x;
        }

        public static Complex64 operator +(Complex64 x) {
            return x;
        }

        public static double Hypot(double x, double y) {

            //  First, get abs
            if (x < 0.0) x = -x;
            if (y < 0.0) y = -y;

            // Obvious cases
            if (x == 0.0) return y;
            if (y == 0.0) return x;

            // Divide smaller number by bigger number to safeguard the (y/x)*(y/x)
            if (x < y) { double temp = y; y = x; x = temp; }

            y /= x;

            // calculate abs(x) * sqrt(1 + (y/x)*(y/x))
            return x * System.Math.Sqrt(1 + y * y);
        }

        public double Abs() {
            return Hypot(real, imag);
        }

        public Complex64 Power(Complex64 y) {
            double c = y.real;
            double d = y.imag;
            int power = (int)c;

            if (power == c && power >= 0 && d == .0) {
                Complex64 result = new Complex64(1.0);
                if (power == 0) return result;
                Complex64 factor = this;
                while (power != 0) {
                    if ((power & 1) != 0) {
                        result = result * factor;
                    }
                    factor = factor * factor;
                    power >>= 1;
                }
                return result;
            } else if (IsZero) {
                return y.IsZero ? Complex64.MakeReal(1.0) : Complex64.MakeReal(0.0);
            } else {
                double a = real;
                double b = imag;
                double powers = a * a + b * b;
                double arg = System.Math.Atan2(b, a);
                double mul = System.Math.Pow(powers, c / 2) * System.Math.Exp(-d * arg);
                double common = c * arg + .5 * d * System.Math.Log(powers);
                return new Complex64(mul * System.Math.Cos(common), mul * System.Math.Sin(common));
            }
        }

        public override int GetHashCode() {
            return (int)real + (int)imag * 1000003;
        }

        public override bool Equals(object obj) {
            if (!(obj is Complex64)) return false;
            return this == ((Complex64)obj);
        }

        public static Complex64 Polar(double modulus, double argument)
        {
          return new Complex64(
                modulus * System.Math.Cos(argument),
                modulus * System.Math.Sin(argument));
        }

        public static Complex64 Cos(Complex64 z)
        {
          Complex64 z1 = Exp(new Complex64(-z.imag, z.real));
          Complex64 z2 = Exp(new Complex64(z.imag, -z.real));

          return new Complex64(0.5 * (z1.real + z2.real), 0.5 * (z1.imag + z2.imag));
        }

        public static Complex64 Cosh(Complex64 z)
        {
          Complex64 z1 = Exp(z);
          Complex64 z2 = Exp(new Complex64(-z.real, -z.imag));

          return new Complex64(0.5 * (z1.real + z2.real), 0.5 * (z1.imag + z2.imag));
        }

        public static Complex64 Sin(Complex64 z)
        {
          Complex64 z1 = Exp(new Complex64(-z.imag, z.real));
          Complex64 z2 = Exp(new Complex64(z.imag, -z.real));

          return new Complex64(0.5 * (z1.imag - z2.imag), 0.5 * (z2.real - z1.real));
        }

        public static Complex64 Sinh(Complex64 z)
        {
          Complex64 z1 = Exp(z);
          Complex64 z2 = Exp(new Complex64(-z.real, -z.imag));

          return new Complex64(0.5 * (z1.real - z2.real), 0.5 * (z1.imag - z2.imag));
        }

        public static Complex64 Tan(Complex64 z)
        {
          return Sin(z) / Cos(z);
        }

        public static Complex64 Tanh(Complex64 z)
        {
          return Sinh(z) / Cosh(z);
        }

        public static Complex64 Exp(Complex64 z)
        {
          double value = System.Math.Exp(z.real);

          return new Complex64(
                value * System.Math.Cos(z.imag),
                value * System.Math.Sin(z.imag));
        }

        public static Complex64 Log(Complex64 z)
        {
          return new Complex64(System.Math.Log(z.Modulus), z.Argument);
        }

        public static Complex64 Pow(Complex64 baseNumber, Complex64 index)
        {
          if (baseNumber == 0)
          {
            return 0;
          }
          return Exp(index * Log(baseNumber));
        }

        public static Complex64 Sqrt(Complex64 z)
        {
          return Polar(System.Math.Sqrt(z.Modulus), z.Argument / 2);
        }
        
        public double Modulus
        {
          [DebuggerStepThrough()]
          get { return System.Math.Sqrt(real * real + imag * imag); }
        }

        public double Argument
        {
          get 
          { 
            return System.Math.Atan2(imag, real);
          }
        }

        readonly static Complex64 i = new Complex64(0, 1);

        public static Complex64 Acos(Complex64 z)
        {
          if (z.imag == 0 && z.real > 0)
          {
            var r = Acos(new Complex64(-z.real, 0));
            return System.Math.PI - r;
          }
          if (z.imag < 0)
          {
            return System.Math.PI - Acos(-z);
          }
          return -i * Log(z + i * Sqrt(1 - (z * z)));
        }

        public static Complex64 Acosh(Complex64 z)
        {
          throw new NotImplementedException();
        }

        public static Complex64 Asin(Complex64 z)
        {
          if (z.imag == 0 && z.real < 0)
          {
            return -Asin(new Complex64(-z.real, 0));
          }
          if (z.imag > 0)
          {
            return -Asin(-z);
          }
          return -i * Log(z * i + Sqrt(1 - (z * z)));
        }

        public static Complex64 Asinh(Complex64 z)
        {
          throw new NotImplementedException();
        }

        public static Complex64 Atan(Complex64 z)
        {
          return i/2 * Log((i + z)/(i - z));
        }

        public static Complex64 Atanh(Complex64 z)
        {
          throw new NotImplementedException();
        }


    }
}
