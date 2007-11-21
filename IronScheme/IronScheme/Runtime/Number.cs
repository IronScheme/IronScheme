#region License
/* ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 
 *
 * This source code is subject to terms and conditions of the Microsoft Public License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Public License.
 *
 * You must not remove this notice, or any other, from this software.
 * ***************************************************************************/
#endregion

using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting.Math;

namespace IronScheme.Runtime
{
#if NEW_NUMBERS
  
  public abstract class Number
  {
    protected virtual Number Add(Number n)
    {
      throw new NotSupportedException();
    }

    public static Number operator + (Number a, Number b)
    {
      return a.Add(b);
    }

    protected virtual Number Subtract(Number n)
    {
      throw new NotSupportedException();
    }

    public static Number operator -(Number a, Number b)
    {
      return a.Subtract(b);
    }


    protected virtual Number Multiply(Number n)
    {
      throw new NotSupportedException();
    }

    public static Number operator *(Number a, Number b)
    {
      return a.Multiply(b);
    }


    protected virtual Number Divide(Number n)
    {
      throw new NotSupportedException();
    }

    public static Number operator /(Number a, Number b)
    {
      return a.Divide(b);
    }

    public static implicit operator Number(int n)
    {
      return new Integer(n);
    }


    public abstract bool IsComplex { get;}
    public abstract bool IsReal { get;}
    public abstract bool IsRational { get;}
    public abstract bool IsInteger { get;}

    public virtual bool IsExact
    {
      get { return false; }
    }

    public virtual bool IsInexact
    {
      get { return false; }
    }
  }

  public abstract class Number<T> : Number
  {
    protected readonly T value;

    protected Number(T value)
    {
      this.value = value;
    }

    public static implicit operator T(Number<T> n)
    {
      return n.value;
    }
  }

  public class Rational : Number<Fraction>
  {
    public Rational(Fraction value) : base(value)
    {
    }

    public override bool IsExact
    {
      get { return true; }
    }

    public override bool IsComplex
    {
      get { return true; }
    }

    public override bool IsReal
    {
      get { return true; }
    }

    public override bool IsRational
    {
      get { return true; }
    }

    public override bool IsInteger
    {
      get { return false; }
    }

  }

  public class Integer : Number<int>
  {
    public Integer(int value) : base(value)
    {
    }

    public override bool IsExact
    {
      get { return true; }
    }

    public override bool IsComplex
    {
      get { return true; }
    }

    public override bool IsReal
    {
      get { return true; }
    }

    public override bool IsRational
    {
      get { return true; }
    }

    public override bool IsInteger
    {
      get { return true; }
    }
  }

  public class Complex : Number<Complex64>
  {
    public Complex(Complex64 value) : base(value)
    {

    }

    public override bool IsInexact
    {
      get { return true; }
    }

    public override bool IsComplex
    {
      get { return true; }
    }

    public override bool IsReal
    {
      get { return false; }
    }

    public override bool IsRational
    {
      get { return false; }
    }

    public override bool IsInteger
    {
      get { return false; }
    }
  }

  public class Real : Number<double>
  {
    public Real(double value) : base(value)
    {

    }

    public override bool IsInexact
    {
      get { return true; }
    }

    public override bool IsComplex
    {
      get { return true; }
    }

    public override bool IsReal
    {
      get { return true; }
    }

    public override bool IsRational
    {
      get { return false; }
    }

    public override bool IsInteger
    {
      get { return false; }
    }
  }
#endif
}
