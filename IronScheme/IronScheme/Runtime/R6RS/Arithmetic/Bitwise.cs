using System;
using System.Collections.Generic;
using System.Text;
using System.ComponentModel;
using Microsoft.Scripting.Math;

namespace IronScheme.Runtime.R6RS.Arithmetic
{
  public class Bitwise : Builtins
  {
    static TypeConverter BigIntConverter = TypeDescriptor.GetConverter(typeof(BigInteger));

    [Builtin("bitwise-not")]
    public static object BitWiseNot(object ei)
    {
      return BigIntConverter.ConvertTo( ~(BigInteger)BigIntConverter.ConvertFrom(ei), ei.GetType());
    }
  }
}
