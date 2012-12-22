#region License
/* Copyright (c) 2007,2008,2009,2010,2011 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Collections.Generic;
using System.Globalization;
using System.Text.RegularExpressions;
using IronScheme.Runtime;
using Microsoft.Scripting;
using Microsoft.Scripting.Math;
using BigInteger = Oyster.Math.IntX;

namespace IronScheme.Compiler
{
  static class Helper
  {
    public static object ListToVector(Parser p, string type, Cons list)
    {
      if (p.skipnumbers)
      {
        return null;
      }

      type = type.Substring(1, type.Length - 2);
      switch (type)
      {
        case "u8":
        case "vu8":
          return Builtins.ListToByteVector(list);
        case "fl":
          return Builtins.ListToFlonumVector(list);
        case "fx":
          return Builtins.ListToFixnumVector(list);
        default:
          throw new SyntaxErrorException("Not a known vector type: " + type);
      }
    }

    static Regex expnum = new Regex(@"^(?<head>-?((\d+\.?)|(\d*\.\d+)))e(?<tail>-?\d+)$", RegexOptions.Compiled | RegexOptions.ExplicitCapture);

    public static object ParseReal(string s)
    {
      Match m = expnum.Match(s);
      if (m.Success)
      {
        string head = m.Groups["head"].Value;
        string tail = m.Groups["tail"].Value;

        object hnum = Builtins.StringToNumber(head);

        if (hnum is double)
        {
          // can't use process below as 1e11 onwards gets precision loss
          return Convert.ToDouble(s, CultureInfo.InvariantCulture);
        }

        object tnum = Builtins.StringToNumber(tail);

        return Builtins.Multiply(hnum, Expt10(tnum));
      }
      else
      {
        return null;
      }
    }

    readonly static BigInteger TEN = 10;

    static object Expt10(int tnum)
    {
      if (tnum < 0)
      {
        return new Fraction(1, BigInteger.Pow(10, (uint)-tnum));
      }
      return BigInteger.Pow(TEN, (uint)tnum);
    }

    static object Expt10(object tnum)
    {
      if (tnum is int)
      {
        return Expt10((int)tnum);
      }
      else if (tnum is BigInteger)
      {
        return Expt10((int)(BigInteger)tnum);
      }
      else if (tnum is double)
      {
        return Math.Pow(10, (double)tnum);
      }
      else
      {
        // we're fucked.
        return null;
      }
    }

    static readonly Regex escapes = new Regex(@"\\(([ntr\\""])|(x[\da-f]+;))", RegexOptions.Compiled | RegexOptions.IgnoreCase);

    public static string CleanString(string input)
    {
      input = input.Substring(1, input.Length - 2);

      input = input.Replace("\r", "");

      input = ProcessStringContinuations(input);

      input = escapes.Replace(input, delegate(Match m)
      {
        string s = m.Value;

        switch (s[1])
        {
          case '\\':
            return "\\";
          case 'n':
            return "\n";
          case 't':
            return "\t";
          case 'r':
            return "\r";
          case '"':
            return "\"";
          case 'x':
            {
              string ss = m.Value;
              ss = ss.Substring(2, s.Length - 3);
              int iv = int.Parse(ss, NumberStyles.HexNumber);
              return ((char)iv).ToString();
            }
        }
        return s;
      });

      return input;
    }

    private static string ProcessStringContinuations(string input)
    {
      // deal with string continuations
      string[] lines = input.Split('\n');

      if (lines.Length > 1)
      {
        List<string> fixup = new List<string>();

        bool refix = false;

        for (int i = 0; i < lines.Length; i++)
        {
          if (lines[i].EndsWith("\\") && !lines[i].EndsWith("\\\\"))
          {
            string line = lines[i];
            string tail = lines[i + 1];

            int index = 0;
            for (int j = 0; j < tail.Length; j++)
            {
              if (!(tail[j] == ' ' || tail[j] == '\t'))
              {
                index = j;
                break;
              }
            }

            string newline = line.Substring(0, line.Length - 1) + tail.Substring(index);
            fixup.Add(newline);
            refix = true;
            i++;
          }
          else
          {
            fixup.Add(lines[i]);
          }
        }

        string c = string.Join("\n", fixup.ToArray());
        if (refix)
        {
          return ProcessStringContinuations(c);
        }
        else
        {
          return c;
        }
      }
      else
      {
        return lines[0];
      }
    }

    public static string ParseChar(string input)
    {
      string output = null;
      switch (input.Substring(2))
      {
        case "nul":
          return ((char)0).ToString();
        case "alarm":
          return ((char)7).ToString();
        case "backspace":
          return ((char)8).ToString();
        case "tab":
          return ((char)9).ToString();
        case "vtab":
          return ((char)11).ToString();
        case "page":
          return ((char)12).ToString();
        case "return":
          return ((char)13).ToString();
        case "esc":
          return ((char)0x1b).ToString();
        case "delete":
          return ((char)0x7f).ToString();
        case "":
        case "space":
          output = " ";
          break;
        case "linefeed":
        case "newline":
          output = "\n";
          break;
        default:
          if (input[2] == 'x' && input.Length > 3)
          {
            //hex escape
            int utf32 = int.Parse(input.Substring(3), System.Globalization.NumberStyles.HexNumber);

            if (((utf32 < 0) || (utf32 > 1114111)) || ((utf32 >= 55296) && (utf32 <= 57343)))
            {
              throw new SyntaxErrorException(string.Format("not a valid Unicode value: {0}", utf32));
            }

            output = char.ConvertFromUtf32(utf32);
          }
          else
          {
            if (input.Length != 3)
            {
              throw new SyntaxErrorException(string.Format("unknown escape sequence: {0}", input));
            }
            output = input[2].ToString();
          }
          break;
      }

      return output;
    }

    static bool IsExact(object obj)
    {
      return obj is int || obj is BigInteger || obj is Fraction || obj is ComplexFraction;
    }

    static Fraction GetFraction(object o)
    {
      if (o is int)
      {
        return (int)o;
      }
      if (o is BigInteger)
      {
        return (BigInteger)o;
      }
      if (o is Fraction)
      {
        return (Fraction)o;
      }
      return null;
    }

    public static object MakeRectangular(object obj1, object obj2)
    {
      if (IsExact(obj1) && IsExact(obj2))
      {
        var f = GetFraction(obj2);
        if (f == 0)
        {
          return obj1;
        }
        return ComplexFraction.Make(GetFraction(obj1), f);
      }
      else
      {
        double o2 = SafeConvert(obj2);
        if (o2 == 0.0 && !(obj2 is double))
        {
          return obj1;
        }
        return Complex64.Make(SafeConvert(obj1), o2);
      }
    }

    static double SafeConvert(object obj1)
    {
      return Convert.ToDouble(obj1);
    }

    public static object MakePolar(object obj1, object obj2)
    {
      double o2 = SafeConvert(obj2);
      if (o2 == 0.0 && !(obj2 is double))
      {
        return obj1;
      }
      object r = MakeRectangular(Math.Cos(o2), Math.Sin(o2));
      if (r is double)
      {
        return SafeConvert(obj1) * (double)r;
      }
      else
      {
        var c = SafeConvert(obj1) * (Complex64)r;
        return c;
      }
    }
  }
}
