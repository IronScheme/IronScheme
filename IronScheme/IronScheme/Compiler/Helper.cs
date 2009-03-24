using System;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using System.Globalization;
using IronScheme.Runtime;
using Microsoft.Scripting.Math;

namespace IronScheme.Compiler
{
  static class Helper
  {
    static Regex expnum = new Regex(@"^(?<head>-?\d+)e(?<tail>-?\d+)$", RegexOptions.Compiled | RegexOptions.ExplicitCapture);

    public static object ParseReal(string s)
    {
      Match m = expnum.Match(s);
      if (m.Success)
      {
        string head = m.Groups["head"].Value;
        string tail = m.Groups["tail"].Value;

        object hnum = Builtins.StringToNumber(head);
        object tnum = Builtins.StringToNumber(tail);

        return Builtins.Multiply(hnum, Builtins.Expt(10, tnum));
      }
      else
      {
        return null;
      }
    }

    static Regex unichar = new Regex(@"\\x[\da-f]+;", RegexOptions.Compiled | RegexOptions.IgnoreCase);
    static Regex escapes = new Regex(@"\\[ntr\\""]", RegexOptions.Compiled | RegexOptions.IgnoreCase);

    public static string CleanString(string input)
    {
      input = input.Substring(1, input.Length - 2);

      input = unichar.Replace(input, delegate(Match m)
      {
        string s = m.Value;
        s = s.Substring(2, s.Length - 3);
        int iv = int.Parse(s, NumberStyles.HexNumber);
        return ((char)iv).ToString();
      });

      input = escapes.Replace(input, delegate(Match m)
      {
        string s = m.Value;

        switch (s[1])
        {
          case 'n':
            return "\n";
          case 't':
            return "\t";
          case 'r':
            return "";
          case '"':
            return "\"";
        }
        return s;
      });

      input = input.Replace("\r", "");

      input = ProcessStringContinuations(input);

      input = escapes.Replace(input, delegate(Match m)
      {
        string s = m.Value;

        switch (s[1])
        {
          case '\\':
            return "\\";
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
              Runtime.Builtins.LexicalError("not a valid Unicode value", utf32);
            }

            output = char.ConvertFromUtf32(utf32);
          }
          else
          {
            if (input.Length != 3)
            {
              Builtins.LexicalError("unknown escape sequence", input);
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
