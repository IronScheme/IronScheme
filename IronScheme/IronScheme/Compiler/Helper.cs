using System;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using System.Globalization;

namespace IronScheme.Compiler
{
  static class Helper
  {
    static Regex unichar = new Regex(@"\\x[\da-f]+;", RegexOptions.Compiled | RegexOptions.IgnoreCase);
    static Regex escapes = new Regex(@"\\[ntr\\]", RegexOptions.Compiled | RegexOptions.IgnoreCase);

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
          case '\\':
            return "\\";
          case 'n':
            return "\n";
          case 't':
            return "\t";
          case 'r':
            return "\r";
        }
        return s;
      });

      // deal with string continuations
      string[] lines = input.Split('\n');

      List<string> fixup = new List<string>();

      for (int i = 0; i < lines.Length; i++)
      {
        if (lines[i].EndsWith("\\") && lines.Length > 1)
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
          i++;
        }
        else
        {
          fixup.Add(lines[i]);
        }
      }

      return string.Join("\n", fixup.ToArray());
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

            output = ((char)utf32).ToString();
          }
          else
          {
            output = input[2].ToString();
          }
          break;
      }

      return output;
    }
  }
}
