using System;
using System.Collections.Generic;
using System.Text;

namespace IronScheme.Compiler
{
  static class Helper
  {
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
