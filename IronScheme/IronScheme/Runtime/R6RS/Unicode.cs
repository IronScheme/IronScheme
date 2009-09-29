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

using System.Text;
using System.Collections;

namespace IronScheme.Runtime.R6RS
{
  public class Unicode : Builtins
  {
    [Builtin("string-downcase")]
    public static object ToLowerCaseString(object obj)
    {
      string s = RequiresNotNull<string>(obj);
      StringBuilder sb = new StringBuilder(s.ToLower());
      for (int i = 0; i < sb.Length; i++)
      {
        if (sb[i] == '\x03C3')
        {
          if (i == sb.Length - 1)
          {
            if (i != 0 && !char.IsSeparator(sb[i - 1]))
            {
              sb[i] = '\x03C2';
            }
          }
          else if (char.IsSeparator(sb[i + 1]))
          {
            sb[i] = '\x03C2';
          }
        }
      }
      return sb.ToString();
    }
  }
}

