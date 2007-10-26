#region License
/* ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 
 *
 * This source code is subject to terms and conditions of the Microsoft Permissive License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Permissive License.
 *
 * You must not remove this notice, or any other, from this software.
 * ***************************************************************************/
#endregion

using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting.Math;
using System.Reflection;
using Microsoft.Scripting.Utils;
using System.Collections;
using Microsoft.Scripting;

namespace IronScheme.Runtime
{
  public partial class Builtins
  {
    /*

; 6.3.5
- string?
- make-string (todo)
- string 
- string-length
- string-ref
- string-set! (todo)
- string=? 
- string-ci=? 

- string<? 
- string>? 
- string<=? 
- string>=? 
- string-ci<? 
- string-ci>? 
- string-ci<=? 
- string-ci>=? 

- substring
- string-append 

- string->list 
- list->string 

- string-copy 
- string-fill! (todo)

     */

    [Builtin("string-set!")]
    public static object StringSet(object obj, int k, object value)
    {
      StringBuilder sb = RequiresNotNull<StringBuilder>(obj);
      if (sb != null)
      {
        if (value is int)
        {
          value = (char)(int)value;
        }
        sb[k] = (char)value;
        return Unspecified;
      }
      throw new NotImplementedException();
    }

    [Builtin("string-fill!")]
    public static object StringFill(object obj, object fill)
    {
      throw new NotImplementedException();
    }

    [Builtin("string-ref")]
    public static char StringRef(object obj, int k)
    {
      StringBuilder sb = obj as StringBuilder;
      if (sb != null)
      {
        return sb[k];
      }
      string s = obj as string;
      if (s != null)
      {
        return s[k];
      }
      throw new ArgumentTypeException("obj must be a StringBuilder or a String");
    }

    [Builtin("make-string")]
    public static StringBuilder MakeString(int k)
    {
      return MakeString(k, (char)0);
    }

    [Builtin("make-string")]
    public static StringBuilder MakeString(int k, object fill)
    {
      StringBuilder sb = new StringBuilder(k);
      for (int i = 0; i < k; i++)
      {
        sb.Append((char)fill);
      }
      return sb;
    }

    [Builtin("string")]
    public static StringBuilder String(params char[] args)
    {
      return new StringBuilder(new string(args));
    }

    static string GetString(object str)
    {
      if (str is StringBuilder)
      {
        return str.ToString();
      }
      if (str is string)
      {
        return (string)str;
      }
      if (str == null)
      {
        return null;
      }
      throw new ArgumentTypeException("obj must be a StringBuilder or a String");
    }

    [Builtin("substring")]
    public static string SubString(object obj, int start, int end)
    {
      string s = GetString(obj);
      return s.Substring(start, end - start);
    }

    [Builtin("string-append")]
    public static string StringAppend(params object[] args)
    {
      return string.Concat(args);
    }

    [Builtin("string-length")]
    public static int StringLength(object obj)
    {
      string s = GetString(obj);
      return s.Length;
      
    }

    [Builtin("string-copy")]
    public static string StringCopy(object obj)
    {
      string s = GetString(obj);
      return s.Clone() as string;
    }

    [Builtin("string?")]
    public static bool IsString(object obj)
    {
      return obj is string;
    }

    [Builtin("string=?")]
    public static bool IsSameString(object obj1, object obj2)
    {
      string s1 = GetString(obj1);
      string s2 = GetString(obj2);
      return s1 == s2;
    }

    [Builtin("string<?")]
    public static bool IsLessThanString(object obj1, object obj2)
    {
      string s1 = GetString(obj1);
      string s2 = GetString(obj2);

      return IsLessThan(s1, s2);
    }

    [Builtin("string>?")]
    public static bool IsGreaterThanString(object obj1, object obj2)
    {
      string s1 = GetString(obj1);
      string s2 = GetString(obj2);

      return IsGreaterThan(s1, s2);
    }

    [Builtin("string<=?")]
    public static bool IsLessThanOrEqualString(object obj1, object obj2)
    {
      string s1 = GetString(obj1);
      string s2 = GetString(obj2);

      return IsLessThanOrEqual(s1, s2);
    }

    [Builtin("string>=?")]
    public static bool IsGreaterThanOrEqualString(object obj1, object obj2)
    {
      string s1 = GetString(obj1);
      string s2 = GetString(obj2);

      return IsGreaterThanOrEqual(s1, s2);
    }

    static string ToLower(string obj)
    {
      string s = GetString(obj);
      return s.ToLower();
    }

    [Builtin("string-ci=?")]
    public static bool IsSameStringCaseInsensitive(object obj1, object obj2)
    {
      string s1 = ToLower(GetString(obj1));
      string s2 = ToLower(GetString(obj2));

      return s1 == s2;
    }

    [Builtin("string-ci<?")]
    public static bool IsLessThanStringCaseInsensitive(object obj1, object obj2)
    {
      string s1 = ToLower(GetString(obj1));
      string s2 = ToLower(GetString(obj2));

      return IsLessThan(s1, s2);
    }

    [Builtin("string-ci>?")]
    public static bool IsGreaterThanStringCaseInsensitive(object obj1, object obj2)
    {
      string s1 = ToLower(GetString(obj1));
      string s2 = ToLower(GetString(obj2));

      return IsGreaterThan(s1,s2);
    }

    [Builtin("string-ci<=?")]
    public static bool IsLessThanOrEqualStringCaseInsensitive(object obj1, object obj2)
    {
      string s1 = ToLower(GetString(obj1));
      string s2 = ToLower(GetString(obj2));

      return IsLessThanOrEqual(s1, s2);
    }

    [Builtin("string-ci>=?")]
    public static bool IsGreaterThanOrEqualStringCaseInsensitive(object obj1, object obj2)
    {
      string s1 = ToLower(GetString(obj1));
      string s2 = ToLower(GetString(obj2));

      return IsGreaterThanOrEqual(s1, s2);
    }

    [Builtin("string->list")]
    public static Cons StringToList(object obj)
    {
      string s = GetString(obj);

      return Runtime.Cons.FromList(s.ToCharArray());
    }

    [Builtin("list->string")]
    public static string ListToString(object obj)
    {
      return new string(obj as char[]);
    }

  }
}
