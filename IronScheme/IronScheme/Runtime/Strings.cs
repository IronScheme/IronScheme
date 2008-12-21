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
using System.Reflection;
using Microsoft.Scripting.Utils;
using System.Collections;
using Microsoft.Scripting;
using System.Globalization;
using System.Runtime.CompilerServices;

namespace IronScheme.Runtime
{

  public partial class Builtins
  {
    [Builtin("string-set!")]
    public static object StringSet(object obj, object k, object value)
    {
      int i = RequiresNotNull<int>(k);
      StringBuilder sb = RequiresNotNull<StringBuilder>(obj);
      if (sb != null)
      {
        if (value is int)
        {
          value = (char)(int)value;
        }
        sb[i] = (char)value;
        return Unspecified;
      }
      return AssertionViolation("string-set!", "not a mutable string", obj);
    }

    [Builtin("string-fill!")]
    public static object StringFill(object obj, object fill)
    {
      StringBuilder sb = RequiresNotNull<StringBuilder>(obj);
      for (int i = 0; i < sb.Length; i++)
      {
        sb[i] = (char)fill;
      }
      return Unspecified;
    }

    [Builtin("string-ref")]
    public static object StringRef(object obj, object k)
    {
      int i = RequiresNotNull<int>(k);
      string s = obj as string;
      if (s != null)
      {
        return s[i];
      }

      StringBuilder sb = obj as StringBuilder;
      if (sb != null)
      {
        return sb[i];
      }

      return AssertionViolation(GetCaller(), "not a string", obj);
    }

    [Builtin("make-string")]
    public static object MakeString(object k)
    {
      return MakeString(k, (char)0);
    }

    [Builtin("make-string")]
    public static object MakeString(object k, object fill)
    {
      int n = RequiresNotNull<int>(k);
      StringBuilder sb = new StringBuilder(n);
      for (int i = 0; i < n; i++)
      {
        sb.Append(RequiresNotNull<char>(fill));
      }
      return sb;
    }

    [Builtin("string")]
    public static object String(params object[] args)
    {
      char[] a = Array.ConvertAll<object, char>(args, delegate(object o) { return RequiresNotNull<char>(o); });
      return new StringBuilder(new string(a));
    }

    [Builtin("substring")]
    public static object SubString(object obj, object start, object end)
    {
      int st = RequiresNotNull<int>(start);
      int ed = RequiresNotNull<int>(end);
      string s = RequiresNotNull<string>(obj);
      return s.Substring(st, ed - st);
    }

    [Builtin("string-append")]
    public static object StringAppend(params object[] args)
    {
      return string.Concat(args);
    }

    [Builtin("string-length")]
    public static object StringLength(object obj)
    {
      if (obj is string)
      {
        return ((string)obj).Length;
      }
      else
      {
        return RequiresNotNull<StringBuilder>(obj).Length;
      }
    }

    [Builtin("string-copy")]
    public static object StringCopy(object obj)
    {
      string s = RequiresNotNull<string>(obj);
      return s.Clone() as string;
    }

    [Builtin("string?")]
    public static object IsString(object obj)
    {
      return GetBool(obj is string || obj is StringBuilder);
    }

    [Builtin("string-compare")]
    public static object StringCompare(object obj1, object obj2)
    {
      string s1 = RequiresNotNull<string>(obj1);
      string s2 = RequiresNotNull<string>(obj2);

      return string.Compare(s1, s2, StringComparison.Ordinal);
    }

    [Builtin("string->list")]
    public static object StringToList(object obj)
    {
      string s = RequiresNotNull<string>(obj);

      return Runtime.Cons.FromList(s);
    }

    [Builtin("string-format")]
    public static object StringFormat(object format, params object[] args)
    {
      string f = RequiresNotNull<string>(format);
      return string.Format(f, args);
    }

  }
}
