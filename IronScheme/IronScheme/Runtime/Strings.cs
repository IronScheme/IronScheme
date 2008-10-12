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
    /*

; 6.3.5
- string?
- make-string (todo)
- string 
- string-length
- string-ref
- string-set! (todo)
- string=? 

- string<? 
- string>? 
- string<=? 
- string>=? 


- substring
- string-append 

- string->list 
- list->string 

- string-copy 
- string-fill! (todo)

     */

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
      return ImplementationRestriction("string-fill!", "will not be supported, ever", obj, fill);
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

      return AssertionViolation(GetCaller(), "obj must be a StringBuilder or a String", obj);
    }

    [Builtin("make-string")]
    public static StringBuilder MakeString(object k)
    {
      return MakeString(k, (char)0);
    }

    [Builtin("make-string")]
    public static StringBuilder MakeString(object k, object fill)
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
    public static StringBuilder String(params object[] args)
    {
      char[] a = Array.ConvertAll<object, char>(args, delegate(object o) { return (char)o; });
      return new StringBuilder(new string(a));
    }

    [Builtin("substring")]
    public static string SubString(object obj, object start, object end)
    {
      int st = RequiresNotNull<int>(start);
      int ed = RequiresNotNull<int>(end);
      string s = RequiresNotNull<string>(obj);
      return s.Substring(st, ed - st);
    }

    [Builtin("string-append")]
    public static string StringAppend(params object[] args)
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
    public static string StringCopy(object obj)
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
    public static Cons StringToList(object obj)
    {
      string s = RequiresNotNull<string>(obj);

      return Runtime.Cons.FromList(s);
    }

    [Builtin("list->string")]
    public static string ListToString(object obj)
    {
      StringBuilder sb = new StringBuilder();

      Cons c = Requires<Runtime.Cons>(obj);

      while (c != null)
      {
        char k = RequiresNotNull<char>(c.car);

        sb.Append(k);

        c = c.cdr as Cons;
      }

      return sb.ToString();
    }

    [Builtin("string-for-each")]
    public static object StringForEach(object proc, params object[] lists)
    {
      int listcount = lists.Length;
      ICallable c = RequiresNotNull<ICallable>(proc);

      if (listcount > 0)
      {
        object f = lists[0];

        if (f is string)
        {
          int ol = ((string)lists[0]).Length;
          for (int i = 0; i < ol; i++)
          {
            object[] args = new object[listcount];

            for (int j = 0; j < listcount; j++)
            {
              args[j] = ((string)lists[j])[i];
            }

            c.Call(args);
          }
        }
        else
        {
          foreach (object o in lists)
          {
            c.Call(o);
          }
        }
      }
      return Unspecified;
    }


    [Builtin("string-format")]
    public static string StringFormat(object format, params object[] args)
    {
      string f = RequiresNotNull<string>(format);
      return string.Format(f, args);
    }

  }
}
