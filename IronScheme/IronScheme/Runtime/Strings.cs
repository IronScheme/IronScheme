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
      throw new NotImplementedException();
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

    protected static string GetString(object str)
    {
      if (str is string)
      {
        return (string)str;
      }
      if (str is StringBuilder)
      {
        return str.ToString();
      }
      AssertionViolation(GetCaller(), "obj must be a StringBuilder or a String", str);
      return null;
    }

    [Builtin("substring")]
    public static string SubString(object obj, object start, object end)
    {
      int st = RequiresNotNull<int>(start);
      int ed = RequiresNotNull<int>(end);
      string s = GetString(obj);
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
      string s = GetString(obj);
      return s.Clone() as string;
    }

    [Builtin("string?")]
    public static object IsString(object obj)
    {
      return GetBool(obj is string);
    }

    [Builtin("string=?")]
    public static object IsSameString(object obj1, object obj2)
    {
      string s1 = GetString(obj1);
      string s2 = GetString(obj2);
      return GetBool(s1 == s2);
    }

    [Builtin("string=?")]
    public static object IsSameString(object obj1, object obj2, object obj3, params object[] rest)
    {
      string s1 = GetString(obj1);
      string s2 = GetString(obj2);
      string s3 = GetString(obj3);

      bool head = s1 == s2 && s2 == s3;

      if (head)
      {
        foreach (object s in rest)
        {
          string ss = GetString(s);
          if (ss == s3)
          {
            s3 = ss;
          }
          else
          {
            return FALSE;
          }
        }
      }
      return GetBool(head);
    }

    [Builtin("string<?")]
    public static object IsLessThanString(object obj1, object obj2)
    {
      string s1 = GetString(obj1);
      string s2 = GetString(obj2);

      return GetBool(s2.CompareTo(s1) < 0);
    }

    [Builtin("string<?")]
    public static object IsLessThanString(object obj1, object obj2, object obj3, params object[] rest)
    {
      string s1 = GetString(obj1);
      string s2 = GetString(obj2);
      string s3 = GetString(obj3);

      bool head = IsTrue(IsLessThanString(s1, s2)) && IsTrue(IsLessThanString(s2, s3));

      if (head)
      {
        foreach (object s in rest)
        {
          string ss = GetString(s);
          if (IsTrue(IsLessThanString(ss, s3)))
          {
            s3 = ss;
          }
          else
          {
            return FALSE;
          }
        }
      }
      return GetBool(head);
    }

    [Builtin("string>?")]
    public static object IsGreaterThanString(object obj1, object obj2)
    {
      string s1 = GetString(obj1);
      string s2 = GetString(obj2);

      return GetBool(s2.CompareTo(s1) > 0);
    }

    [Builtin("string>?")]
    public static object IsGreaterThanString(object obj1, object obj2, object obj3, params object[] rest)
    {
      string s1 = GetString(obj1);
      string s2 = GetString(obj2);
      string s3 = GetString(obj3);

      bool head = IsTrue(IsGreaterThanString(s1, s2)) && IsTrue(IsGreaterThanString(s2, s3));

      if (head)
      {
        foreach (object s in rest)
        {
          string ss = GetString(s);
          if (IsTrue(IsGreaterThanString(ss, s3)))
          {
            s3 = ss;
          }
          else
          {
            return FALSE;
          }
        }
      }
      return GetBool(head);
    }

    [Builtin("string<=?")]
    public static object IsLessThanOrEqualString(object obj1, object obj2)
    {
      string s1 = GetString(obj1);
      string s2 = GetString(obj2);

      return GetBool(s2.CompareTo(s1) <= 0);
    }

    [Builtin("string<=?")]
    public static object IsLessThanOrEqualString(object obj1, object obj2, object obj3, params object[] rest)
    {
      string s1 = GetString(obj1);
      string s2 = GetString(obj2);
      string s3 = GetString(obj3);

      bool head = IsTrue(IsLessThanOrEqualString(s1, s2)) && IsTrue(IsLessThanOrEqualString(s2, s3));

      if (head)
      {
        foreach (object s in rest)
        {
          string ss = GetString(s);
          if (IsTrue(IsLessThanOrEqualString(ss, s3)))
          {
            s3 = ss;
          }
          else
          {
            return FALSE;
          }
        }
      }
      return GetBool(head);
    }

    [Builtin("string>=?")]
    public static object IsGreaterThanOrEqualString(object obj1, object obj2)
    {
      string s1 = GetString(obj1);
      string s2 = GetString(obj2);

      return GetBool(s2.CompareTo(s1) >= 0);
    }

    [Builtin("string>=?")]
    public static object IsGreaterThanOrEqualString(object obj1, object obj2, object obj3, params object[] rest)
    {
      string s1 = GetString(obj1);
      string s2 = GetString(obj2);
      string s3 = GetString(obj3);

      bool head = IsTrue(IsGreaterThanOrEqualString(s1, s2)) && IsTrue(IsGreaterThanOrEqualString(s2, s3));

      if (head)
      {
        foreach (object s in rest)
        {
          string ss = GetString(s);
          if (IsTrue(IsGreaterThanOrEqualString(ss, s3)))
          {
            s3 = ss;
          }
          else
          {
            return FALSE;
          }
        }
      }
      return GetBool(head);
    }

    protected static string ToLower(object obj)
    {
      string s = GetString(obj);
      return s.ToLower();
    }

    [Builtin("string->list")]
    public static Cons StringToList(object obj)
    {
      string s = GetString(obj);

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
      string f = GetString(format);
      return string.Format(f, args);
    }

  }
}
