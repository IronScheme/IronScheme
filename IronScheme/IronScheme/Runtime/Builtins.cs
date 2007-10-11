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
using Microsoft.Scripting;
using System.Diagnostics;
using System.Reflection;
using System.Collections;
using Microsoft.Scripting.Hosting;
using System.ComponentModel;
using Microsoft.Scripting.Utils;

namespace IronScheme.Runtime
{
  [AttributeUsage(AttributeTargets.Method, AllowMultiple=true)]
  public sealed class BuiltinAttribute : Attribute
  {
    string name;

    public string Name
    {
      get { return name; }
      set {name = value;}
    }

    public BuiltinAttribute()
    {

    }

    public BuiltinAttribute(string name)
    {
      this.name = name;
    }
  }

  public static partial class Builtins
  {
    public static bool IsTrue(object arg)
    {
      if (arg is bool)
      {
        return (bool)arg;
      }

      return true;
    }

    static readonly object Unspecified = null;


    #region array



    

    //[Builtin]
    //public static IEnumerable toarray(IEnumerable e)
    //{
    //  if (e is Array)
    //  {
    //    return e;
    //  }
    //  ArrayList list = e as ArrayList;
    //  if (list == null)
    //  {
    //    list = new ArrayList();
    //    foreach (object var in e)
    //    {
    //      list.Add(var);
    //    }
    //  }
    //  return list.ToArray();
    //}

    //[Builtin]
    //public static IEnumerable tolist(IEnumerable e)
    //{
    //  if (e is IList)
    //  {
    //    return e;
    //  }
    //  ArrayList list = e as ArrayList;
    //  if (list == null)
    //  {
    //    list = new ArrayList();
    //    foreach (object var in e)
    //    {
    //      list.Add(var);
    //    }
    //  }
    //  return list;
    //}



    #endregion

    public static Cons ToImproper(Cons c)
    {
      Cons i = c;
      Cons j = null;
      while (i.Cdr != null)
      {
        j = i;
        i = i.Cdr as Cons;
        if (i == null)
        {
          return c; // improper already
        }
      }

      j.Replacd(i.Car);
      return c;
    }

    public static Cons MakeImproperList(params object[] list)
    {
      if (list.Length < 2)
      {
        throw new ArgumentException("needs at least 2 args");
      }
      if (list == null)
      {
        return null;
      }
      if (list.Length == 2)
      {
        return new Cons(list[0], list[1]);
      }
      else
      {
        Cons c = new Cons(list[0], MakeImproperList(ArrayUtils.RemoveFirst(list)));
        return c;
      }
    }

    [Builtin]
    public static Type @typeof(object o)
    {
      if (o == null)
      {
        return null;
      }
      return o.GetType();
    }

    #region console


    [Builtin]
    public static object prl(object obj1)
    {
      return prl(new object[] { obj1 });
    }

    [Builtin]
    public static object prl(object obj1, object obj2)
    {
      return prl(new object[] { obj1, obj2 });
    }

    [Builtin]
    public static object prl(object obj1, object obj2, object obj3)
    {
      return prl(new object[] { obj1, obj2, obj3 });
    }

    [Builtin]
    public static object prl(params object[] args)
    {
      Debug.Assert(args != null);
      object o = null;
      foreach (object arg in args)
      {
        string s = DisplayFormat(arg);
        Console.WriteLine(s);
        o = arg;
      }
      return o;
    }

    [Builtin]
    public static object cwl(object str)
    {
      Console.WriteLine(str);
      return str as string;
    }

    [Builtin]
    public static object cwl(object format, object arg1)
    {
      string r = string.Format(format as string, arg1);
      Console.WriteLine(r);
      return r;
    }

    [Builtin]
    public static object cwl(object format, object arg1, object arg2)
    {
      string r = string.Format(format as string, arg1, arg2);
      Console.WriteLine(r);
      return r;
    }

    [Builtin]
    public static object cwl(object format, object arg1, object arg2, object arg3)
    {
      string r = string.Format(format as string, arg1, arg2, arg3);
      Console.WriteLine(r);
      return r;
    }


    [Builtin]
    public static object cwl(object format, params object[] args)
    {
      string r = string.Format(format as string, args);
      Console.WriteLine(r);
      return r;
    }

    #endregion


    static object RequiresNotNull(object obj)
    {
      if (obj == null)
      {
        throw new ArgumentNullException();
      }
      return obj;
    }

    static T Requires<T>(object obj)
    {
      if (obj != null && !(obj is T))
      {
        throw new ArgumentTypeException("Expected type '" + typeof(T).Name + "'");
      }
      if (obj == null)
      {
        return default(T);
      }
      return (T)obj;
    }

    static T RequiresNotNull<T>(object obj)
    {
      RequiresNotNull(obj);
      return Requires<T>(obj);
    }

 

  }
}
