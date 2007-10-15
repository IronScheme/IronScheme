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
  public static partial class Builtins
  {
    /*
; 6.3.2
- pair? (todo)
- cons
- car
- cdr
- set-car! (need to rename)
- set-cdr! (need to rename)
- null?
- list?
- list
- length
- append
- reverse
- list-tail
- list-ref
- memq
- memv
- member
- assq
- assv
- assoc     
     */

#if EXT_LIB
    [Builtin("unfold")]
    public static object Unfold(CodeContext cc, object function, object init, object pred)
    {
      FastCallable fc = function as FastCallable;
      FastCallable p = pred as FastCallable;
      return (bool) p.Call(cc, init) ? Cons(init) : Cons(init, Unfold(cc, function, fc.Call(cc, init), pred));
    }

    [Builtin("foldl"), Builtin("reduce"), Builtin("fold")]
    public static object FoldLeft(CodeContext cc, object function, object accum, IEnumerable list)
    {
      FastCallable fc = function as FastCallable;
      return IsNull(list) ? accum : FoldLeft(cc, function, fc.Call(cc, accum, Car(list)), Cdr(list) as IEnumerable);
    }

    [Builtin("foldr")]
    public static object FoldRight(CodeContext cc, object function, object end, IEnumerable list)
    {
      FastCallable fc = function as FastCallable;
      return IsNull(list) ? end : fc.Call(cc, Car(list), FoldRight(cc, function, end, Cdr(list) as IEnumerable));
    }

#endif

    [Builtin("pair?")]
    public static bool IsPair(object arg1)
    {
      return arg1 is Cons;
    }

    [Builtin("list?")]
    public static bool IsList(object arg1)
    {
      if (arg1 is Cons && !(((Cons)arg1).Cdr is Cons))
      {
        return false;
      }
      return arg1 is Cons;
    }

    [Builtin("null?")]
    public static bool IsNull(object arg1)
    {
      return arg1 == null;
    }

    [Builtin]
    public static Cons List(object arg1)
    {
      return Runtime.Cons.FromArray(arg1);
    }

    [Builtin]
    public static Cons List(object arg1, object arg2)
    {
      return Runtime.Cons.FromArray(arg1, arg2);
    }

    [Builtin]
    public static Cons List(object arg1, object arg2, object arg3)
    {
      return Runtime.Cons.FromArray(arg1, arg2, arg3);
    }

    [Builtin]
    public static Cons List(object arg1, object arg2, object arg3, object arg4)
    {
      return Runtime.Cons.FromArray(arg1, arg2, arg3, arg4);
    }

    [Builtin]
    public static Cons List(object arg1, object arg2, object arg3, object arg4, object arg5)
    {
      return Runtime.Cons.FromArray(arg1, arg2, arg3, arg4, arg5);
    }


    [Builtin]
    public static Cons List(params object[] args)
    {
      return Runtime.Cons.FromArray(args);
    }

    [Builtin]
    public static Cons Cons(object car)
    {
      return new Cons(car);
    }

    [Builtin]
    public static Cons Cons(object car, object cdr)
    {
      return new Cons(car, cdr);
    }

    [Builtin]
    public static int Length(object args)
    {
      Requires<Cons>(args);
      if (args is string)
      {
        return ((string)args).Length;
      }
      else if (args is Array)
      {
        return ((Array)args).Length;
      }
      else if (args is ICollection)
      {
        return ((ICollection)args).Count;
      }
      else
      {
        int length = 0;
        if (args != null)
        {
          foreach (object var in args as IEnumerable)
          {
            length++;
          }
        }
        return length;
      }
    }



    [Builtin]
    public static object First(object args)
    {
      return Car(args);
    }

    [Builtin]
    public static object Second(object args)
    {
      return Cadr(args);
    }

    [Builtin]
    public static object Third(object args)
    {
      return Caddr(args);
    }

#if EXT_LIB

    [Builtin]
    public static object Last(IEnumerable args)
    {
      if (args is string)
      {
        string sa = args as string;
        return sa[sa.Length - 1];
      }
      else if (args is IList)
      {
        IList il = args as IList;
        return il[il.Count - 1];
      }
      else
      {
        object last = null;
        if (args != null)
        {
          foreach (object o in args)
          {
            last = o;
          }
        }
        return last;
      }
    }
#endif

    delegate bool Pred(object a, object b);

    static object AssocHelper(Pred pred, object obj, object list)
    {
      IEnumerable e = Requires<IEnumerable>(list);
      foreach (object o in e)
      {
        IEnumerable asslist = o as IEnumerable;
        foreach (object assvar in asslist)
        {
          if (pred(obj, assvar))
          {
            return asslist;
          }
          break; // not found
        }
      }
      return false;
    }

    [Builtin]
    public static object Assoc(object obj, object list)
    {
      return AssocHelper(IsEquivalent, obj, list);
    }

    [Builtin]
    public static object Assq(object obj, object list)
    {
      return AssocHelper(IsEqual, obj, list);
    }

    [Builtin]
    public static object Assv(object obj, object list)
    {
      return AssocHelper(IsEqualValue, obj, list);
    }

    static object MemberHelper(Pred pred, object obj, object list)
    {
      Cons c = list as Cons;
      if (c != null)
      {
        while (c != null)
        {
          if (pred(Car(c), obj))
          {
            return c;
          }
          c = Cdr(c) as Cons;
        }
        return false;
      }
      return false;
    }


    [Builtin]
    public static object Member(object obj, object list)
    {
      return MemberHelper(IsEquivalent, obj, list);
    }

    [Builtin]
    public static object Memq(object obj, object list)
    {
      return MemberHelper(IsEqual, obj, list);
    }

    [Builtin]
    public static object Memv(object obj, object list)
    {
      return MemberHelper(IsEqualValue, obj, list);
    }


    [Builtin("set-car!")]
    public static object SetCar(object list, object value)
    {
      Cons c = RequiresNotNull<Runtime.Cons>(list);
      c.SetCar(value);
      return Unspecified;
    }

    [Builtin("set-cdr!")]
    public static object SetCdr(object list, object value)
    {
      Cons c = RequiresNotNull<Runtime.Cons>(list);
      c.SetCdr(value);
      return Unspecified;
    }

    [Builtin]
    public static object Caaaar(object lst)
    {
      return Car(Car(Car(Car(lst))));
    }

    [Builtin]
    public static object Caaadr( object args)
    {
      return Car(Car(Car(Cdr(args))));
    }

    [Builtin]
    public static object Caadar (object args)
    {
      return Car(Car(Cdr(Car(args))));
    }

    [Builtin]
    public static object Caaddr (object args)
    {
      return Car(Car(Cdr(Cdr(args))));
    }

    [Builtin]
    public static object Cadaar (object args)
    {
      return Car(Cdr(Car(Car(args))));
    }

    [Builtin]
    public static object Cadadr (object args)
    {
      return Car(Cdr(Car(Cdr(args))));
    }

    [Builtin]
    public static object Caddar (object args)
    {
      return Car(Cdr(Cdr(Car(args))));
    }

    [Builtin]
    public static object Cadddr (object args)
    {
      return Car(Cdr(Cdr(Cdr(args))));
    }

    [Builtin]
    public static object Cdaaar(object args)
    {
      return Cdr(Car(Car(Car(args))));
    }

    [Builtin]
    public static object Cdaadr(object args)
    {
      return Cdr(Car(Car(Cdr(args))));
    }

    [Builtin]
    public static object Cdadar(object args)
    {
      return Cdr(Car(Cdr(Car(args))));
    }

    [Builtin]
    public static object Cdaddr(object args)
    {
      return Cdr(Car(Cdr(Cdr(args))));
    }

    [Builtin]
    public static object Cddaar(object args)
    {
      return Cdr(Cdr(Car(Car(args))));
    }

    [Builtin]
    public static object Cddadr(object args)
    {
      return Cdr(Cdr(Car(Cdr(args))));
    }

    [Builtin]
    public static object Cdddar(object args)
    {
      return Cdr(Cdr(Cdr(Car(args))));
    }

    [Builtin]
    public static object Cddddr(object args)
    {
      return Cdr(Cdr(Cdr(Cdr(args))));
    }

    [Builtin]
    public static object Caaar(object args)
    {
      return Car(Car(Car(args)));
    }

    [Builtin]
    public static object Caadr(object args)
    {
      return Car(Car(Cdr(args)));
    }

    [Builtin]
    public static object Caar(object args)
    {
      return Car(Car(args));
    }

    [Builtin]
    public static object Cadar(object args)
    {
      return Car(Cdr(Car(args)));
    }

    [Builtin]
    public static object Caddr(object args)
    {
      return Car(Cdr(Cdr(args)));
    }

    [Builtin]
    public static object Cadr(object args)
    {
      return Car(Cdr(args));
    }

    [Builtin]
    public static object Cdaar(object args)
    {
      return Cdr(Car(Car(args)));
    }

    [Builtin]
    public static object Cdadr(object args)
    {
      return Cdr(Car(Cdr(args)));
    }

    [Builtin]
    public static object Cdar(object args)
    {
      return Cdr(Car(args));
    }

    [Builtin]
    public static object Cddar(object args)
    {
      return Cdr(Cdr(Car(args)));
    }

    [Builtin]
    public static object Cdddr(object args)
    {
      return Cdr(Cdr(Cdr(args)));
    }

    [Builtin]
    public static object Cddr(object args)
    {
      return Cdr(Cdr(args));
    }

    [Builtin]
    public static Runtime.Cons Rest(object args)
    {
      Requires<Runtime.Cons>(args);
      return Cdr(args) as Runtime.Cons;
    }


    [Builtin]
    public static object Car(object args)
    {
      Requires<Runtime.Cons>(args);
      if (args == null)
      {
        return null;
      }
      if (args is Cons)
      {
        return ((Cons)args).Car;
      }
      return null;
    }

    [Builtin]
    public static object Cdr(object args)
    {
      Requires<Runtime.Cons>(args);
      if (args == null)
      {
        return null;
      }
      if (args is Cons)
      {
        return ((Cons)args).Cdr;
      }
      return null;
    }

    //[Builtin]
    //public static IEnumerable Reverse(object lst)
    //{
    //  IEnumerable list = Requires<IEnumerable>(lst);
    //  if (list is Cons)
    //  {
    //    return ((Cons)list).Reverse();
    //  }
    //  else
    //  {
    //    ArrayList rev = new ArrayList();
    //    foreach (object var in list)
    //    {
    //      rev.Add(var);
    //    }
    //    rev.Reverse();
    //    return rev;
    //  }
    //}




 

    //[Builtin("list-ref")]
    //public static object ListRef(object lst, object index)
    //{
    //  IEnumerable list =  RequiresNotNull<IEnumerable>(lst);
    //  int i =             RequiresNotNull<int>(index);

    //  if (list is IList)
    //  {
    //    return ((IList)list)[i];
    //  }
    //  else
    //  {
    //    int j = 0;
    //    foreach (object o in list)
    //    {
    //      if (j == i)
    //      {
    //        return o;
    //      }
    //      j++;
    //    }
    //    throw new IndexOutOfRangeException();
    //  }
    //}

    ////The resulting list is always newly allocated, except that it shares structure with the last list argument. 
    ////The last argument may actually be any object; an improper list results if the last argument is not a proper list. 
    //[Builtin]
    //public static IEnumerable Append(params object[] args)
    //{
    //  if (args.Length == 0)
    //  {
    //    return null;
    //  }
    //  string s = args[0] as string;
    //  if (s != null)
    //  {
    //    StringBuilder sb = new StringBuilder(s);

    //    for (int i = 1; i < args.Length; i++)
    //    {
    //      sb.Append(args[i] as string);
    //    }

    //    return sb.ToString();
    //  }

    //  List<object> all = new List<object>();

    //  for (int i = 0; i < args.Length; i++)
    //  {
    //    IEnumerable ii = args[i] as IEnumerable;
    //    if (ii == null && i == args.Length - 1 && args[i] != null)
    //    {
    //      all.Add(args[i]);
    //    }
    //    else
    //    {
    //      if (ii != null)
    //      {
    //        foreach (object o in ii)
    //        {
    //          all.Add(o);
    //        }
    //      }
    //    }
    //  }
    //  return Runtime.Cons.FromList(all);
    //}
  }
}
