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
      ICallable fc = function as ICallable;
      ICallable p = pred as ICallable;
      return (bool) p.Call(cc, init) ? Cons(init) : Cons(init, Unfold(cc, function, fc.Call(cc, init), pred));
    }

    [Builtin("foldl"), Builtin("reduce"), Builtin("fold")]
    public static object FoldLeft(CodeContext cc, object function, object accum, IEnumerable list)
    {
      ICallable fc = function as ICallable;
      return IsNull(list) ? accum : FoldLeft(cc, function, fc.Call(cc, accum, Car(list)), Cdr(list) as IEnumerable);
    }

    [Builtin("foldr")]
    public static object FoldRight(CodeContext cc, object function, object end, IEnumerable list)
    {
      ICallable fc = function as ICallable;
      return IsNull(list) ? end : fc.Call(cc, Car(list), FoldRight(cc, function, end, Cdr(list) as IEnumerable));
    }

#endif

    [Builtin("pair?")]
    public static object IsPair(object arg1)
    {
      return arg1 is Cons;
    }

    [Builtin("list?")]
    public static object IsList(object arg1)
    {
      if (arg1 == null)
      {
        return true;
      }
      Cons c = arg1 as Cons;
      if (c == null)
      {
        return false;
      }
      else
      {
        return c.IsProper;
      }
    }

    [Builtin("null?")]
    public static object IsNull(object arg1)
    {
      return arg1 == null;
    }

    [Builtin]
    public static Cons List(object arg1)
    {
      return new Cons(arg1);
    }

    [Builtin]
    public static Cons List(object arg1, object arg2)
    {
      return new Cons(arg1, new Cons(arg2));
    }

    [Builtin]
    public static Cons List(object arg1, object arg2, object arg3)
    {
      return new Cons(arg1, new Cons(arg2, new Cons(arg3)));
    }

    [Builtin]
    public static Cons List(object arg1, object arg2, object arg3, object arg4)
    {
      return new Cons(arg1, new Cons(arg2, new Cons(arg3, new Cons(arg4))));
    }

    [Builtin]
    public static Cons List(object arg1, object arg2, object arg3, object arg4, object arg5)
    {
      return new Cons(arg1, new Cons(arg2, new Cons(arg3, new Cons(arg4, new Cons(arg5)))));
    }


    [Builtin]
    public static Cons List(params object[] args)
    {
      return Runtime.Cons.FromArray(args);
    }

    // overload
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
    public static object Length(object args)
    {
      Cons c = Requires<Runtime.Cons>(args);
      int length = 0;

      if (c != null)
      {
        // this does add some overhead
        //RequiresCondition(c.IsProper, "must be a properlist");
        while (c != null)
        {
          length++;
          c = c.Cdr as Cons;
        }
      }

      return length;
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
    
    [Builtin]
    public static Cons Last(object args)
    {
      Cons c = args as Cons;
      while (c.Cdr is Cons)
      {
        c = c.Cdr as Cons;
      }
      return c;
    }
    
    delegate object Pred(object a, object b);

    static object AssocHelper(Pred pred, object obj, object list)
    {
      Cons e = Requires<Runtime.Cons>(list);

      if (e != null)
      {
        // this does add some overhead
        //RequiresCondition(e.IsProper, "must be a properlist");
        while (e != null)
        {
          Cons ass = e.Car as Cons;
          if ((bool)pred(obj, ass.Car))
          {
            return ass;
          }
          e = e.Cdr as Cons;
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
      // must be properlist
      Cons c = Requires<Runtime.Cons>(list);

      if (c != null)
      {
        // this does add some overhead
        //RequiresCondition(c.IsProper, "must be a properlist");
        while (c != null)
        {
          if ((bool)pred(c.Car, obj))
          {
            return c;
          }
          c = c.Cdr as Cons;
        }
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
      c.Car = value;
      return Unspecified;
    }

    [Builtin("set-cdr!")]
    public static object SetCdr(object list, object value)
    {
      Cons c = RequiresNotNull<Runtime.Cons>(list);
      c.Cdr = value;
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
      Cons c = RequiresNotNull<Runtime.Cons>(args);
      return c.Cdr as Runtime.Cons;
    }


    [Builtin]
    public static object Car(object args)
    {
      Cons c = RequiresNotNull<Runtime.Cons>(args);
      return c.Car;
    }

    [Builtin]
    public static object Cdr(object args)
    {
      Cons c = RequiresNotNull<Runtime.Cons>(args);
      return c.Cdr;
    }

    [Builtin]
    public static Cons Reverse(object lst)
    {
      Cons c = Requires<Runtime.Cons>(lst);
      Cons list = null;

      if (c != null)
      {
        // this does add some overhead
        //RequiresCondition(c.IsProper, "must be a properlist");
        while (c != null)
        {
          list = new Cons(c.Car, list);
          c = c.Cdr as Cons;
        }
      }
      return list;
    }


    [Builtin("list-tail")]
    public static object ListTail(object lst, object index)
    {
      Cons list = RequiresNotNull<Runtime.Cons>(lst);
      int i = RequiresNotNull<int>(index);
      object c = list;

      while (c != null)
      {
        if (i-- == 0)
        {
          return c;
        }

        c = ((Cons) c).Cdr;
      }

      throw new IndexOutOfRangeException();
    }




    [Builtin("list-ref")]
    public static object ListRef(object lst, object index)
    {
      Cons list = RequiresNotNull<Runtime.Cons>(lst);
      int i = RequiresNotNull<int>(index);

      while (list != null)
      {
        if (i-- == 0)
        {
          return list.Car;
        }
        list = list.Cdr as Cons;
      }

      throw new IndexOutOfRangeException();
    }

    //The resulting list is always newly allocated, except that it shares structure with the last list argument. 
    //The last argument may actually be any object; an improper list results if the last argument is not a proper list. 
    [Builtin]
    public static object Append(params object[] args)
    {
      if (args.Length == 0)
      {
        return null;
      }
      // some fast rules
      if (args.Length == 1)
      {
        return args[0];
      }

      if (args.Length == 2)
      {
        if (args[0] == null)
        {
          return args[1];
        }
      }

      bool proper = true;
 
      List<object> all = new List<object>();

      for (int i = 0; i < args.Length; i++)
      {
        Cons ii = args[i] as Cons;

        if (ii == null && i == args.Length - 1 && args[i] != null)
        {
          all.Add(args[i]);
          proper = false;
        }
        else
        {
          while (ii != null)
          {
            all.Add(ii.Car);
            if (i == args.Length - 1 && ii.Cdr != null && !(ii.Cdr is Cons))
            {
              all.Add(ii.Cdr);
              proper = false;
              break;
            }
            ii = ii.Cdr as Cons;
          }
        }
      }
      Cons c = Runtime.Cons.FromList(all);
      if (!proper)
      {
        c = ToImproper(c);
      }
      return c;
    }

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

      j.Cdr = i.Car;
      return c;
    }
  }
}
