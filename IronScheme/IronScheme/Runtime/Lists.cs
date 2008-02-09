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
using IronScheme.Compiler;
using Microsoft.Scripting.Ast;

namespace IronScheme.Runtime
{
  public static partial class BuiltinEmitters
  {

    [InlineEmitter("null?")]
    public static Expression IsNull(Expression[] values)
    {
      return Ast.Equal(values[0], Ast.Null());
    }


    [InlineEmitter("pair?")]
    public static Expression IsPair(Expression[] values)
    {
      return Ast.TypeIs(values[0], typeof(Cons));
    }
  }

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


    [Builtin("pair?")]
    public static object IsPair(object arg1)
    {
      return GetBool(arg1 is Cons);
    }

    [Builtin("list?")]
    public static object IsList(object arg1)
    {
      if (arg1 == null)
      {
        return TRUE;
      }
      Cons c = arg1 as Cons;
      if (c == null)
      {
        return FALSE;
      }
      else
      {
        return GetBool(c.IsProper);
      }
    }

    [Builtin("null?")]
    public static object IsNull(object arg1)
    {
      return GetBool(arg1 == null);
    }

    [Builtin]
    public static object List(object arg1)
    {
      return new Cons(arg1);
    }

    [Builtin]
    public static object List(object arg1, object arg2)
    {
      return new Cons(arg1, new Cons(arg2));
    }

    [Builtin]
    public static object List(object arg1, object arg2, object arg3)
    {
      return new Cons(arg1, new Cons(arg2, new Cons(arg3)));
    }

    [Builtin]
    public static object List(object arg1, object arg2, object arg3, object arg4)
    {
      return new Cons(arg1, new Cons(arg2, new Cons(arg3, new Cons(arg4))));
    }

    [Builtin]
    public static object List(object arg1, object arg2, object arg3, object arg4, object arg5)
    {
      return new Cons(arg1, new Cons(arg2, new Cons(arg3, new Cons(arg4, new Cons(arg5)))));
    }


    [Builtin]
    public static object List(params object[] args)
    {
      return Runtime.Cons.FromArray(args);
    }

    // overload
    [Builtin]
    public static object Cons(object car)
    {
      return new Cons(car);
    }

    [Builtin]
    public static object Cons(object car, object cdr)
    {
      return new Cons(car, cdr);
    }

    [Builtin]
    public static object Length(object args)
    {
      Cons c = Requires<Runtime.Cons>(args);
      int length = 0;

      while (c != null)
      {
        length++;
        c = c.cdr as Cons;
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
      return Car(Cdr(args));
    }

    [Builtin]
    public static object Third(object args)
    {
      return Car(Cdr(Cdr(args)));
    }
    
    [Builtin]
    public static object Last(object args)
    {
      Cons c = args as Cons;
      while (c.cdr is Cons)
      {
        c = c.cdr as Cons;
      }
      return c;
    }

    [Builtin("last-pair")]
    public static object LastPair(object args)
    {
      Cons c = args as Cons;
      while (c.cdr is Cons)
      {
        c = c.cdr as Cons;
      }
      return c;
    }

    [Builtin("make-list")]
    public static object MakeList(object n)
    {
      return VectorToList(MakeVector(n));

    }



    [Builtin("make-list")]
    public static object MakeList(object n, object fill)
    {
      return VectorToList(MakeVector(n, fill));
    }

    
    protected delegate object Pred(object a, object b);

    protected static object AssocHelper(Pred pred, object obj, object list)
    {
      Cons e = Requires<Runtime.Cons>(list);

      while (e != null)
      {
        Cons ass = e.car as Cons;
        if ((bool)pred(obj, ass.car))
        {
          return ass;
        }
        e = e.cdr as Cons;
      }
      return FALSE;
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

    protected static object MemberHelper(Pred pred, object obj, object list)
    {
      // must be properlist
      Cons c = Requires<Runtime.Cons>(list);

      while (c != null)
      {
        if ((bool)pred(c.car, obj))
        {
          return c;
        }
        c = c.cdr as Cons;
      }

      return FALSE;
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
      c.car = value;
      return Unspecified;
    }

    [Builtin("set-cdr!")]
    public static object SetCdr(object list, object value)
    {
      Cons c = RequiresNotNull<Runtime.Cons>(list);
      c.cdr = value;
      return Unspecified;
    }

// this isnt helping..., but they get patched anyways

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
    public static object Rest(object args)
    {
      Cons c = RequiresNotNull<Runtime.Cons>(args);
      return c.cdr;
    }


    [Builtin]
    public static object Car(object args)
    {
      Cons c = RequiresNotNull<Runtime.Cons>(args);
      return c.car;
    }

    [Builtin]
    public static object Cdr(object args)
    {
      Cons c = RequiresNotNull<Runtime.Cons>(args);
      return c.cdr;
    }

    [Builtin]
    public static object Reverse(object lst)
    {
      Cons c = Requires<Runtime.Cons>(lst);
      Cons list = null;


      while (c != null)
      {
        list = new Cons(c.car, list);
        c = c.cdr as Cons;
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

        c = ((Cons) c).cdr;
      }

      return AssertionViolation("list-tail", "index out of range", lst, index);
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
          return list.car;
        }
        list = list.cdr as Cons;
      }

      return AssertionViolation("list-ref", "index out of range", lst, index);
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
        if (args[1] == null)
        {
          return args[0];
        }
      }

      Cons head = null, h = null;

      for (int i = 0; i < args.Length - 1; i++)
      {
        Cons ii = args[i] as Cons;
        if (null == args[i])
        {
          //empty list, do nothing
        }
        else if (ii != null)
        {
          while (ii != null)
          {
            Cons cc = new Cons(ii.car);
            if (head == null)
            {
              h = head = cc;
            }
            else
            {
              h.cdr = cc;
              h = cc;
            }

            ii = ii.cdr as Cons;
          }
        }
        else
        {
          AssertionViolation(SymbolTable.StringToId("append"), "not a list", args[i]);
        }
        
      }

      object tail = args[args.Length - 1];
      h.cdr = tail;
      return head;
    }

    public static Cons ToImproper(Cons c)
    {
      Cons i = c;
      Cons j = null;

      while (i.cdr != null)
      {
        j = i;
        i = i.cdr as Cons;
        if (i == null)
        {
          return c; // improper already
        }
      }

      j.cdr = i.car;
      return c;
    }

  }
}
