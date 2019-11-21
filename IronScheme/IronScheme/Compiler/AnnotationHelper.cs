#region License
/* Copyright (c) 2007-2016 Llewellyn Pritchard
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using IronScheme.Runtime;
using IronScheme.Runtime.psyntax;
using Microsoft.Scripting;

namespace IronScheme.Compiler
{
  static class AnnotationHelper
  {
    public static string Filename { get; set; }

    public static ConsAnnotation AnnotateList(Cons lst, SourceSpan loc)
    {
      Cons stripped = Strip(lst);
      return new ConsAnnotation(lst, new Cons(Filename, loc.ToString()), stripped);
    }

    public static Cons Strip(Cons lst)
    {
      Cons head = null, cur = lst;

      while (lst != null)
      {
        if (head == null)
        {
          cur = head = new Cons(Strip(lst.car));
        }
        else
        {
          cur.cdr = new Cons(Strip(lst.car));
          cur = cur.cdr as Cons;
        }

        if (lst.cdr is Cons cdr)
        {
          lst = cdr;
        }
        else
        {
          cur.cdr = Strip(lst.cdr);
          break;
        }
      }

      return head;
    }

    static object Strip(object p)
    {
      switch (p)
      {
        case Annotation ann:
          return ann.stripped;
        case Cons cons:
          return Strip(cons);
        default:
          return p;
      }
    }

    public static Annotation Annotate(object obj, SourceSpan loc)
    {
      switch (obj)
      {
        case Cons cons:
          return AnnotateList(cons, loc);
        case object[] arr:
          var stripped = Array.ConvertAll(arr, Strip);
          return new Annotation(arr, new Cons(Filename, loc.ToString()), stripped);
        default:
          return new Annotation(obj, new Cons(Filename, loc.ToString()), obj);
      }
    }
  }
}
