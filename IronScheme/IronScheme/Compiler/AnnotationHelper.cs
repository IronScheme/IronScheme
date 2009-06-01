using System;
using System.Collections.Generic;
using System.Text;
using IronScheme.Runtime.psyntax;
using Microsoft.Scripting;
using IronScheme.Runtime;

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

        if (lst.cdr is Cons)
        {
          lst = lst.cdr as Cons;
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
      if (p is Annotation)
      {
        var ann = (Annotation)p;
        return ann.stripped;
      }
      if (p is Cons)
      {
        return Strip((Cons)p);
      }
      return p;
    }

    public static Annotation Annotate(object obj, SourceSpan loc)
    {
      if (obj is Cons)
      {
        return AnnotateList(obj as Cons, loc);
      }
      else if (obj is object[])
      {
        var arr = (object[])obj;
        var stripped = Array.ConvertAll(arr, x => Strip(x));
        return new Annotation(obj, new Cons(Filename, loc.ToString()), stripped);
      }
      else
      {
        return new Annotation(obj, new Cons(Filename, loc.ToString()), obj);
      }
    }
  }
}
