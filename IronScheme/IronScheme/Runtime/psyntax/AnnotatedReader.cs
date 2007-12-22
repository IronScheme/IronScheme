using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting;

namespace IronScheme.Runtime.psyntax
{
  public class AnnotatedReader : Builtins
  {
    sealed class Annotation
    {
      public readonly object expression;
      public readonly object source;
      public readonly object stripped;

      public Annotation(object expression, object source, object stripped)
      {
        this.expression = expression;
        this.source = source;
        this.stripped = stripped;
      }

      public override string ToString()
      {
        return string.Format("#[annotation {0} {1} {2}]", expression, source, stripped);
      }
    }

    [Builtin("read-annotated")]
    public static object ReadAnnotated()
    {
      return ReadAnnotated(CurrentInputPort());
    }

    static object Annotate(object obj)
    {
      if (obj is Cons)
      {
        Cons c = (Cons)obj;
        Cons h = null, head = null;

        while (c != null)
        {
          Cons a = new Cons(Annotate(c.car));
          if (head == null)
          {
            head = h = a;
          }
          else
          {
            h.cdr = a;
            h = a;
          }
          if (c.cdr == null || c.cdr is Cons)
          {
            c = c.cdr as Cons;
          }
          else
          {
            c.cdr = Annotate(c.cdr);
            break;
          }
        }

        SourceSpan loc;
        if (Compiler.Parser.sourcemap.TryGetValue((Cons)obj, out loc))
        {
          return new Annotation(head, loc, obj);
        }
        else
        {
          return head;
        }
      }
      return obj;
    }

    [Builtin("read-annotated")]
    public static object ReadAnnotated(object port)
    {
      object expr = Read(port);
      //expr = Annotate(expr);
      return expr;
    }

    [Builtin("annotation?")]
    public static object IsAnnotation(object obj)
    {
      return obj is Annotation;
    }

    [Builtin("annotation-expression")]
    public static object AnnotationExpression(object obj)
    {
      return RequiresNotNull<Annotation>(obj).expression;
    }

    [Builtin("annotation-source")]
    public static object AnnotationSource(object obj)
    {
      return RequiresNotNull<Annotation>(obj).source;
    }

    [Builtin("annotation-stripped")]
    public static object AnnotationStripped(object obj)
    {
      return RequiresNotNull<Annotation>(obj).stripped;
    }


  }
}
