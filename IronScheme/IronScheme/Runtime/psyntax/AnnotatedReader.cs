using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting;
using System.IO;

namespace IronScheme.Runtime.psyntax
{
  public class AnnotatedReader : Builtins
  {
    [Serializable]
    internal sealed class Annotation
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
    }

    [Builtin("read-annotated")]
    public static object ReadAnnotated()
    {
      return ReadAnnotated(Builtins.CurrentInputPort());
    }

    static SourceSpan lastloc;

    internal static object Annotate(object obj)
    {
      if (obj is Cons)
      {
        Cons c = (Cons)obj;

        SourceSpan loc;
        if (Compiler.Parser.sourcemap.TryGetValue(c, out loc) && loc.IsValid)
        {
          lastloc = loc;
          return new Annotation(new Cons(Annotate(c.car), Annotate(c.cdr)), new Cons(filename, loc.ToString()), obj);
        }
        else
        {
          return new Cons(Annotate(c.car), Annotate(c.cdr));
        }
      }
      else if (obj != null)
      {
        SourceSpan loc;
        if (Compiler.Parser.sourcemap.TryGetValue(obj, out loc) && loc.IsValid)
        {
          lastloc = loc;
          return new Annotation(obj, new Cons(filename, loc.ToString()), obj);
        }
      }
      if (obj is SymbolId && lastloc.IsValid)
      {
        return new Annotation(obj, new Cons(filename, lastloc.ToString()), obj);
      }
      return obj;
    }

    static string filename;

    [Builtin("read-annotated")]
    public static object ReadAnnotated(object port)
    {
      object expr = Read(port);
      if (port is StreamReader)
      {
        StreamReader r = (StreamReader)port;
        if (r.BaseStream is FileStream)
        {
          FileStream fs = (FileStream)r.BaseStream;
          filename =  fs.Name.Replace(Environment.CurrentDirectory + "\\", string.Empty);
        }
        expr = Annotate(expr);
      }
      
      return expr;
    }

    [Builtin("annotation?")]
    public static object IsAnnotation(object obj)
    {
      return GetBool( obj is Annotation);
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
