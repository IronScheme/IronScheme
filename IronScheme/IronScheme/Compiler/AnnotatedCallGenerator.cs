#region License
/* Copyright (c) 2007-2013 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using IronScheme.Runtime;
using IronScheme.Runtime.psyntax;
using Microsoft.Scripting;
using Microsoft.Scripting.Ast;

namespace IronScheme.Compiler
{
  [Generator("annotated-call")]
  public sealed class AnnotatedCallGenerator : SimpleGenerator
  {
    public override Expression Generate(object args, CodeBlock cb)
    {
      Cons c = (Cons)args;

      var an = c.car;

      if (an is Annotation)
      {
        var anno = (Annotation)an;
        if (anno.source is Cons)
        {
          Cons src = anno.source as Cons;
          string filename = src.car as string;
          object location = src.cdr;

          //LocationHint = filename;
          if (cb.Filename == null)
          {
            cb.Filename = filename;
          }

          if (location is string)
          {
            var SpanHint = ExtractLocation(location as string);
            return Apply(cb, c, SpanHint);
          }
          else if (location is SourceSpan)
          {
            return Apply(cb, c, (SourceSpan)location);
          }
        }
      }

      return GetAst(c.cdr, cb);
    }

    static Expression Apply(CodeBlock cb, Cons c, SourceSpan SpanHint)
    {
      var result = GetAst(c.cdr, cb);

      if (!result.Span.IsValid)
      {
        var ve = result as VoidExpression;
        if (ve != null && ve.Statement is WriteStatement)
        {
          ve.Statement.SetLoc(SpanHint);
        }
        else
        {
          result.SetLoc(SpanHint);
        }
      }
      return result;
    }
  }
}
