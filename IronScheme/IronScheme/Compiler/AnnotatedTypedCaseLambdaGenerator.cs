#region License
/* Copyright (c) 2007-2016 Llewellyn Pritchard
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
  [Generator("annotated-typed-case-lambda")]
  sealed class AnnotatedTypedCaseLambdaGenerator : TypedCaseLambdaGenerator
  {
    public override Expression Generate(object args, CodeBlock c)
    {
      Cons a = (Cons)args;
      object an = a.car;
      if (an is Annotation)
      {
        var anno = (Annotation)an;

        if (anno.source is Cons)
        {
          Cons src = anno.source as Cons;
          string filename = src.car as string;
          object location = src.cdr;

          Cons expr = anno.expression as Cons;

          annotations = expr == null ? null : expr.cdr as Cons;

          // bootstrap check
          if (location is string)
          {
            SpanHint = ExtractLocation(location as string);
          }
          else if (location is SourceSpan)
          {
            SpanHint = (SourceSpan)location;
          }

          if (c.Filename == null)
          {
            c.Filename = filename;
          }

          LocationHint = filename;

          return base.Generate(a.cdr, c);
        }
      }
      LocationHint = null;
      SpanHint = SourceSpan.None;
      return base.Generate(a.cdr, c);
    }


  }
}
