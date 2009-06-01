﻿#region License
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
using Microsoft.Scripting.Ast;
using IronScheme.Runtime;
using IronScheme.Runtime.psyntax;
using System.Text.RegularExpressions;
using Microsoft.Scripting;

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
            
            var result = GetAst(c.cdr, cb);
            result.SetLoc(SpanHint);
            return result;
          }
          else if (location is SourceSpan)
          {
            var result = GetAst(c.cdr, cb);
            result.SetLoc((SourceSpan)location);
            return result;
          }
        }
      }

      return GetAst(c.cdr, cb);
    }
  }
}
