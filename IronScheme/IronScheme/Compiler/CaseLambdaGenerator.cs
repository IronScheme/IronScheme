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
using Microsoft.Scripting.Ast;
using IronScheme.Runtime;
using Microsoft.Scripting;
using IronScheme.Runtime.psyntax;
using System.Text.RegularExpressions;

namespace IronScheme.Compiler
{
  [Generator("annotated-case-lambda")]
  public sealed class AnnotatedCaseLambdaGenerator : CaseLambdaGenerator
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

  [Generator("case-lambda")]
  public class CaseLambdaGenerator : SimpleGenerator
  {
    protected Cons annotations;
    LambdaGenerator lambdagen;
    public override Expression Generate(object args, CodeBlock c)
    {
      Cons lambdas = args as Cons;

      int arlen = lambdas.Length;

      if (arlen == 1)
      {
        if (lambdagen == null)
        {
          lambdagen = Context.Scope.LookupName(SymbolTable.StringToId("lambda")) as LambdaGenerator;
        }
        return lambdagen.Generate(lambdas.car, c);
      }
      else
      {
        List<CodeBlockDescriptor> cbs = new List<CodeBlockDescriptor>();
        Cons annotations = this.annotations;
        this.annotations = null;

        string lambdaname = GetLambdaName(c);

        NameHint = SymbolId.Empty;

        var sh = SpanHint;
        var lh = LocationHint;
        Annotation ann = null;

        while (lambdas != null)
        {
          object actual = lambdas.car;
          if (annotations != null)
          {
            ann = annotations.car as Annotation;

            if (ann != null)
            {
              var h = (Cons)ann.source;

              if (h.cdr is string)
              {
                sh = ExtractLocation(((Cons)ann.source).cdr as string);
              }
              else if (h.cdr is SourceSpan)
              {
                sh = (SourceSpan)h.cdr;
              }
            }
          }

          var refs = ClrGenerator.SaveReferences();

          CodeBlock cb = Ast.CodeBlock(sh, lambdaname);
          cb.Filename = lh;
          cb.Parent = c;

          object arg = Builtins.First(actual);
          Cons body = Builtins.Cdr(actual) as Cons;

          bool isrest = AssignParameters(cb, arg);

          List<Statement> stmts = new List<Statement>();
          FillBody(cb, stmts, body, true);

          CodeBlockDescriptor cbd = new CodeBlockDescriptor();
          cbd.arity = isrest ? -cb.ParameterCount : cb.ParameterCount;
          cbd.codeblock = Ast.CodeBlockExpression(cb, false);
          cbd.varargs = isrest;

          descriptorshack.Add(cbd.codeblock, cbd);

          cbs.Add(cbd);
          if (annotations != null)
          {
            annotations = annotations.cdr as Cons;
          }
          lambdas = lambdas.cdr as Cons;

          ClrGenerator.ResetReferences(refs);
        }

        return MakeCaseClosure(lambdaname, cbs);
      }
    }
  }
}
