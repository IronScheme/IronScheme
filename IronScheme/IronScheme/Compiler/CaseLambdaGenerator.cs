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
      if (an is AnnotatedReader.Annotation)
      {
        var anno = (AnnotatedReader.Annotation)an;
        if (anno.source is Cons)
        {
          Cons src = anno.source as Cons;
          string filename = src.car as string;
          string location = src.cdr as string;

          Cons expr = anno.expression as Cons;

          annotations = expr == null ? null : expr.cdr as Cons;

          SpanHint = ExtractLocation(location);

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
    protected static SourceSpan ExtractLocation(string location)
    {
      var m = Regex.Match(location, @"\((?<startline>\d+),(?<startcol>\d+)\)\s-\s\((?<endline>\d+),(?<endcol>\d+)\)");

      return new SourceSpan(
        new SourceLocation(0, Convert.ToInt32(m.Groups["startline"].Value), Convert.ToInt32(m.Groups["startcol"].Value)),
        new SourceLocation(0, Convert.ToInt32(m.Groups["endline"].Value), Convert.ToInt32(m.Groups["endcol"].Value)));
    } 

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
        AnnotatedReader.Annotation ann = null;

        while (lambdas != null)
        {
          object actual = lambdas.car;
          if (annotations != null)
          {
            ann = annotations.car as AnnotatedReader.Annotation;
            sh = ExtractLocation(((Cons)ann.source).cdr as string);
          }
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
        }

        return MakeCaseClosure(lambdaname, cbs);
      }
    }
  }
}
