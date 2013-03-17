#region License
/* Copyright (c) 2007-2013 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System.Collections.Generic;
using IronScheme.Runtime;
using Microsoft.Scripting;
using Microsoft.Scripting.Ast;

namespace IronScheme.Compiler
{
  [Generator("lambda")]
  public sealed class LambdaGenerator : SimpleGenerator
  {
    public override Expression Generate(object args, CodeBlock c)
    {
      var refs = ClrGenerator.SaveReferences();

      CodeBlock cb = Ast.CodeBlock(SpanHint, GetLambdaName(c));
      NameHint = SymbolId.Empty;
      cb.Filename = LocationHint;
      cb.Parent = c;
      cb.Source = new Cons(SymbolTable.StringToObject("lambda"), args);

      object arg = Builtins.First(args);

      Cons body = Builtins.Cdr(args) as Cons;

      bool isrest = AssignParameters(cb, arg);

      cb.IsRest = isrest;


      List<Statement> stmts = new List<Statement>();
      FillBody(cb, stmts, body, true);

      Expression ex = MakeClosure(cb, isrest);

      ClrGenerator.ResetReferences(refs);

      return ex;
    }
  }
}
