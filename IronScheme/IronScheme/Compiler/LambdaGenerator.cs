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

      List<Statement> stmts = new List<Statement>();
      FillBody(cb, stmts, body, true);

      Expression ex = MakeClosure(cb, isrest);

      ClrGenerator.ResetReferences(refs);

      return ex;
    }
  }
}
