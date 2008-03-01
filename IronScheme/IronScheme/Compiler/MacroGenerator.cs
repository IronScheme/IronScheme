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

namespace IronScheme.Compiler
{
  [Generator("macro")]
  public sealed class MacroGenerator : SimpleGenerator
  {
    public override Expression Generate(object args, CodeBlock c)
    {
      CodeBlock cb = Ast.CodeBlock(GetFullname(NameHint, c));
      cb.Parent = c;

      object arg = Builtins.First(args);
      Cons body = Builtins.Cdr(args) as Cons;

      bool isrest = AssignParameters(cb, arg);

      List<Statement> stmts = new List<Statement>();
      FillBody(cb, stmts, body, true);

      CodeBlockExpression cbe = Ast.CodeBlockExpression(cb, false);

      Expression ex = Ast.SimpleCallHelper(
        isrest ? Macro_MakeVarArgX : Macro_Make,
        Ast.CodeContext(),
        cbe,
        Ast.Constant(cb.Parameters.Count),
        Ast.Constant(cb.Name));

      return ex;
    }
  }
}
