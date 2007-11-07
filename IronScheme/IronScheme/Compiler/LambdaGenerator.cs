using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting.Ast;
using IronScheme.Runtime;

namespace IronScheme.Compiler
{
  [Generator]
  class LambdaGenerator : SimpleGenerator
  {
    public override Expression Generate(object args, CodeBlock c)
    {
      CodeBlock cb = Ast.CodeBlock(SpanHint, GetLambdaName(c));
      cb.Parent = c;

      object arg = Builtins.First(args);
      Cons body = Builtins.Cdr(args) as Cons;

      bool isrest = AssignParameters(cb, arg);

      List<Statement> stmts = new List<Statement>();
      FillBody(cb, stmts, body, true);

      Expression ex = MakeClosure(cb, isrest);
      return ex;
    }
  }
}
