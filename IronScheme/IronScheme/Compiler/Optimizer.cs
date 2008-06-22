using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting.Ast;

namespace IronScheme.Compiler
{
  static class Optimizer
  {
    class Pass1 : Walker
    {
      protected override bool Walk(CodeBlock node)
      {
        return base.Walk(node);
      }

      protected override void PostWalk(CodeBlock node)
      {
        base.PostWalk(node);
      }

      protected override bool Walk(CodeBlockExpression node)
      {
        return base.Walk(node);
      }

      protected override void PostWalk(CodeBlockExpression node)
      {
        base.PostWalk(node);
      }

      protected override bool Walk(IfStatement node)
      {
        return base.Walk(node);
      }

      protected override void PostWalk(IfStatement node)
      {
        base.PostWalk(node);
      }
    }

    public static void Optimize(CodeBlock cb)
    {
      //Pass1 p1 = new Pass1();
      //p1.WalkNode(cb);
    }
  }
}
