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

      protected override bool Walk(MethodCallExpression node)
      {
        return base.Walk(node);
      }

      protected override void PostWalk(MethodCallExpression node)
      {
        base.PostWalk(node);
      }
    }

    class RemoveTemporaries
    {
      readonly CodeBlock root;
      readonly Dictionary<Variable, int> references = new Dictionary<Variable, int>();

      public RemoveTemporaries(CodeBlock root)
      {
        this.root = root;
      }

      class Pass0 : Walker
      {
        protected override bool Walk(WriteStatement node)
        {
          if (!Generator.assigns.ContainsKey(node.Variable.Name))
          {
            node.Variable.AssumedValue = node.Value;
          }
          return base.Walk(node);
        }
      }

      // assign aliasing
      class Pass1 : Walker
      {
        protected override bool Walk(BoundExpression node)
        {
          BoundExpression e = node;
          while (e.Variable.AssumedValue is BoundExpression)
          {
            e = e.Variable.AssumedValue as BoundExpression;
          }
          node.Variable = e.Variable;

          return base.Walk(node);
        }
      }

      // count references
      class Pass2 : Walker
      {
        readonly Dictionary<Variable, int> references;

        public Pass2(Dictionary<Variable, int> references)
        {
          this.references = references;
        }

        protected override bool Walk(BoundExpression node)
        {
          if (references.ContainsKey(node.Variable))
          {
            references[node.Variable]++;
          }
          else
          {
            references[node.Variable] = 1;
          }
          return base.Walk(node);
        }
      }

      // removed unreferenced
      class Pass3 : Walker
      {
        readonly Dictionary<Variable, int> references;

        public Pass3(Dictionary<Variable, int> references)
        {
          this.references = references;
        }

        protected override bool Walk(CodeBlock node)
        {
          List<Variable> vars = new List<Variable>(node.Variables);

          foreach (Variable var in vars)
          {
            if (!references.ContainsKey(var))
            {
              //remove
              ; ;
            }
          }

          return base.Walk(node);
        }

      }

      public void Optimize()
      {
        Pass0 p0 = new Pass0();
        p0.WalkNode(root);
        Pass1 p1 = new Pass1();
        p1.WalkNode(root);
        Pass2 p2 = new Pass2(references);
        p2.WalkNode(root);
        Pass3 p3 = new Pass3(references);
        p3.WalkNode(root);

      }


    }

    public static void Optimize(CodeBlock cb)
    {
      //Pass1 p1 = new Pass1();
      //p1.WalkNode(cb);

      //new RemoveTemporaries(cb).Optimize();

    }
  }
}
