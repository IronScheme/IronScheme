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

    class DeepWalker : Walker
    {
      readonly Dictionary<CodeBlock, bool> blocks = new Dictionary<CodeBlock, bool>();

      protected override bool Walk(CodeBlock node)
      {
        blocks[node] = true;
        return base.Walk(node);
      }

      protected override void PostWalk(CodeBlockExpression node)
      {
        if (!blocks.ContainsKey(node.Block))
        {
          WalkNode(node.Block);
        }
        base.PostWalk(node);
      }
    }

    class FlattenBodies
    {
      readonly CodeBlock root;
      

      public FlattenBodies(CodeBlock root)
      {
        this.root = root;
      }

      public void Optimize()
      {
        Pass0 p0 = new Pass0();
        p0.WalkNode(root);
      }

      class Pass0 : DeepWalker
      {
        static Statement Rewrite(Statement body)
        {
          if (body is BlockStatement)
          {
            var node = body as BlockStatement;
            var newbody = new List<Statement>();

            foreach (var stmt in node.Statements)
            {
              var ns = Rewrite(stmt);
              if (ns is BlockStatement)
              {
                newbody.AddRange(((BlockStatement)ns).Statements);
              }
              else
              {
                newbody.Add(ns);
              }
            }
            return Ast.Block(newbody);
          }

          if (body is ExpressionStatement)
          {
            var es = (ExpressionStatement)body;
            if (es.Expression is VoidExpression)
            {
              VoidExpression ve = (VoidExpression)es.Expression;
              return ve.Statement;
            }
          }

          if (body is IfStatement)
          {
            var ifs = (IfStatement)body;
            if (ifs.ElseStatement == null)
            {
              return Ast.If(ifs.Tests[0].Test, Rewrite(ifs.Tests[0].Body)).ToStatement();
            }
            else
            {
              return Ast.If(ifs.Tests[0].Test, Rewrite(ifs.Tests[0].Body)).Else(Rewrite(ifs.ElseStatement));
            }
          }

          return body;
        }

        protected override void PostWalk(CodeBlock node)
        {
          node.Body = Rewrite(node.Body);
          base.PostWalk(node);
        }

        protected override void PostWalk(VoidExpression node)
        {
          node.Statement = Rewrite(node.Statement);
          base.PostWalk(node);
        }

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

      class Pass0 : DeepWalker
      {
        protected override bool Walk(WriteStatement node)
        {
          if (!Generator.assigns.ContainsKey(node.Variable.Name))
          {
            if (node.Value is BoundExpression)
            {
              var be = node.Value as BoundExpression;
              if (Generator.assigns.ContainsKey(be.Variable.Name))
              {
                return base.Walk(node);
              }
            }
            if (node.Variable.AssumedValue == null)
            {
              node.Variable.AssumedValue = node.Value;
            }
          }
          return base.Walk(node);
        }
      }

      // assign aliasing
      class Pass1 : DeepWalker
      {
        protected override bool Walk(BoundExpression node)
        {
          BoundExpression e = node;
          Dictionary<Variable, bool> loopcheck = new Dictionary<Variable, bool>();
          while (e.Variable.AssumedValue is BoundExpression)
          {
            if (loopcheck.ContainsKey(e.Variable))
            {
              e = node;
              break;
            }
            loopcheck.Add(e.Variable, true);
            if (e.Variable.Lift)
            {
              break;
            }
            e = e.Variable.AssumedValue as BoundExpression;
          }
          node.Variable = e.Variable;

          return base.Walk(node);
        }
      }

      // count references
      class Pass2 : DeepWalker
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
      class Pass3 : DeepWalker
      {
        readonly Dictionary<Variable, int> references;

        public Pass3(Dictionary<Variable, int> references)
        {
          this.references = references;
        }

        Statement Rewrite(Statement s)
        {
          if (s is BlockStatement)
          {
            var stmts = ((BlockStatement)s).Statements;
            List<Statement> newbody = new List<Statement>();

            foreach (var stmt in stmts)
            {
              var nstmt = Rewrite(stmt);
              if (nstmt != null)
              {
                newbody.Add(stmt);
              }
            }
            return Ast.Block(newbody);
          }

          if (s is WriteStatement)
          {
            var ws = (WriteStatement)s;
            if (!references.ContainsKey(ws.Variable))
            {
              return null;
            }
          }

          if (s is ExpressionStatement)
          {
            var es = (ExpressionStatement)s;
            if (es.Expression is VoidExpression)
            {
              var ve = (VoidExpression)es.Expression;
              return ve.Statement;
            }
          }

          if (s is IfStatement)
          {
            var ifs = (IfStatement)s;
            if (ifs.ElseStatement == null)
            {
              return Ast.If(ifs.Tests[0].Test, Rewrite(ifs.Tests[0].Body)).ToStatement();
            }
            else
            {
              return Ast.If(ifs.Tests[0].Test, Rewrite(ifs.Tests[0].Body)).Else(Rewrite(ifs.ElseStatement));
            }
          }

          return s;
        }

        protected override void PostWalk(CodeBlock node)
        {
          node.Body = Rewrite(node.Body);

          List<Variable> toremove = new List<Variable>();

          foreach (var v in node.Variables)
          {
            if (!references.ContainsKey(v))
            {
              toremove.Add(v);
            } 
          }

          node.RemoveVariables(toremove);

          node.Bind();
          base.PostWalk(node);
        }

        protected override void PostWalk(VoidExpression node)
        {
          node.Statement = Rewrite(node.Statement);
          base.PostWalk(node);
        }

        protected override void PostWalk(IfStatement node)
        {
          node.Tests[0].Body = Rewrite(node.Tests[0].Body);
          if (node.ElseStatement != null)
          {
            node.ElseStatement = Rewrite(node.ElseStatement);
          }


          base.PostWalk(node);
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



      new FlattenBodies(cb).Optimize();
      new RemoveTemporaries(cb).Optimize();

    }
  }
}
