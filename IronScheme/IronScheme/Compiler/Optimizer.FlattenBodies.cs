﻿using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting.Ast;
using System.Collections.ObjectModel;

namespace IronScheme.Compiler
{
  static partial class Optimizer
  {
    class FlattenBodies : OptimizerBase
    {
      static Expression Unspecified = Ast.ReadField(null, Generator.Unspecified);

      public override void Optimize()
      {
        Pass0 p0 = new Pass0();
        p0.WalkNode(Root);
      }

      class Pass0 : DeepWalker
      {
        static Statement TryRewriteExpression(Expression ex)
        {
          if (ex is UnaryExpression && ex.NodeType == AstNodeType.Convert)
          {
            var ux = ex as UnaryExpression;
            return TryRewriteExpression(ux.Operand);
          }
          if (ex is VoidExpression)
          {
            VoidExpression ve = (VoidExpression)ex;
            return Rewrite(ve.Statement);
          }

          if (ex is CommaExpression)
          {
            var ce = ex as CommaExpression;
            var block = RewriteExpressions(ce.Expressions, Ast.Statement);
            return Rewrite(Ast.Block(block));
          }

          if (ex is ConditionalExpression)
          {
            var ce = ex as ConditionalExpression;

            if (ce.IfFalse == Unspecified || ce.IfFalse is MemberExpression && ((MemberExpression)ce.IfFalse).Member == Generator.Unspecified)
            {
              return Ast.If(ce.Test, Rewrite(Ast.Statement(ce.IfTrue)));
            }
            if (ce.IfTrue == Unspecified || ce.IfTrue is MemberExpression && ((MemberExpression)ce.IfTrue).Member == Generator.Unspecified)
            {
              return Ast.If(Ast.Not(ce.Test), Rewrite(Ast.Statement(ce.IfFalse)));
            }

            return Ast.If(ce.Test, Rewrite(Ast.Statement(ce.IfTrue))).Else(Rewrite(Ast.Statement(ce.IfFalse)));
          }

          return null;
        }

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

          if (body is WriteStatement)
          {
            var ws = (WriteStatement)body;

            if (ws.Value is CommaExpression)
            {
              var ce = ws.Value as CommaExpression;
              var block = RewriteExpressions(ce.Expressions, x => Ast.Write(ws.Variable, x));
              return Rewrite(Ast.Block(block));
            }
          }

          if (body is ReturnStatement)
          {
            var rs = body as ReturnStatement;

            if (rs.Expression is UnaryExpression && rs.Expression.NodeType == AstNodeType.Convert)
            {
              var ux = (UnaryExpression)rs.Expression;
              var op = ux.Operand;

              if (op is VoidExpression)
              {
                return Rewrite(op as VoidExpression);
              }
              if (op is CommaExpression)
              {
                var ce = op as CommaExpression;
                var block = RewriteExpressions(ce.Expressions,
                  x => Ast.Return(Ast.ConvertHelper(x, ux.Type)));

                return Rewrite(Ast.Block(block));
              }
            }

            if (rs.Expression is CommaExpression)
            {
              var ce = rs.Expression as CommaExpression;
              var block = RewriteExpressions(ce.Expressions, Ast.Return);
              return Rewrite(Ast.Block(block));
            }

            if (rs.Expression is VoidExpression)
            {
              var ve = rs.Expression as VoidExpression;
              return Rewrite(ve);
            }

          }

          if (body is ExpressionStatement)
          {
            var es = (ExpressionStatement)body;
            var trs = TryRewriteExpression(es.Expression);
            if (trs != null)
            {
              return trs;
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

        private static Statement Rewrite(VoidExpression ve)
        {
          var block = new List<Statement>();
          var s = Rewrite(ve.Statement);
          if (s is BlockStatement)
          {
            block.AddRange(((BlockStatement)s).Statements);
          }
          else
          {
            block.Add(s);
          }

          block.Add(Ast.Return(Unspecified));

          return Rewrite(Ast.Block(block));
        }

        private static List<Statement> RewriteExpressions(ReadOnlyCollection<Expression> cee,
          IronScheme.Runtime.Typed.Func<Expression, Statement> lastmaker)
        {
          var block = new List<Statement>();
          int i = 0;
          for (; i < cee.Count - 1; i++)
          {
            var ex = cee[i];
            var trs = TryRewriteExpression(ex);
            if (trs is BlockStatement)
            {
              block.AddRange(((BlockStatement)trs).Statements);
            }
            else if (trs == null)
            {
              block.Add(Ast.Statement(ex));
            }
            else
            {
              block.Add(trs);
            }
          }

          var last = lastmaker(cee[i]);
          block.Add(last);
          return block;
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
  }
}
