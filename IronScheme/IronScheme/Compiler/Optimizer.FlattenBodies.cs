#region License
/* Copyright (c) 2007,2008,2009,2010,2011 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System.Collections.Generic;
using System.Collections.ObjectModel;
using Microsoft.Scripting.Ast;
using IronScheme.Runtime;
using System;

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
          //if (ex is BoundAssignment)
          //{
          //  var ba = (BoundAssignment)ex;
          //  return Ast.Write(ba.Variable, ba.Value);
          //}
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
          if (body is LabeledStatement)
          {
            var ls = (LabeledStatement)body;
            return ls.Mark(Rewrite(ls.Statement));
          }

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
            else if (ws.Value == null)
            {
              var cb = ws.Variable.Block;
              if (cb != null)
              {
                cb.RemoveVariables(new List<Variable>(new[] { ws.Variable }));
                cb.Bind();
              }
              return Ast.Empty();
            }
            else if (ws.Variable.Block == null)
            {
              //var cb = ws.Variable.Block;
              //cb.RemoveVariables(new List<Variable>(new[] { ws.Variable }));
              //cb.Bind();
              return Ast.Empty();
            }
            else if (ws.HasNoRef)
            {
              //var cb = ws.Variable.Block;
              //cb.RemoveVariables(new List<Variable>(new[] { ws.Variable }));
              //cb.Bind();
              //return Ast.Empty();
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
              var le = ce.Expressions[ce.Expressions.Count - 1];
              if (le is VoidExpression && ((VoidExpression)le).Statement is ContinueStatement)
              {
                var block = RewriteExpressions(ce.Expressions, x => Ast.Continue());
                return Rewrite(Ast.Block(block));
              }
              else
              {
                var block = RewriteExpressions(ce.Expressions, Ast.Return);
                return Rewrite(Ast.Block(block));
              }
            }

            if (rs.Expression is VoidExpression)
            {
              var ve = rs.Expression as VoidExpression;
              return Rewrite(ve);
            }

            if (rs.Expression is MethodCallExpression)
            {
              var mce = rs.Expression as MethodCallExpression;
              var uce = Unwrap(mce.Instance);
              if (uce is CommaExpression)
              {
                var ce = uce as CommaExpression;
                var block = RewriteExpressions(ce.Expressions, 
                  x => 
                    {
                      if (mce.Arguments.Count == 1 && mce.Arguments[0].Type == typeof(object[]))
                      {
                        var mc = Ast.SimpleCallHelper(x, mce.Method, mce.Arguments.ToArray());
                        mc.TailCall = mce.TailCall;
                        return Ast.Return(mc);
                      }
                      else
                      {
                        //var args = Array.ConvertAll(mce.Arguments.ToArray(), y => Ast.ConvertHelper(y, typeof(object)));
                        var mc = Ast.SimpleCallHelper(x, mce.Method, mce.Arguments.ToArray());
                        mc.TailCall = mce.TailCall;
                        return Ast.Return(mc);
                      }
                    });
                return Rewrite(Ast.Block(block));
              }
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

        protected internal static Expression Unwrap(Expression ex)
        {
          while (ex is UnaryExpression && ex.NodeType == AstNodeType.Convert)
          {
            ex = ((UnaryExpression)ex).Operand;
          }

          return ex;
        }

        static Statement Rewrite(VoidExpression ve)
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

        static List<Statement> RewriteExpressions(ReadOnlyCollection<Expression> cee,
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
