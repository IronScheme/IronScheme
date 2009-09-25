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
using Microsoft.Scripting;
using System.Diagnostics;
using System.Collections.ObjectModel;
using IronScheme.Runtime;
using Microsoft.Scripting.Utils;
using System.Reflection;

namespace IronScheme.Compiler
{
  static class Optimizer
  {
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

    class FixupPrimitives
    {
      readonly CodeBlock root;


      public FixupPrimitives(CodeBlock root)
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
        protected override bool Walk(MethodCallExpression node)
        {
          var i = node.Instance;
          while (i is UnaryExpression && i.NodeType == AstNodeType.Convert)
          {
             i = ((UnaryExpression)i).Operand;
          }

          if (i is BoundExpression)
          {
            var be = (BoundExpression)i;
            var v = be.Variable.Name;
            if (Builtins.IsTrue(Builtins.IsSymbolBound(v)))
            {
              var c = Builtins.SymbolValue(v) as BuiltinMethod;
              if (c != null)
              {
                var mb = c.Binder;

                var pars = new Expression[node.Arguments.Count];
                node.Arguments.CopyTo(pars, 0);

                Type[] types = Array.ConvertAll(pars, x => x.Type);
                MethodCandidate mc = mb.MakeBindingTarget(CallType.None, types);
                if (mc != null)
                {
                  var meth = mc.Target.Method as MethodInfo;

                  node.Method = meth;
                  node.Instance = null;
                }
              }
            }
            else
            {
              //var pars = new Expression[node.Arguments.Count];
              //node.Arguments.CopyTo(pars, 0);

              //var r = Generator.TryConvertToDirectCall(v, pars);

              //if (r != null)
              //{
              //  node.Instance = r.Instance;
              //  node.Method = r.Method;
              //}
            }
          }

          return base.Walk(node);
        }
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

          block.Add(Ast.Return(Ast.ReadField(null, Generator.Unspecified)));

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

    class RemoveTemporaries
    {
      readonly CodeBlock root;
      readonly Dictionary<Variable, int> references = new Dictionary<Variable, int>();

      static bool CanAlias(Variable here, Variable there)
      {
        var sameblock = here.Block == there.Block;
        var assigned = Generator.assigns.ContainsKey(there.Name);
        return here.Type == there.Type && sameblock && !assigned && !there.Lift;
      }

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
              if (be.Variable.Block != node.Variable.Block || Generator.assigns.ContainsKey(be.Variable.Name) || be.Type != node.Variable.Type)
              {
                return base.Walk(node);
              }
            }
            if (node.Variable.AssumedValue == null)
            {
              if (node.Value.Type == node.Variable.Type)
              {
                node.Variable.AssumedValue = node.Value;
              }
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
            var be = e.Variable.AssumedValue as BoundExpression;
            if (loopcheck.ContainsKey(e.Variable))
            {
              e = node;
              break;
            }
            loopcheck.Add(e.Variable, true);

            if (!CanAlias(node.Variable, be.Variable))
            {
              e = node;
              break;
            }

            if (e.Variable.Lift)
            {
              break;
            }

            e = be;
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
                if (nstmt is BlockStatement)
                {
                  newbody.AddRange(((BlockStatement)nstmt).Statements);
                }
                else
                {
                  newbody.Add(stmt);
                }
              }
            }
            return Ast.Block(newbody);
          }

          if (s is WriteStatement)
          {
            var ws = (WriteStatement)s;
            if (!references.ContainsKey(ws.Variable))
            {
              if (ws.Value is MemberExpression)
              {
                var me = ws.Value as MemberExpression;
                if (me.Member == Compiler.Generator.Unspecified)
                {
                  return null;
                }
              }
              if (ws.Value is BoundExpression)
              {
                return null;
              }
              return Ast.Statement(ws.Value);
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

    class ConversionCSE
    {
      readonly CodeBlock root;
      readonly Dictionary<Variable, Dictionary<Type, int>> references = new Dictionary<Variable, Dictionary<Type, int>>();

      public ConversionCSE(CodeBlock root)
      {
        this.root = root;
      }

      // count references
      class Pass0 : DeepWalker
      {
        readonly Dictionary<Variable, Dictionary<Type, int>> references;

        public Pass0(Dictionary<Variable, Dictionary<Type, int>> references)
        {
          this.references = references;
        }

        protected override bool Walk(UnaryExpression node)
        {
          if (node.NodeType == AstNodeType.Convert && node.Operand is BoundExpression)
          {
            var be = node.Operand as BoundExpression;

            if (be.Variable.Kind == Variable.VariableKind.Local && !be.Variable.Lift)
            {
              Dictionary<Type, int> counts;
              if (!references.TryGetValue(be.Variable, out counts))
              {
                references[be.Variable] = counts = new Dictionary<Type, int>();
              }

              if (counts.ContainsKey(node.Type))
              {
                counts[node.Type]++;
              }
              else
              {
                counts[node.Type] = 1;
              }
            }
          }
          return base.Walk(node);
        }

      }

      // create typed references
      class Pass1 : DeepWalker
      {
        readonly Dictionary<Variable,Dictionary<Type, int>> references;

        public Pass1(Dictionary<Variable, Dictionary<Type, int>> references)
        {
          this.references = references;
        }

        protected override bool Walk(UnaryExpression node)
        {

          return base.Walk(node);
        }
      }

      public void Optimize()
      {
        Pass0 p0 = new Pass0(references);
        p0.WalkNode(root);

        foreach (var v in references.Keys)
        {
          foreach (var t in references[v].Keys)
          {
            var c = references[v][t];

            if (t != typeof(object) && c > 1)
            {
              var n = SymbolTable.StringToId(t.Name + "___" + SymbolTable.IdToString(v.Name));
              var tv = v.Block.CreateVariable(n, Variable.VariableKind.Local, t);
              v.SetTypedVariable(t, tv);
            }
          }
          v.Block.Bind();
        }

        Pass1 p1 = new Pass1(references);
        p1.WalkNode(root);

      }
    }


    public static void Optimize(CodeBlock cb)
    {
      //Pass1 p1 = new Pass1();
      //p1.WalkNode(cb);

      new FlattenBodies(cb).Optimize();

      //if (!ScriptDomainManager.Options.DebugMode)
      {
        new RemoveTemporaries(cb).Optimize();
      }

      new FixupPrimitives(cb).Optimize();

      //new ConversionCSE(cb).Optimize();


    }
  }
}
