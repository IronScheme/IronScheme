#region License
/* Copyright (c) 2007,2008,2009,2010,2011 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System.Collections.Generic;
using Microsoft.Scripting.Ast;
using System;
using Microsoft.Scripting;

namespace IronScheme.Compiler
{
  static partial class Optimizer
  {

    class RemoveTemporaries : OptimizerBase
    {
      readonly Dictionary<Variable, int> references = new Dictionary<Variable, int>();

      static bool CanAlias(Variable here, Variable there)
      {
        var sameblock = here.Block == there.Block;
        var assigned = Generator.assigns.ContainsKey(there.Name);
        return here.Type == there.Type && sameblock && !assigned && !there.Lift;
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

        //protected override bool Walk(BoundExpression node)
        //{
        //  BoundExpression e = node;
        //  Dictionary<Variable, bool> loopcheck = new Dictionary<Variable, bool>();
        //  while (e.Variable.AssumedValue is BoundExpression)
        //  {
        //    var be = e.Variable.AssumedValue as BoundExpression;
        //    if (loopcheck.ContainsKey(e.Variable))
        //    {
        //      e = node;
        //      break;
        //    }
        //    loopcheck.Add(e.Variable, true);

        //    if (!CanAlias(node.Variable, be.Variable))
        //    {
        //      e = node;
        //      break;
        //    }

        //    if (e.Variable.Lift)
        //    {
        //      break;
        //    }

        //    e = be;

        //    break;
        //  }
        //  node.Variable = e.Variable;

        //  return base.Walk(node);
        //}
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
                  newbody.Add(nstmt);
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
                if (me.Member == Compiler.Generator.Unspecified && !ScriptDomainManager.Options.LightweightDebugging)
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
        }

        protected override void PostWalk(VoidExpression node)
        {
          node.Statement = Rewrite(node.Statement);
        }

        protected override void PostWalk(IfStatement node)
        {
          node.Tests[0].Body = Rewrite(node.Tests[0].Body);
          if (node.ElseStatement != null)
          {
            node.ElseStatement = Rewrite(node.ElseStatement);
          }
        }
      }

      public override void Optimize()
      {
        Pass0 p0 = new Pass0();
        p0.WalkNode(Root);
        //Pass1 p1 = new Pass1();
        //p1.WalkNode(Root);
        Pass2 p2 = new Pass2(references);
        p2.WalkNode(Root);
        Pass3 p3 = new Pass3(references);
        p3.WalkNode(Root);

      }
    }
  }
}
