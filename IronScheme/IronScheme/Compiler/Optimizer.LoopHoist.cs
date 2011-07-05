#region License
/* Copyright (c) 2007,2008,2009,2010,2011 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using Microsoft.Scripting.Ast;
using System;
using System.Collections.Generic;
using IronScheme.Runtime;
using Microsoft.Scripting;
using System.Diagnostics;

namespace IronScheme.Compiler
{
  static partial class Optimizer
  {
    class LoopHoist : OptimizerBase
    {
      public static IEnumerable<CodeBlock> Distinct(IEnumerable<CodeBlock> l)
      {
        var dic = new Dictionary<CodeBlock, bool>();

        foreach (var v in l)
        {
          if (!dic.ContainsKey(v))
          {
            dic[v] = true;
            yield return v;
          }
        }
      }

      public override void Optimize()
      {
        var fixups = new List<CodeBlock>();
        var refmap = new Dictionary<Variable, int>();
        var callsites = new Dictionary<Variable, int>();

        Pass0 p0 = new Pass0(refmap, callsites);
        p0.WalkNode(Root);

        Pass1 p1 = new Pass1(fixups, refmap, callsites);
        p1.WalkNode(Root);

        Pass2 p2 = new Pass2();
        p2.WalkNode(Root);

        foreach (var cb in Distinct(fixups))
        {
          cb.Bind();
        }

        Root.Bind();
      }

      class Pass0 : DeepWalker
      {
        Dictionary<Variable, int> refmap, callsites;

        public Pass0(Dictionary<Variable, int> refmap, Dictionary<Variable, int> callsites)
        {
          this.refmap = refmap;
          this.callsites = callsites;
        }

        static Expression Unwrap(Expression ex)
        {
          while (ex is UnaryExpression && ex.NodeType == AstNodeType.Convert)
          {
            ex = ((UnaryExpression)ex).Operand;
          }

          return ex;
        }

        protected override bool Walk(ReturnStatement node)
        {
          Variable var;
          var mce = node.Expression as MethodCallExpression;
          if (mce != null && IsGood(mce, out var))
          {
            if (!callsites.ContainsKey(var))
            {
              callsites[var] = 1;
            }
            else
            {
              callsites[var]++;
            }
          }
          return base.Walk(node);
        }

        bool IsGood(MethodCallExpression mce, out Variable var)
        {
          var = null;
          if (!mce.TailCall) return false;
          if (mce.Instance == null) return false;
          var i = Unwrap(mce.Instance);
          if (i.Type != typeof(Callable)) return false;
          var be = i as BoundExpression;
          if (be == null) return false;
          var = be.Variable;
          return true;
        }

        protected override bool Walk(BoundExpression node)
        {
          var var = node.Variable;
          if (!refmap.ContainsKey(var))
          {
            refmap[var] = 1;
          }
          else
          {
            refmap[var]++;
          }
          return base.Walk(node);
        }
      }

      class Pass1 : DeepWalker
      {
        readonly List<CodeBlock> fixups;
        Dictionary<Variable, int> refmap, callsites;

        public Pass1(List<CodeBlock> fixups, Dictionary<Variable, int> refmap, Dictionary<Variable, int> callsites)
        {
          this.fixups = fixups;
          this.refmap = refmap;
          this.callsites = callsites;
        }

        static Expression Unwrap(Expression ex)
        {
          while (ex is UnaryExpression && ex.NodeType == AstNodeType.Convert)
          {
            ex = ((UnaryExpression)ex).Operand;
          }

          return ex;
        }

        protected override bool Walk(ReturnStatement node)
        {
          CodeBlockExpression cbe;
          Variable var;
          var mce = node.Expression as MethodCallExpression;
          if (mce != null && IsHoistable(mce, out cbe, out var))
          {
            var inlinedexpr = Generator.InlineCall(Current, cbe, mce.TailCall, mce.Arguments.ToArray());
            node.Expression = inlinedexpr;

            fixups.Add(var.Block);
          }

          return base.Walk(node);
        }

        bool IsHoistable(MethodCallExpression mce, out CodeBlockExpression cbe, out Variable var)
        {
          cbe = null;
          var = null;
          if (!mce.TailCall) return false;
          if (mce.Instance == null) return false;
          var i = Unwrap(mce.Instance);
          if (i.Type != typeof(Callable)) return false;
          var be = i as BoundExpression;
          if (be == null) return false;
          var = be.Variable;

          int x;
          if (!refmap.TryGetValue(var, out x))
          {
            return false;
          }
          else
          {
            if (x != 1)
            {
              return false;
            }
          }

          if (!callsites.TryGetValue(var, out x))
          {
            return false;
          }
          else
          {
            if (x != 1)
            {
              return false;
            }
          }

          if (var.Lift || var.Type != typeof(Callable) || var.ReAssigned) return false;
          if (mce.Method.Name != "Call") return false;
          if (mce.Arguments.Count > 0 && mce.Arguments[0].Type == typeof(object[])) return false;
          var av = var.AssumedValue as MethodCallExpression;
          if (av == null || av.Type != typeof(Callable) || av.Method.Name != "Create") return false;
          cbe = av.Arguments[0] as CodeBlockExpression;
          if (cbe == null || cbe.Block.Parent != Current) return false;
          if (mce.Arguments.Count > 8) return false;

          return true;
        }

      }

      class Pass2 : DeepWalker
      {
        protected override bool Walk(WriteStatement node)
        {
          if (node.Value is MethodCallExpression)
          {
            var mce = node.Value as MethodCallExpression;
            if (mce.Method.Name == "Create" && mce.Type == typeof(Callable))
            {
              var cbe = mce.Arguments[0] as CodeBlockExpression;
              if (cbe != null && cbe.Block.Inlined)
              {
                node.Value = null;
              }
            }
          }
          return base.Walk(node);
        }
      }
    }
  }
}
