#region License
/* Copyright (c) 2007-2013 Llewellyn Pritchard 
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
    class TCE : OptimizerBase
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

        Pass0 p0 = new Pass0(fixups);
        p0.WalkNode(Root);

        foreach (var cb in Distinct(fixups))
        {
          cb.Bind();
        }

        Root.Bind();
      }

      class Pass0 : DeepWalker
      {
        readonly List<CodeBlock> fixups;

        public Pass0(List<CodeBlock> fixups)
        {
          this.fixups = fixups;
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
          if (mce != null && IsTCE(mce, out var))
          {
            if (!(Current.Body is LabeledStatement))
            {
              Current.Body = Ast.Labeled(Current.Body);
            }

            var ee = new List<Expression>();
            int i = 0;
            var temps = new List<Variable>();
            foreach (var par in Current.Parameters)
            {
              var v = Current.CreateLocalVariable((SymbolId)Builtins.GenSym(par.Name), par.Type);
              ee.Add(Ast.Assign(v, mce.Arguments[i++]));
              temps.Add(v);
            }
            i = 0;
            foreach (var par in Current.Parameters)
            {
              ee.Add(Ast.Assign(par, Ast.Read(temps[i++])));
            }
            ee.Add(Ast.Void(Ast.Continue(mce.Span)));
            node.Expression = Ast.Comma(ee);

            if (var != null)
            {
              var.Lift = false;
              fixups.Add(var.Block);
            }

            Current.Bind();
          }

          return base.Walk(node);
        }

        bool IsTCE(MethodCallExpression mce, out Variable var)
        {
          var = null;
          // this could possibly removed if the label can be moved before the environment is allocated
          if (Current.HasEnvironment) return false;
          if (!mce.TailCall) return false;
          if (mce.Instance == null) return false;
          if (mce.Arguments.Count > 8) return false;
          var i = Unwrap(mce.Instance);
          if (i is CodeBlockExpression)
          {
            var cbe = i as CodeBlockExpression;
            if (cbe.Block == Current)
            {
              return true;
            }
            return false;
          }
          else
          {
            if (!typeof(Callable).IsAssignableFrom(i.Type)) return false;
            var be = i as BoundExpression;
            if (be == null) return false;
            var = be.Variable;
            if (/*!var.Lift ||*/ !typeof(Callable).IsAssignableFrom(var.Type) || var.ReAssigned) return false;
            if (!(mce.Method.Name == "Call" || mce.Method.Name == "Invoke")) return false;
            if (mce.Arguments.Count > 0 && mce.Arguments[0].Type == typeof(object[])) return false;
            var av = var.AssumedValue as MethodCallExpression;
            
            while (av == null && be.Variable.AssumedValue is BoundExpression)
            {
              be = be.Variable.AssumedValue as BoundExpression;
              av = be.Variable.AssumedValue as MethodCallExpression;
            }

            if (av == null && be.Variable.AssumedValue is NewExpression)
            {
              var ne = be.Variable.AssumedValue as NewExpression;
              if (!typeof(Callable).IsAssignableFrom(ne.Type)) return false;
              var cbe = ne.Arguments[0] as CodeBlockExpression;
              if (cbe == null || cbe.Block != Current) return false;
            }
            else
            {
              if (av == null || !typeof(Callable).IsAssignableFrom(av.Type) || av.Method.Name != "Create") return false;
              var cbe = av.Arguments[0] as CodeBlockExpression;
              if (cbe == null || cbe.Block != Current) return false;
            }

            return true;
          }
        }

      }
    }
  }
}
