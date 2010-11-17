#region License
/* Copyright (c) 2007,2008,2009,2010 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Collections.Generic;
using System.Text;

namespace IronScheme.Compiler
{
  static partial class Optimizer
  {

#if DONE
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
#endif
  }
}
