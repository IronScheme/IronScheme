#region License
/* Copyright (c) 2007,2008,2009,2010,2011 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting;
using Microsoft.Scripting.Ast;
using System.Diagnostics;

namespace IronScheme.Compiler
{
  static partial class Optimizer
  {

#if !DONE
    class ConversionCSE : OptimizerBase
    {
      readonly Dictionary<Variable, Dictionary<Type, int>> references = new Dictionary<Variable, Dictionary<Type, int>>();

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
          //if (Current.Name.EndsWith("fxdiv")) Debugger.Break();

          if (node.NodeType == AstNodeType.Convert && node.Operand is BoundExpression)
          {
            var be = node.Operand as BoundExpression;

            if (!be.Variable.Lift)
            {
              var origbe = be;

              if (be.Variable.Kind == Variable.VariableKind.Local && be.Variable.AssumedValue is BoundExpression)
              {
                var assbe = be.Variable.AssumedValue as BoundExpression;
                if (assbe.Variable.Kind == Variable.VariableKind.Parameter)
                {
                  be = assbe;
                }
              }

              if (be.Variable.Kind == Variable.VariableKind.Parameter
                && be.Type == typeof(object)
                && node.Type != typeof(object))
              {
                Dictionary<Type, int> counts;
                if (!references.TryGetValue(origbe.Variable, out counts))
                {
                  references[origbe.Variable] = counts = new Dictionary<Type, int>();
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
          if (node.NodeType == AstNodeType.Convert && node.Operand is BoundExpression)
          {
            //if (Current.Name.EndsWith("fxdiv")) Debugger.Break();
            var be = node.Operand as BoundExpression;
            var tv = be.Variable.GetTypedVariable(node.Type);
            if (tv != null)
            {
              be.Variable = tv;
            }
          }
          return base.Walk(node);
        }
      }

      public override void Optimize()
      {
        Pass0 p0 = new Pass0(references);
        p0.WalkNode(Root);

        foreach (var v in references.Keys)
        {
          foreach (var t in references[v].Keys)
          {
            var c = references[v][t];

            if (t != typeof(object) && c > 1 && t.IsValueType)
            {
              var n = SymbolTable.StringToId(t.Name + "___" + SymbolTable.IdToString(v.Name));
              var tv = v.Block.CreateVariable(n, Variable.VariableKind.Local, t);
              v.SetTypedVariable(t, tv);

              var vv = v;

              if (vv.Kind == Variable.VariableKind.Local && vv.AssumedValue is BoundExpression)
              {
                var assbe = vv.AssumedValue as BoundExpression;
                if (assbe.Variable.Kind == Variable.VariableKind.Parameter)
                {
                  vv = assbe.Variable;
                }
              }

              var inittv = Ast.Write(tv, Ast.SimpleCallHelper(typeof(IronScheme.Runtime.Helpers).GetMethod("UnsafeConvert").MakeGenericMethod(tv.Type), Ast.Read(vv)));

              var bs = v.Block.Body as BlockStatement;
              if (bs != null)
              {
                Statement[] newbody = new Statement[bs.Statements.Count + 1];
                newbody[0] = inittv;
                bs.Statements.CopyTo(newbody, 1);

                v.Block.Body = Ast.Block(newbody);
              }
              else
              {
                v.Block.Body = Ast.Block(inittv, v.Block.Body);
              }
            }
          }
          v.Block.Bind();
        }

        Pass1 p1 = new Pass1(references);
        p1.WalkNode(Root);

        foreach (var v in references.Keys)
        {
          v.Block.Bind();
        }

      }
    }
#endif
  }
}
