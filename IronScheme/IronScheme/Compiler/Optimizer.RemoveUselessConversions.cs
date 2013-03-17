#region License
/* Copyright (c) 2007-2013 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using Microsoft.Scripting.Ast;
using System.Diagnostics;

namespace IronScheme.Compiler
{
  static partial class Optimizer
  {
    class RemoveUselessConversions : OptimizerBase
    {
      public override void Optimize()
      {
        Pass0 p0 = new Pass0();
        p0.WalkNode(Root);
      }

      class Pass0 : DeepWalker
      {
        protected override void PostWalk(UnaryExpression node)
        {
          base.PostWalk(node);

          if (node.NodeType == AstNodeType.Convert)
          {
            var op = node.Operand;
            if (op.NodeType == AstNodeType.Convert)
            {
              var inode = op as UnaryExpression;
              if (inode.Operand.Type == node.Type)
              {
                node.Operand = inode.Operand;
              }
            }
            if (node.Type != typeof(object) && 
              node.Type != op.Type && 
              node.Type.IsAssignableFrom(op.Type) && 
              !(node.Type.IsInterface && op.Type.IsValueType))
            {
              node.SetType(op.Type);
            }
          }
        }

        protected override void PostWalk(ReturnStatement node)
        {
          base.PostWalk(node);

          if (node.Expression is UnaryExpression && node.Expression.NodeType == AstNodeType.Convert)
          {
            var ue = (UnaryExpression)node.Expression;
            if (ue.Operand is MethodCallExpression)
            {
              var mce = (MethodCallExpression)ue.Operand;

              if (ue.Type == mce.Type || mce.Type == Current.ReturnType)
              {
                node.Expression = mce;

                if (mce.Type == Current.ReturnType)
                {
                  mce.TailCall = true;
                }
              }
            }
          }
        }

        protected override void PostWalk(ConditionalExpression node)
        {
          base.PostWalk(node);

          if (node.Test is MethodCallExpression)
          {
            var mce = (MethodCallExpression)node.Test;
            if (mce.Method == typeof(IronScheme.Runtime.Builtins).GetMethod("IsTrue"))
            {
              if (mce.Arguments[0].Type == typeof(bool))
              {
                node.Test = mce.Arguments[0];
              }
            }
          }
        }

        protected override void PostWalk(IfStatementTest node)
        {
          base.PostWalk(node);

          if (node.Test is MethodCallExpression)
          {
            var mce = (MethodCallExpression)node.Test;
            if (mce.Method == typeof(IronScheme.Runtime.Builtins).GetMethod("IsTrue"))
            {
              if (mce.Arguments[0].Type == typeof(bool))
              {
                node.Test = mce.Arguments[0];
              }
            }
          }
        }
      }
    }
  }
}
