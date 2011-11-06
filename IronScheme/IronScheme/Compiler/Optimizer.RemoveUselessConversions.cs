#region License
/* Copyright (c) 2007,2008,2009,2010,2011 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using Microsoft.Scripting.Ast;

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
      }
    }
  }
}
