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
    class FixupConditionals : OptimizerBase
    {
      public override void Optimize()
      {
        Pass0 p0 = new Pass0();
        p0.WalkNode(Root);
      }

      class Pass0 : DeepWalker
      {
        protected override void PostWalk(ConditionalExpression node)
        {
          base.PostWalk(node);

          if (node.Test is UnaryExpression && node.Test.NodeType == AstNodeType.Not)
          {
            var tmp = node.IfFalse;
            node.IfFalse = node.IfTrue;
            node.IfTrue = tmp;

            var ue = node.Test as UnaryExpression;

            node.Test = ue.Operand;
          }

          var truetype = node.IfTrue.Type;
          var falsetype = node.IfFalse.Type;

          // the type gets fixed later in removeuseless conversions
          if (truetype != falsetype && (truetype.IsValueType || falsetype.IsValueType))
          {
            if (node.IfTrue is UnaryExpression && node.IfTrue.NodeType == AstNodeType.Convert)
            {
              var ue = (UnaryExpression)node.IfTrue;
              if (ue.Operand.Type == falsetype)
              {
                node.IfTrue = ue.Operand;
                node.SetType(falsetype);
                return;
              }
            }
            if (node.IfFalse is UnaryExpression && node.IfFalse.NodeType == AstNodeType.Convert)
            {
              var ue = (UnaryExpression)node.IfFalse;
              if (ue.Operand.Type == truetype)
              {
                node.IfFalse = ue.Operand;
                node.SetType(truetype);
                return;
              }
            }
          }
        }
      }
    }
  }
}
