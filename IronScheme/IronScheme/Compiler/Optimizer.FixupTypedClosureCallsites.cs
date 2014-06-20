#region License
/* Copyright (c) 2007-2014 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using Microsoft.Scripting.Ast;
using IronScheme.Runtime;

namespace IronScheme.Compiler
{
  static partial class Optimizer
  {
    class FixupTypedClosureCallsites : OptimizerBase
    {
      public override void Optimize()
      {
        Pass0 p0 = new Pass0();
        p0.WalkNode(Root);
      }

      class Pass0 : DeepWalker
      {
        static Expression Unwrap(Expression ex)
        {
          while (ex is UnaryExpression && ((UnaryExpression)ex).NodeType == AstNodeType.Convert)
          {
            ex = ((UnaryExpression)ex).Operand;
          }

          return ex;
        }

        protected override void PostWalk(MethodCallExpression node)
        {
          base.PostWalk(node);

          var i = Unwrap(node.Instance);

          if (i != null && node.Method.Name == "Call" && typeof(IronScheme.Runtime.Typed.ITypedCallable).IsAssignableFrom(i.Type))
          {
            var mi = i.Type.GetMethod("Invoke");
            node.Method = mi;
            node.Instance = i;
            node.Arguments = node.Arguments.ConvertAll(e => Unwrap(e));
            node.ParameterInfos = mi.GetParameters();
          }
        }
      }
    }
  }
}
