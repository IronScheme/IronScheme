#region License
/* Copyright (c) 2007-2016 Llewellyn Pritchard
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using Microsoft.Scripting.Ast;
using IronScheme.Runtime;
using System;
using System.Net.Configuration;

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

      // WHY IS THIS NEEDED? build breaks without it
      class Pass0 : DeepWalker
      {
        protected override void PostWalk(MethodCallExpression node)
        {
          base.PostWalk(node);

          var i = Unwrap(node.Instance) as BoundExpression;

          if (i != null && node.Method.Name == "Call" && typeof(IronScheme.Runtime.Typed.ITypedCallable).IsAssignableFrom(i.Type))
          {
            if (SimpleGenerator.descriptorshack2.TryGetValue(i.Variable.AssumedValue, out var cbd))
            {
              if (cbd.varargs)
              {
                // hack but is what it is for now
                return;
              }
            }

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
