#region License
/* Copyright (c) 2007,2008,2009,2010,2011 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using Microsoft.Scripting.Ast;
using System;

namespace IronScheme.Compiler
{
  static partial class Optimizer
  {
    class TypeVariables : OptimizerBase
    {
      public override void Optimize()
      {
        Pass0 p0 = new Pass0();
        p0.WalkNode(Root);
      }

      class Pass0 : DeepWalker
      {
        protected override void PostWalk(WriteStatement node)
        {
          var v = node.Variable;
          if (v.Kind != Variable.VariableKind.Global)
          {
            var val = node.Value;
            if (v.Type == typeof(object))
            {
              if (val.Type != typeof(object))
              {
                v.Type = val.Type;
              }
            }
            else if (v.Type != val.Type)
            {
              v.Type = typeof(object);
            }
          }
          base.PostWalk(node);
        }
      }
    }
  }
}
