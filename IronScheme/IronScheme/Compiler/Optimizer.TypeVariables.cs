#region License
/* Copyright (c) 2007,2008,2009,2010,2011 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using Microsoft.Scripting.Ast;
using System;
using System.Collections.Generic;

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
        Dictionary<Variable, int> writecounts = new Dictionary<Variable, int>();

        protected override bool Walk(WriteStatement node)
        {
          writecounts[node.Variable] = writecounts.ContainsKey(node.Variable) ? writecounts[node.Variable] + 1 : 1;
          return base.Walk(node);
        }

        protected override void PostWalk(WriteStatement node)
        {
          var v = node.Variable;
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
          else
          {
            writecounts[node.Variable]--;
          }

          if (writecounts[node.Variable] > 1)
          {
            v.Type = typeof(object);
          }

          base.PostWalk(node);
        }
      }
    }
  }
}
