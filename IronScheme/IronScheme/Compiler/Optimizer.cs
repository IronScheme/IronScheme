#region License
/* Copyright (c) 2007,2008,2009,2010,2011 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System.Collections.Generic;
using Microsoft.Scripting.Ast;

namespace IronScheme.Compiler
{
  static partial class Optimizer
  {
    class DeepWalker : Walker
    {
      readonly Dictionary<CodeBlock, bool> blocks = new Dictionary<CodeBlock, bool>();
      protected CodeBlock Current;

      protected override bool Walk(CodeBlock node)
      {
        blocks[node] = true;
        Current = node;
        return base.Walk(node);
      }

      protected override void PostWalk(CodeBlock node)
      {
        base.PostWalk(node);
        Current = node.Parent ?? node;
      }

      protected override void PostWalk(CodeBlockExpression node)
      {
        if (!blocks.ContainsKey(node.Block))
        {
          WalkNode(node.Block);
        }
        base.PostWalk(node);
      }
    }

    abstract class OptimizerBase
    {
      public CodeBlock Root { get; set; }

      public abstract void Optimize();
    }

    static void Optimize<T>(CodeBlock cb) where T : OptimizerBase, new()
    {
      var opt = new T { Root = cb };
      opt.Optimize();
    }
    
    public static void Optimize(CodeBlock cb)
    {
      Optimize<FixupConditionals>(cb);
      Optimize<FlattenBodies>(cb);
      Optimize<RemoveTemporaries>(cb);
      Optimize<FixupPrimitives>(cb);
      Optimize<TypeVariables>(cb);
      //Optimize<ConversionCSE>(cb);
      Optimize<TCE>(cb);
      Optimize<FlattenBodies>(cb);
      //Optimize<LoopHoist>(cb);
      //Optimize<FlattenBodies>(cb);
      Optimize<RemoveTemporaries>(cb);
    }
  }
}
