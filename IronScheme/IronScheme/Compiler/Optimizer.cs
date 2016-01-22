#region License
/* Copyright (c) 2007-2016 Llewellyn Pritchard
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

      protected override bool Walk(CodeBlockExpression node)
      {
        return !blocks.ContainsKey(node.Block) && base.Walk(node);
      }

      protected override void PostWalk(CodeBlockExpression node)
      {
        base.PostWalk(node);
        if (!blocks.ContainsKey(node.Block))
        {
          var c = Current;
          WalkNode(node.Block);
          Current = c;
        }
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
      Optimize<FixupTypedClosureCallsites>(cb);
      Optimize<RemoveUselessConversions>(cb);
      Optimize<FixupConditionals>(cb);
      Optimize<FlattenBodies>(cb);
      Optimize<RemoveTemporaries>(cb);
      Optimize<RemoveUselessConversions>(cb); // just 1 hit
      Optimize<FixupPrimitives>(cb);
      Optimize<SimpleTypeVariables>(cb); // for callables

      Optimize<FixupTypedClosureCallsites>(cb);
      Optimize<RemoveUselessConversions>(cb);

      Optimize<TCE>(cb);
      Optimize<FlattenBodies>(cb);
      Optimize<TCE>(cb); // seems to get a few extra
      Optimize<FlattenBodies>(cb);

      //Optimize<MarkUnspecifiedProcedureReturns>(cb);

      Optimize<LoopHoist>(cb);
      Optimize<FlattenBodies>(cb); // important to clean up previous
      Optimize<TypeVariables>(cb);

      Optimize<RemoveTemporaries>(cb); // for unreferenced loop variables


      //Optimize<RemoveTemporaries>(cb);
      //Optimize<ConversionCSE>(cb);
      Optimize<RemoveUselessConversions>(cb);
      Optimize<FixupTypedClosureCallsites>(cb);
      Optimize<RemoveUselessConversions>(cb);
      Optimize<RemoveTemporaries>(cb);

      //Optimize<MarkUnspecifiedProcedureReturns>(cb);
      Optimize<MarkNonRecursiveProcedures>(cb);
      Optimize<AnalyzeProcedureReturns>(cb); // this is too early too

    }
  }
}
