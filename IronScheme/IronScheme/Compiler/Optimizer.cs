#region License
/* ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 
 *
 * This source code is subject to terms and conditions of the Microsoft Public License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Public License.
 *
 * You must not remove this notice, or any other, from this software.
 * ***************************************************************************/
#endregion

using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting.Ast;
using Microsoft.Scripting;
using System.Diagnostics;
using System.Collections.ObjectModel;
using IronScheme.Runtime;
using Microsoft.Scripting.Utils;
using System.Reflection;

namespace IronScheme.Compiler
{
  static partial class Optimizer
  {
    class DeepWalker : Walker
    {
      readonly Dictionary<CodeBlock, bool> blocks = new Dictionary<CodeBlock, bool>();

      protected override bool Walk(CodeBlock node)
      {
        blocks[node] = true;
        return base.Walk(node);
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
    }
  }
}
