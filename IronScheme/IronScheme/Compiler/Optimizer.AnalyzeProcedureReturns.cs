#region License
/* Copyright (c) 2007-2013 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Collections.Generic;
using Microsoft.Scripting.Ast;
using System.Diagnostics;
using IronScheme.Runtime;
using System.Reflection.Emit;

namespace IronScheme.Compiler
{
  static partial class Optimizer
  {
    class MarkUnspecifiedProcedureReturns : OptimizerBase
    {
      public override void Optimize()
      {
        int changes = 0;

        do
        {
          changes = 0;
          var returns = new Dictionary<CodeBlock, List<ReturnStatement>>();
          Pass0 p0 = new Pass0 { Returns = returns };
          p0.WalkNode(Root);

          foreach (var kv in returns)
          {
            var cb = kv.Key;
            var retlist = kv.Value;

            if (cb.DecorateWithUnspecifiedReturn)
            {
              continue;
            }

            var allgood = false;

            foreach (var ret in retlist)
            {
              if (ret.Expression is MethodCallExpression)
              {
                var mce = (MethodCallExpression)ret.Expression;
                if (mce.Instance is CodeBlockExpression && mce.Method.Name == "Invoke")
                {
                  var ii = mce.Instance as CodeBlockExpression;
                  if (ii.Block.DecorateWithUnspecifiedReturn)
                  {
                    allgood = true;
                    continue;
                  }
                }

                if (!(mce.Method is MethodBuilder) && Attribute.IsDefined(mce.Method, typeof(UnspecifiedReturnAttribute)))
                {
                  allgood = true;
                  continue;
                }
              }
              if (!(ret.Expression is MemberExpression))
              {
                allgood = false;
                break;
              }

              var expr = ret.Expression;
              if (expr is MemberExpression)
              {
                var me = (MemberExpression)expr;
                if (me.Member == Generator.Unspecified)
                {
                  allgood = true;
                }
                else
                {
                  allgood = false;
                  break;
                }
              }
            }

            if (allgood && !cb.DecorateWithUnspecifiedReturn)
            {
              changes++;
              cb.DecorateWithUnspecifiedReturn = true;
            }

          }
        }
        while (changes > 0);
      }

      class Pass0 : DeepWalker
      {
        internal Dictionary<CodeBlock, List<ReturnStatement>> Returns;

        protected override void PostWalk(ReturnStatement node)
        {
          base.PostWalk(node);

          List<ReturnStatement> retlist;

          if (!Returns.TryGetValue(Current, out retlist))
          {
            Returns[Current] = retlist = new List<ReturnStatement>();
          }

          retlist.Add(node);
        }
      }
    }


    class AnalyzeProcedureReturns : OptimizerBase
    {
      public override void Optimize()
      {
        var returns = new Dictionary<CodeBlock, List<ReturnStatement>>();
        Pass0 p0 = new Pass0 { Returns = returns };
        p0.WalkNode(Root);

        foreach (var kv in returns)
        {
          var cb = kv.Key;
          var retlist = kv.Value;

          foreach (var ret in retlist)
          {
            var expr = ret.Expression;
            if (expr is MethodCallExpression)
            {
              var mce = (MethodCallExpression)expr;
              if (mce.TailCall)
              {
                var ass = mce.Method.DeclaringType.Assembly;
                if (ass == typeof(Optimizer).Assembly || 
                    ass == typeof(Oyster.Math.IntX).Assembly ||
                    ass == typeof(CodeBlock).Assembly ||
                    ass.GlobalAssemblyCache)
                {
                  if (mce.Method.DeclaringType.BaseType != typeof(MulticastDelegate))
                  {
                    mce.TailCall = false;
                  }
                }
                else
                {
                  // what here?
                  // look for method decorated with [Recursive] or [NonRecursive] attribute
                  //Console.WriteLine(mce.Method);
                }
              }
            }
          }
        }

      }

      class Pass0 : DeepWalker
      {
        internal Dictionary<CodeBlock, List<ReturnStatement>> Returns;

        protected override void PostWalk(ReturnStatement node)
        {
          base.PostWalk(node);

          List<ReturnStatement> retlist;

          if (!Returns.TryGetValue(Current, out retlist))
          {
            Returns[Current] = retlist = new List<ReturnStatement>();
          }

          retlist.Add(node);
        }
      }
    }
  }
}
