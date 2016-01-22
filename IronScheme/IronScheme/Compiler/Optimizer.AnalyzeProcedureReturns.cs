#region License
/* Copyright (c) 2007-2016 Llewellyn Pritchard
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Collections.Generic;
using Microsoft.Scripting;
using Microsoft.Scripting.Ast;
using System.Diagnostics;
using IronScheme.Runtime;
using System.Reflection.Emit;
using Microsoft.Scripting.Utils;

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

    //TODO: check why this does not work for bootfile...
    class MarkNonRecursiveProcedures : OptimizerBase
    {
      public override void Optimize()
      {
        int changes = 0;

        var returns = new Dictionary<CodeBlock, List<ReturnStatement>>();
        Pass0 p0 = new Pass0 { Returns = returns };
        p0.WalkNode(Root);

        do
        {
          changes = 0;

          foreach (var kv in returns)
          {
            var cb = kv.Key;
            var retlist = kv.Value;

            if (cb.DecorateWithNonRecursive)
            {
              continue;
            }

            var allgood = true;

            foreach (var ret in retlist)
            {
              if (ret.Expression is MethodCallExpression)
              {
                var mce = (MethodCallExpression)ret.Expression;
                if (mce.Instance is CodeBlockExpression && mce.Method.Name == "Invoke")
                {
                  var ii = mce.Instance as CodeBlockExpression;
                  if (ii.Block.DecorateWithNonRecursive)
                  {
                    continue;
                  }
                }

                if (mce.TailCall)
                {
                  allgood = false;
                  break;
                }

                if (!(mce.Method is MethodBuilder) && Attribute.IsDefined(mce.Method, typeof(IronScheme.Runtime.NonRecursiveAttribute)))
                {
                  continue;
                }
              }

            }

            if (allgood && !cb.DecorateWithNonRecursive)
            {
              changes++;
              cb.DecorateWithNonRecursive = true;
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

            var ue = expr as UnaryExpression;
            if (ue != null && ue.NodeType == AstNodeType.Convert)
            {
              if (ue.Type == typeof(object) && !ue.Operand.Type.IsValueType)
              {
                expr = ue.Operand;
                ret.Expression = expr;
              }
            }

            if (expr is MethodCallExpression)
            {
              var mce = (MethodCallExpression)expr;
              if (mce.TailCall)
              {
                var type = mce.Method.DeclaringType;
                var ass = type.Assembly;
                
                // check for proc attribute class and defy if found, all will be tail called
                if (ass.GlobalAssemblyCache || (!Attribute.IsDefined(type, typeof(ProcedureAttribute)) && !typeof(IModuleDictionaryInitialization).IsAssignableFrom(type)))
                {
                  // always tail call delegates
                  if (type.BaseType != typeof(MulticastDelegate))
                  {
                    var pa = (ProcedureAttribute) Attribute.GetCustomAttribute(mce.Method, typeof(ProcedureAttribute));

                    // either must not allow it
                    if (pa != null)
                    {
                      mce.TailCall = pa.AllowTailCall;
                    }
                    else
                    {
                      bool tc = false;
                      foreach (BuiltinAttribute ba in Attribute.GetCustomAttributes(mce.Method, typeof(BuiltinAttribute)))
                      {
                        if (ba.AllowTailCall)
                        {
                          tc = true;
                        }
                      }
                      mce.TailCall = tc;
                    }
                  }
                }
                //else if (!mce.Method.DeclaringType.IsAssignableFrom(typeof(Callable)))
                {
                  // what here?
                  // Callable should be excluded, match with known list
                  // The problem here is that direct calls are only resolved at link time
                  // We need to make a list to use at LTO time too
                  // Perhaps all we need, is to take care of it at LTO
                  //Console.WriteLine("In {0} => {1}", cb.Name, mce.Method.Name);

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
