#region License
/* Copyright (c) 2007,2008,2009,2010,2011 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Reflection;
using IronScheme.Runtime;
using Microsoft.Scripting;
using Microsoft.Scripting.Ast;
using System.Collections.Generic;

namespace IronScheme.Compiler
{
  static partial class Optimizer
  {
    class FixupPrimitives : OptimizerBase
    {
      public override void Optimize()
      {
        Pass0 p0 = new Pass0();
        p0.WalkNode(Root);
      }

      class Pass0 : DeepWalker
      {
        protected override bool Walk(MethodCallExpression node)
        {
          var i = node.Instance;
          while (i is UnaryExpression && i.NodeType == AstNodeType.Convert)
          {
            i = ((UnaryExpression)i).Operand;
          }

          if (i is BoundExpression)
          {

            var be = (BoundExpression)i;
            var v = be.Variable.Name;

            if (Builtins.IsTrue(Builtins.IsSymbolBound(v)))
            {
              var val = Builtins.SymbolValue(v);

              var c = val as BuiltinMethod;
              if (c != null)
              {
                var mb = c.Binder;

                var pars = new Expression[node.Arguments.Count];
                node.Arguments.CopyTo(pars, 0);

                Type[] types = Array.ConvertAll(pars, x => x.Type);
                MethodCandidate mc = mb.MakeBindingTarget(CallType.None, types);
                if (mc != null)
                {
                  var meth = mc.Target.Method as MethodInfo;

                  node.Method = meth;
                  node.Instance = null;
                }
              }
              else if (val is Closure)
              {
                if (node.Method != Generator.ICallable_CallN)
                {
                  var cc = val as Closure;

                  // will not work on varargs, need to somehow decorate them
                  var meth = Array.Find(cc.Targets,
                    x => x.GetParameters().Length == node.Arguments.Count);

                  if (meth != null)
                  {
                    node.Method = meth;
                    node.Instance = null;
                  }
                  else
                  {
                    meth = Array.Find(cc.VarargTargets,
                      x => x.GetParameters().Length - 1 <= node.Arguments.Count);

                    if (meth != null)
                    {
                      var newargs = new List<Expression>();

                      var pars = meth.GetParameters();
                      var parlen = pars.Length;
                      int x = 0;
                      for (; x < parlen - 1; x++)
                      {
                        newargs.Add(node.Arguments[x]);
                      }

                      var tail = new List<Expression>();
                      for (; x < node.Arguments.Count; x++)
                      {
                        tail.Add(node.Arguments[x]);
                      }

                      var tailarr = tail.ToArray();

                      newargs.Add(Ast.ComplexCallHelper(Compiler.Generator.MakeList(tailarr, true), tailarr));

                      node.Arguments.Clear();
                      node.Arguments.AddRange(newargs);

                      node.Method = meth;

                      node.ParameterInfos = pars;

                      node.Instance = null;
                    }
                  }
                }
              }
            }
          }

          return base.Walk(node);
        }
      }
    }
  }
}
