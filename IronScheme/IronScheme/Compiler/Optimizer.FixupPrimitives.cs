using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting.Ast;
using IronScheme.Runtime;
using System.Reflection;
using Microsoft.Scripting;
using System.Diagnostics;

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

          if (i is BoundExpression && node.Method != Generator.ICallable_CallN)
          {
            var be = (BoundExpression)i;
            var v = be.Variable.Name;

            if (Builtins.IsTrue(Builtins.IsSymbolBound(v)))
            {
              var val = Builtins.SymbolValue(v);
              var c =  val as BuiltinMethod;
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
                var cc = val as Closure;

                // will not work on varargs, need to somehow decorate them
                var meth = Array.Find(cc.Targets, 
                  x => x.GetParameters().Length == node.Arguments.Count);

                if (meth != null)
                {
                  node.Method = meth;
                  node.Instance = null;
                }
              }
            }
            else
            {
              // this break much shit!
              //var pars = new Expression[node.Arguments.Count];
              //node.Arguments.CopyTo(pars, 0);

              //var r = Generator.TryConvertToDirectCall(v, pars);

              //if (r != null)
              //{
              //  node.Instance = r.Instance;
              //  node.Method = r.Method;
              //}
            }
          }

          return base.Walk(node);
        }
      }
    }
  }
}
