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
using System.Reflection;
using System.Diagnostics;
using Microsoft.Scripting.Ast;
using Microsoft.Scripting;
using IronScheme.Runtime;
using IronScheme.Compiler;

using Generator = IronScheme.Compiler.Generator;
using Microsoft.Scripting.Utils;

[assembly: Extension]

namespace IronScheme.Clr
{
  [Generator("clr-call")]
  public class ClrCallGenerator : SimpleGenerator
  {
    // (clr-call type:member obj arg1 ... )
    public override Expression Generate(object args, CodeBlock cb)
    {
      string typemember = SymbolTable.IdToString((SymbolId)Builtins.First(args));
      string[] tokens = typemember.Split(':');
      Type t = GetType(tokens[0]);
      if (t == null)
      {
        throw new NotSupportedException();
      }
      string member = tokens[1];

      Expression instance = GetAst(Builtins.Second(args), cb);

      CallType ct = CallType.ImplicitInstance;

      if (instance is ConstantExpression)
      {
        if (((ConstantExpression)instance).Value == null)
        {
          ct = CallType.None;
        }
      }


      Expression[] arguments = GetAstList(Builtins.Cddr(args) as Cons, cb);

      List<MethodBase> candidates = new List<MethodBase>();

      foreach (MemberInfo mi in t.GetMember(member, BindingFlags.IgnoreCase | BindingFlags.Public | BindingFlags.Static | BindingFlags.Instance))
      {
        if (mi is MethodInfo)
        {
          candidates.Add((MethodInfo)mi);    
        }
      }

      Type[] types = new Type[arguments.Length];

      for (int i = 0; i < types.Length; i++)
			{
			 types[i] = arguments[i].Type;
			}

      if (ct == CallType.ImplicitInstance)
      {
        types = ArrayUtils.Insert(t, types);
      }

      MethodBinder mb = MethodBinder.MakeBinder(Binder, member, candidates, BinderType.Normal);

      MethodCandidate mc = mb.MakeBindingTarget(ct, types);

      if (mc == null)
      {
        types = new Type[arguments.Length];

        for (int i = 0; i < types.Length; i++)
        {
          types[i] = typeof(object);
        }

        if (ct == CallType.ImplicitInstance)
        {
          types = ArrayUtils.Insert(t, types);
        }

        mc = mb.MakeBindingTarget(ct, types);
      }

      if (mc != null)
      {
        Expression r = null;
        // o god...
        if (ct == CallType.ImplicitInstance)
        {
          r = Ast.ComplexCallHelper(instance, (MethodInfo)mc.Target.Method, arguments);
        }
        else
        {
          r = Ast.ComplexCallHelper((MethodInfo)mc.Target.Method, arguments);
        }

        if (((MethodInfo)mc.Target.Method).ReturnType == typeof(void))
        {
          return Ast.Comma(r, Ast.ReadField(null, Unspecified));
        }
        else
        {
          return r;
        }
      }

      throw new NotImplementedException();
    }

    Type GetType(string nsandname)
    {
      foreach (Assembly  ass in AppDomain.CurrentDomain.GetAssemblies())
      {
        foreach (Type t in ass.GetExportedTypes())
        {
          string nsm = t.Namespace + "." + t.Name;
          nsm = nsm.ToLower();

          if (nsm == nsandname)
          {
            return t;
          }
        }
      }
      return null;
    }
  }
}
