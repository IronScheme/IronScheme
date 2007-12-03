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

using Microsoft.Scripting.Utils;

namespace IronScheme.Compiler
{
  public abstract class ClrGenerator : SimpleGenerator
  {
    protected static Type GetType(string nsandname)
    {
      foreach (Assembly ass in AppDomain.CurrentDomain.GetAssemblies())
      {
        if (ass.ManifestModule.Name != "<In Memory Module>")
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
      }
      return null;
    }
  }

  [Generator("clr-call-internal")]
  public class ClrCallInternalGenerator : ClrGenerator
  {
    // (clr-call type:member obj arg1 ... )
    public override Expression Generate(object args, CodeBlock cb)
    {
      string typemember = SymbolTable.IdToString((SymbolId)Builtins.Second(Builtins.First(args)));
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

      Expression[] arguments = GetAstListNoCast(Builtins.Cdr(Builtins.Cdr(args)) as Cons, cb);

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
        else if (((MethodInfo)mc.Target.Method).ReturnType.IsValueType)
        {
          return Ast.DynamicConvert(r, typeof(object));
        }
        else
        {
          return r;
        }
      }

      throw new NotImplementedException();
    }

  }

  [Generator("clr-cast-internal")]
  public class ClrCastInternalGenerator : ClrGenerator
  {
    // (clr-cast type arg)
    public override Expression Generate(object args, CodeBlock cb)
    {
      string type = SymbolTable.IdToString((SymbolId)Builtins.Second(Builtins.First(args)));
      Type t = GetType(type);
      if (t == null)
      {
        throw new NotSupportedException();
      }

      Expression obj = GetAst(Builtins.Second(args), cb);

      return Ast.ConvertHelper(obj, t);
    }
  }

  [Generator("clr-new-internal")]
  public class ClrNewInternalGenerator : ClrGenerator
  {
    // (clr-new type arg1 ... )
    public override Expression Generate(object args, CodeBlock cb)
    {
      string type = SymbolTable.IdToString((SymbolId)Builtins.Second(Builtins.First(args)));
      Type t = GetType(type);
      if (t == null)
      {
        throw new NotSupportedException();
      }

      Expression[] arguments = GetAstListNoCast(Builtins.Cdr(args) as Cons, cb);

      List<MethodBase> candidates = new List<MethodBase>();

      foreach (ConstructorInfo c in t.GetConstructors())
      {
        candidates.Add(c);
      }

      Type[] types = new Type[arguments.Length];

      for (int i = 0; i < types.Length; i++)
      {
        types[i] = arguments[i].Type;
      }

      CallType ct = CallType.None;

      MethodBinder mb = MethodBinder.MakeBinder(Binder, "ctr", candidates, BinderType.Normal);

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

      ConstructorInfo ci = null;

      if (mc == null && candidates.Count > 0)
      {
        foreach (ConstructorInfo c in candidates)
        {
          if (c.GetParameters().Length == arguments.Length)
          {
            ci = c;
            break; // tough luck for now
          }
        }
      }
      else
      {
        ci = mc.Target.Method as ConstructorInfo;
      }

      if (ci != null)
      {
        
        ParameterInfo[] cipars = ci.GetParameters();

        for (int i = 0; i < types.Length; i++)
        {
          while (arguments[i] is DynamicConversionExpression)
          {
            arguments[i] = ((DynamicConversionExpression)arguments[i]).Expression;
          }
          if (arguments[i].Type != cipars[i].ParameterType)
          {
            arguments[i] = Ast.DynamicConvert(arguments[i], cipars[i].ParameterType);
          }
        }

        Expression r = Ast.New(ci, arguments);
        return r;
      }

      throw new NotImplementedException();
    }
  }

}
