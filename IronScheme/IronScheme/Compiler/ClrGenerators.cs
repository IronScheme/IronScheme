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
    protected static MethodInfo Helpers_ConvertToDelegate = typeof(Helpers).GetMethod("ConvertToDelegate");

    protected static Dictionary<string, string> namespaces = new Dictionary<string, string>(StringComparer.CurrentCultureIgnoreCase);

    //this clearly wont scale well at all...
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
            else if (t.Name.ToLower() == nsandname)
            {
              if (namespaces.ContainsKey(t.Namespace))
              {
                return t;
              }
            }
          }
        }
      }
      return null;
    }
  }

  [Generator("clr-field-get-internal")]
  public class ClrFieldGetGenerator : ClrGenerator
  {
    // (clr-field-get type field-name obj )
    public override Expression Generate(object args, CodeBlock cb)
    {
      string type = SymbolTable.IdToString((SymbolId)Builtins.Second(Builtins.First(args)));

      Type t = GetType(type);
      if (t == null)
      {
        throw new NotSupportedException();
      }
      string member = SymbolTable.IdToString((SymbolId)Builtins.Second(Builtins.Second(args)));

      Expression instance = GetAst(Builtins.Third(args), cb);

      return Ast.ReadField(instance, t, member);
    }
  }

  [Generator("clr-field-set!-internal")]
  public class ClrFieldSetGenerator : ClrGenerator
  {
    // (clr-field-set! type field-name obj value)
    public override Expression Generate(object args, CodeBlock cb)
    {
      string type = SymbolTable.IdToString((SymbolId)Builtins.Second(Builtins.First(args)));

      Type t = GetType(type);
      if (t == null)
      {
        throw new NotSupportedException();
      }
      string member = SymbolTable.IdToString((SymbolId)Builtins.Second(Builtins.Second(args)));

      Expression instance = GetAst(Builtins.Third(args), cb);

      Expression value = GetAst(Builtins.Car(Builtins.LastPair(args)), cb);

      return Ast.AssignField(instance, t, member, value);
    }
  }


  [Generator("clr-call-internal")]
  public class ClrCallInternalGenerator : ClrGenerator
  {
    // (clr-call type member obj arg1 ... )
    public override Expression Generate(object args, CodeBlock cb)
    {
      string type = SymbolTable.IdToString((SymbolId)Builtins.Second(Builtins.First(args)));
      
      Type t = GetType(type);
      if (t == null)
      {
        throw new NotSupportedException();
      }
      string member = SymbolTable.IdToString((SymbolId)Builtins.Second(Builtins.Second(args)));

      Expression instance = GetAst(Builtins.Third(args), cb);

      CallType ct = CallType.ImplicitInstance;

      if (instance is ConstantExpression)
      {
        if (((ConstantExpression)instance).Value == null)
        {
          ct = CallType.None;
        }
      }

      Expression[] arguments = GetAstListNoCast(Builtins.Cdddr(args) as Cons, cb);

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
        MethodInfo meth = (MethodInfo)mc.Target.Method;
        // do implicit cast
        ParameterInfo[] pars = meth.GetParameters();
        for (int i = 0; i < arguments.Length; i++)
        {
          Type tt = pars[i].ParameterType;
          if (tt == arguments[i].Type)
          {
            // do nothing
          }
          else
          if (tt.BaseType == typeof(MulticastDelegate))
          {
            arguments[i] = Ast.Call(Helpers_ConvertToDelegate.MakeGenericMethod(tt), arguments[i]);
          }
          else
          {
            arguments[i] = Ast.ConvertHelper(arguments[i], tt);
          }
        }

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

        if ((meth).ReturnType == typeof(void))
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

  //hack for now
  [Generator("clr-clear-usings-internal")]
  public class ClrClearUsingsInternalGenerator : ClrGenerator
  {
    // (clr-clear-usings)
    public override Expression Generate(object args, CodeBlock cb)
    {
      namespaces.Clear();
      return Ast.ReadField(null, Unspecified);
    }
  }

  [Generator("clr-using-internal")]
  public class ClrUsingInternalGenerator : ClrGenerator
  {
    // (clr-using namespace)
    public override Expression Generate(object args, CodeBlock cb)
    {
      object name = Builtins.Second(Builtins.First(args));
      string assname = null;
      if (name is SymbolId)
      {
        assname = SymbolTable.IdToString((SymbolId)name);
        namespaces.Add(assname, assname);
      }
      else
      {
        throw new NotSupportedException();
      }

      return Ast.ReadField(null, Unspecified);
    }
  }

  [Generator("clr-reference-internal")]
  public class ClrReferenceInternalGenerator : ClrGenerator
  {
    // (clr-reference assname)
    public override Expression Generate(object args, CodeBlock cb)
    {
      object name = Builtins.Second(Builtins.First(args));
      string assname = null;
      if (name is SymbolId)
      {
        assname = SymbolTable.IdToString((SymbolId)name);
      }
      else if (name is string)
      {
        assname = (string)name;
      }
      else
      {
        throw new NotSupportedException();
      }
#pragma warning disable 618
      Assembly.LoadWithPartialName(assname);
#pragma warning restore 618

      return Ast.ReadField(null, Unspecified);
    }
  }

  [Generator("clr-is-internal")]
  public class ClrIsInternalGenerator : ClrGenerator
  {
    // (clr-is type arg)
    public override Expression Generate(object args, CodeBlock cb)
    {
      string type = SymbolTable.IdToString((SymbolId)Builtins.Second(Builtins.First(args)));
      Type t = GetType(type);
      if (t == null)
      {
        throw new NotSupportedException();
      }

      return Ast.TypeIs(GetAst(Builtins.Second(args), cb), t);
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

      if (t.BaseType == typeof(MulticastDelegate))
      {
        return Ast.Call( Helpers_ConvertToDelegate.MakeGenericMethod(t), obj);
      }
      else
      {
        return Ast.ConvertHelper(obj, t);
      }
    }
  }

  [Generator("clr-new-array-internal")]
  public class ClrNewArrayInternalGenerator : ClrGenerator
  {
    // (clr-new-array type size )
    public override Expression Generate(object args, CodeBlock cb)
    {
      string type = SymbolTable.IdToString((SymbolId)Builtins.Second(Builtins.First(args)));
      Type t = GetType(type);
      if (t == null)
      {
        throw new NotSupportedException();
      }

      t = t.MakeArrayType();

      Expression size = GetAst(Builtins.Second(args), cb);

      ConstructorInfo ci = t.GetConstructor(new Type[] { typeof(int) });

      return Ast.New(ci, size);
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

        ParameterInfo[] pars = ci.GetParameters();
        for (int i = 0; i < arguments.Length; i++)
        {
          Type tt = pars[i].ParameterType;
          if (tt == arguments[i].Type)
          {
            // do nothing
          }
          else
            if (tt.BaseType == typeof(MulticastDelegate))
            {
              arguments[i] = Ast.Call(Helpers_ConvertToDelegate.MakeGenericMethod(tt), arguments[i]);
            }
            else
            {
              arguments[i] = Ast.ConvertHelper(arguments[i], tt);
            }
        }


        Expression r = Ast.New(ci, arguments);
        return r;
      }

      throw new NotImplementedException();
    }
  }

}
