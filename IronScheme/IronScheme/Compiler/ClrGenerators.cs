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
using System.Text.RegularExpressions;

namespace IronScheme.Compiler
{
  public abstract class ClrGenerator : SimpleGenerator
  {
    protected static MethodInfo Helpers_ConvertToDelegate = typeof(Helpers).GetMethod("ConvertToDelegate");
    protected static MethodInfo Helpers_SymbolToEnum = typeof(Helpers).GetMethod("SymbolToEnum");
    protected static MethodInfo Helpers_EnumToSymbol = typeof(Helpers).GetMethod("EnumToSymbol");

    protected static Dictionary<string, string> namespaces = new Dictionary<string, string>(StringComparer.CurrentCultureIgnoreCase);

    protected static Regex typeparser = new Regex(@"(?<ns>([^\s.<]+\.)*)(?<type>[^\s.<]+)(?<args>(<[^>]+>)?)"); // last bit has to be greedy

    //this clearly wont scale well at all...
    protected static Type GetType(string nsandname)
    {
      Match m = typeparser.Match(nsandname);
      if (!m.Success)
      {
        return null;
      }

      string am = m.Groups["args"].Value;

      string[] genargs = am.Length > 0 ? am.Substring(1, am.Length - 2).Split('&') : new string[0];

      Type[] gentypes = Array.ConvertAll<string, Type>(genargs, GetType);

      nsandname = m.Groups["ns"].Value + m.Groups["type"].Value;
      
      foreach (Assembly ass in AppDomain.CurrentDomain.GetAssemblies())
      {
        if (ass.ManifestModule.Name != "<In Memory Module>")
        {
          foreach (Type t in ass.GetExportedTypes())
          {
            string tname = t.Name;
            int genargsc = 0;

            int geni = t.Name.IndexOf('`');

            if (geni > 0)
            {
              genargsc = int.Parse(tname.Substring(geni + 1));
              tname = tname.Substring(0, geni);
            }

            string nsm = t.Namespace + "." + tname;
            nsm = nsm.ToLower();

            if (nsm == nsandname)
            {
              if (gentypes.Length == genargsc && gentypes.Length > 0)
              {
                return t.MakeGenericType(gentypes);
              }
              return t;
            }
            else if (tname.ToLower() == nsandname)
            {
              if (namespaces.ContainsKey(t.Namespace))
              {
                if (gentypes.Length == genargsc && gentypes.Length > 0)
                {
                  return t.MakeGenericType(gentypes);
                }
                return t;
              }
            }
          }
        }
      }
      return null;
    }

    protected static Expression ConvertFromHelper(Type t, Expression e)
    {
      if (t == typeof(void))
      {
        return Ast.Comma(e, Ast.ReadField(null, Unspecified));
      }
      else if (t.BaseType == typeof(Enum))
      {
        return Ast.Call(Helpers_EnumToSymbol.MakeGenericMethod(t), e);
      }
      else if (t.IsValueType)
      {
        return Ast.DynamicConvert(e, typeof(object));
      }
      else
      {
        return e;
      }
    }

    protected static Expression ConvertToHelper(Type t, Expression e)
    {
      if (t == e.Type)
      {
        return e;
      }
      else
        if (t.BaseType == typeof(MulticastDelegate))
        {
          return Ast.Call(Helpers_ConvertToDelegate.MakeGenericMethod(t), e);
        }
        else
          if (t.BaseType == typeof(Enum))
          {
            return Ast.Call(Helpers_SymbolToEnum.MakeGenericMethod(t), e);
          }
          else
          {
            return Ast.ConvertHelper(e, t);
          }
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
        Builtins.SyntaxError("clr-field-get", "type not found", type, false);
      }
      string member = SymbolTable.IdToString((SymbolId)Builtins.Second(Builtins.Second(args)));

      FieldInfo fi = t.GetField(member);

      if (fi == null)
      {
        Builtins.SyntaxError("clr-field-get", "field not found on type: " + type, args, member);
      }

      Expression instance = GetAst(Builtins.Third(args), cb);

      return Ast.ReadField(instance, fi);
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
        Builtins.SyntaxError("clr-field-set!", "type not found", type, false);
      }
      string member = SymbolTable.IdToString((SymbolId)Builtins.Second(Builtins.Second(args)));

      FieldInfo fi = t.GetField(member);

      if (fi == null)
      {
        Builtins.SyntaxError("clr-field-set!", "field not found on type: " + type, args, member);
      }

      Expression instance = GetAst(Builtins.Third(args), cb);

      Expression value = GetAst(Builtins.Car(Builtins.LastPair(args)), cb);

      return Ast.AssignField(instance,fi, value);
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
        Builtins.SyntaxError("clr-call", "type not found", type, false);
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

      BindingFlags bf = BindingFlags.IgnoreCase | BindingFlags.Public | (ct == CallType.None ? BindingFlags.Static : BindingFlags.Instance);

      foreach (MemberInfo mi in t.GetMember(member, bf))
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
          arguments[i] = ConvertToHelper(tt, arguments[i]);
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

        return ConvertFromHelper(meth.ReturnType, r);
      }

      Builtins.SyntaxError("clr-call", "member could not be resolved on type: " + type, args, member);

      return null;
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
        Builtins.SyntaxError("clr-using", "namespace is not a symbol", name, false);
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
        Builtins.SyntaxError("clr-reference", "reference is not a symbol or a string", name, false);
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
        Builtins.SyntaxError("clr-is", "type not found", type, false);
      }

      return Ast.TypeIs(GetAst(Builtins.Second(args), cb), t);
    }
  }

  [Generator("define-clr-class-internal")]
  public class DefineClrClassInternalGenerator : ClrGenerator
  {
    // (clr-is type arg)
    public override Expression Generate(object args, CodeBlock cb)
    {
      return Ast.Null();
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
        Builtins.SyntaxError("clr-cast", "type not found", type, false);
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
        Builtins.SyntaxError("clr-new-array", "type not found", type, false);
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
        Builtins.SyntaxError("clr-new", "type not found", type, false);
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
          arguments[i] = ConvertToHelper(tt, arguments[i]);
        }
        
        Expression r = Ast.New(ci, arguments);
        return r;
      }

      Builtins.SyntaxError("clr-new", "constructor could not be resolved on type: " + type, args, false);

      return null;
    }
  }

}
