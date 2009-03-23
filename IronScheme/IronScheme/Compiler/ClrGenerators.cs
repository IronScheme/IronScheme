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
using Microsoft.Scripting.Math;
using System.Collections;

namespace IronScheme.Compiler
{
  public abstract class ClrGenerator : SimpleGenerator
  {
    protected static MethodInfo Helpers_ConvertToDelegate = typeof(Helpers).GetMethod("ConvertToDelegate");
    protected static MethodInfo Helpers_SymbolToEnum = typeof(Helpers).GetMethod("SymbolToEnum");
    protected static MethodInfo Helpers_EnumToSymbol = typeof(Helpers).GetMethod("EnumToSymbol");
    protected static MethodInfo Helpers_Requires = typeof(Helpers).GetMethod("Requires");
    protected static MethodInfo Helpers_RequiresArray = typeof(Helpers).GetMethod("RequiresArray");
    protected static MethodInfo Helpers_RequiresNotNull = typeof(Helpers).GetMethod("RequiresNotNull");

    protected static Dictionary<string, string> namespaces = new Dictionary<string, string>(StringComparer.CurrentCultureIgnoreCase);

    static ClrGenerator()
    {
      ResetReferences();
    }

    public static void ResetReferences()
    {
      namespaces.Clear();
      namespaces.Add("System", "System");
      namespaces.Add("System.Collections", "System.Collections");
    }

    protected static Regex typeparser = new Regex(@"(?<ns>([^\s.<]+\.)*)(?<type>[^\s.<\[]+)(?<args>(<[^>]+>)?)(?<isarray>\[\])?",
      RegexOptions.Compiled | RegexOptions.ExplicitCapture); // last bit has to be greedy, greedy wont help, need to figure out nesting constructs

    //this clearly wont scale well at all...
    protected internal static Type GetType(string nsandname)
    {
      nsandname = nsandname.ToLower();
      Match m = typeparser.Match(nsandname);
      if (!m.Success)
      {
        return null;
      }

      string am = m.Groups["args"].Value;

      bool isarray = m.Groups["isarray"].Success;


      string[] genargs = am.Length > 0 ? am.Substring(1, am.Length - 2).Split('&') : new string[0];

      Type[] gentypes = Array.ConvertAll<string, Type>(genargs, GetType);

      nsandname = m.Groups["ns"].Value + m.Groups["type"].Value;
      
      foreach (Assembly ass in AppDomain.CurrentDomain.GetAssemblies())
      {
        if (ass.ManifestModule.Name != "<In Memory Module>")
        {
          try
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

              Type tt = t;

              if (nsm == nsandname)
              {
                if (gentypes.Length == genargsc && gentypes.Length > 0)
                {
                  tt = tt.MakeGenericType(gentypes);
                }
                if (isarray)
                {
                  tt = tt.MakeArrayType();
                }
                return tt;
              }
              else if (tname.ToLower() == nsandname)
              {
                if (namespaces.ContainsKey(t.Namespace))
                {
                  if (gentypes.Length == genargsc && gentypes.Length > 0)
                  {
                    tt = tt.MakeGenericType(gentypes);
                  }
                  if (isarray)
                  {
                    tt = tt.MakeArrayType();
                  }
                  return tt;
                }
              }
            }
          }
          catch (Exception) //mono
          {
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
        return Ast.SimpleCallHelper(Helpers_EnumToSymbol.MakeGenericMethod(t), e);
      }
      else if (t.IsValueType)
      {
        return Ast.ConvertHelper(e, typeof(object));
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
          return Ast.SimpleCallHelper(Helpers_ConvertToDelegate.MakeGenericMethod(t), e);
        }
        else
          if (t.BaseType == typeof(Enum))
          {
            if (e.Type.IsValueType)
            {
              e = Ast.ConvertHelper(e, typeof(object));
            }
            return Ast.SimpleCallHelper(Helpers_SymbolToEnum.MakeGenericMethod(t), e);
          }
          else
          {
            if (t.IsArray && t != typeof(byte[]))
            {
              return Ast.SimpleCallHelper(Helpers_RequiresArray.MakeGenericMethod(t.GetElementType()), e);
            }
            if (t == typeof(double) || t == typeof(int) || t == typeof(char) || t == typeof(BigInteger) || t == typeof(Complex64) ||
              t == typeof(byte) || t == typeof(sbyte) || t == typeof(float) || t == typeof(ComplexFraction) || t == typeof(Fraction) ||
              t == typeof(bool) || t == typeof(string) || t == typeof(System.IO.Stream) || t == typeof(Encoding) || t == typeof(Hashtable) ||
                t == typeof(Array) || t == typeof(byte[]) || t == typeof(ICallable))
            {
              return Ast.ConvertHelper(e, t);
            }
            else
            {
              if (e is ConstantExpression && ((ConstantExpression)e).Value == null)
              {
                return e;
              }
              return Ast.SimpleCallHelper(Helpers_Requires.MakeGenericMethod(t), e);
            }
          }
    }
  }

  [Generator("clr-field-get-internal")]
  public sealed class ClrFieldGetGenerator : ClrGenerator
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

      BindingFlags bf = BindingFlags.Instance;

      Expression instance = GetAst(Builtins.Third(args), cb);

      if (instance is ConstantExpression && ((ConstantExpression)instance).Value == null)
      {
        bf = BindingFlags.Static;
        instance = null;
      }

      FieldInfo fi = t.GetField(member, BindingFlags.Public | bf | BindingFlags.IgnoreCase);

      if (fi == null)
      {
        Builtins.SyntaxError("clr-field-get", "field not found on type: " + type, args, member);
      }

      return Ast.ReadField(instance, fi);
    }
  }

  [Generator("clr-field-set!-internal")]
  public sealed class ClrFieldSetGenerator : ClrGenerator
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

      BindingFlags bf = BindingFlags.Instance;

      Expression instance = GetAst(Builtins.Third(args), cb);

      if (instance is ConstantExpression && ((ConstantExpression)instance).Value == null)
      {
        bf = BindingFlags.Static;
        instance = null;
      }

      FieldInfo fi = t.GetField(member, BindingFlags.Public | bf | BindingFlags.IgnoreCase);

      if (fi == null)
      {
        Builtins.SyntaxError("clr-field-set!", "field not found on type: " + type, args, member);
      }

      Expression value = GetAst(Builtins.Car(Builtins.LastPair(args)), cb);

      return Ast.AssignField(instance,fi, value);
    }
  }


  [Generator("clr-call-internal")]
  public sealed class ClrCallInternalGenerator : ClrGenerator
  {
    static object Cdddr(object lst)
    {
      return Builtins.Cdr(Builtins.Cdr(Builtins.Cdr(lst)));
    }

    // (clr-call type member obj arg1 ... )
    public override Expression Generate(object args, CodeBlock cb)
    {
      string type = SymbolTable.IdToString((SymbolId)Builtins.Second(Builtins.First(args)));
      
      Type t = GetType(type);
      if (t == null)
      {
        Builtins.SyntaxError("clr-call", "type not found", type, false);
      }
      object memobj = Builtins.Second(Builtins.Second(args));

      string member = memobj is SymbolId ? SymbolTable.IdToString((SymbolId)memobj) : "";

      if (memobj is string)
      {
        string mems = memobj as string;
        int bi = mems.IndexOf('(');
        if (bi < 0)
        {
          member = mems;
        }
        else
        {
          member = mems.Substring(0, bi);
        }
      }

      Expression instance = GetAst(Builtins.Third(args), cb);

      CallType ct = CallType.ImplicitInstance;

      if (instance is ConstantExpression && ((ConstantExpression)instance).Value == null)
      {
        ct = CallType.None;
      }
      else
      {
        instance = ConvertToHelper(t, instance);
      }

      Expression[] arguments = GetAstListNoCast(Cdddr(args) as Cons, cb);

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

      if (memobj is string)
      {
        string mems = memobj as string;
        int bi = mems.IndexOf('(');
        if (bi < 0)
        {
          // do notthig
        }
        else
        {
          string[] typeargs = mems.Substring(bi + 1).TrimEnd(')').Split(',');

          for (int i = 0; i < types.Length; i++)
          {
            if (typeargs[i].Length > 0)
            {
              types[i] = GetType(typeargs[i]);
            }
          }
        }
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

  [Generator("clr-using-internal")]
  public sealed class ClrUsingInternalGenerator : ClrGenerator
  {
    // (clr-using namespace)
    public override Expression Generate(object args, CodeBlock cb)
    {
      object name = Builtins.Second(Builtins.First(args));
      string assname = null;
      if (name is SymbolId)
      {
        assname = SymbolTable.IdToString((SymbolId)name);
        namespaces[assname] = assname;
      }
      else
      {
        Builtins.SyntaxError("clr-using", "namespace is not a symbol", name, false);
      }

      return Ast.ReadField(null, Unspecified);
    }
  }

  [Generator("clr-reference-internal")]
  public sealed class ClrReferenceInternalGenerator : ClrGenerator
  {
    // (clr-reference assname)
    public override Expression Generate(object args, CodeBlock cb)
    {
      object name = Builtins.Second(Builtins.First(args));
      string assname = null;
      if (name is SymbolId)
      {
        assname = SymbolTable.IdToString((SymbolId)name);
#pragma warning disable 618
        Assembly.LoadWithPartialName(assname);
#pragma warning restore 618
      }
      else if (name is string)
      {
        assname = (string)name;
        Assembly.Load(assname);
      }
      else
      {
        Builtins.SyntaxError("clr-reference", "reference is not a symbol or a string", name, false);
      }


      return Ast.ReadField(null, Unspecified);
    }
  }

  [Generator("clr-is-internal")]
  public sealed class ClrIsInternalGenerator : ClrGenerator
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
  public sealed class DefineClrClassInternalGenerator : ClrGenerator
  {
    // (clr-is type arg)
    public override Expression Generate(object args, CodeBlock cb)
    {
      return Ast.Null();
    }
  }

  [Generator("clr-cast-internal")]
  public sealed class ClrCastInternalGenerator : ClrGenerator
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

      return ConvertToHelper(t, obj);
    }
  }

  [Generator("clr-new-array-internal")]
  public sealed class ClrNewArrayInternalGenerator : ClrGenerator
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

      Expression size = ConvertToHelper(typeof(int), GetAst(Builtins.Second(args), cb));

      ConstructorInfo ci = t.GetConstructor(new Type[] { typeof(int) });

      return Ast.New(ci, size);
    }
  }

  [Generator("clr-new-internal")]
  public sealed class ClrNewInternalGenerator : ClrGenerator
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
