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
using System.Runtime.CompilerServices;

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

    protected static Dictionary<string, string> namespaces = ResetReferences();

    public static bool TypeHelpersEnabled { get; set; }

    protected static void ClrSyntaxError(string who, string msg, params object[] forms)
    {
      var f1 = forms.Length > 0 ? forms[0] : Builtins.FALSE;
      var f2 = forms.Length > 1 ? forms[1] : Builtins.FALSE;
      Builtins.SyntaxError(who, msg, f1, f2);
    }

    public static object SaveReferences()
    {
      var old = namespaces;
      namespaces = ResetReferences();
      foreach (var kvp in old)
      {
        namespaces[kvp.Key] = kvp.Value;
      }
      return old;
    }

    public static void ResetReferences(object prev)
    {
      namespaces = prev as Dictionary<string, string>;
    }

    static Dictionary<string, string> ResetReferences()
    {
      var namespaces = new Dictionary<string, string>(StringComparer.CurrentCultureIgnoreCase);
      namespaces.Add("System", "System");
      return namespaces;
    }

    public static Type GetTypeFast(string name)
    {
      foreach (Assembly ass in AppDomain.CurrentDomain.GetAssemblies())
      {
        if (ass.ManifestModule.Name != "<In Memory Module>")
        {
          var t = ass.GetType(name);
          if (t != null)
          {
            return t;
          }
        }
      }
      return null;
    }

    static string GetTypeName(string name, string ns)
    {
      if (string.IsNullOrEmpty(ns))
      {
        return name;
      }
      else
      {
        return ns + "." + name;
      }
    }

    protected static Type ReadType(object type)
    {
      Cons gt = type as Cons;
      if (gt != null)
      {
        var ga = new List<Type>();

        if (gt.cdr == null)
        {
          // no support for open types
          return null;
        }

        foreach (var garg in gt.cdr as Cons)
        {
          var t = ReadType(garg);
          if (t == null)
          {
            return null;
          }
          ga.Add(t);
        }

        var st = string.Format("{0}`{1}",
          SymbolTable.IdToString((SymbolId)gt.car),
          ga.Count);

        var mt = ScanForType(st);
        if (mt == null)
        {
          return null;
        }

        if (ga.Count == 0)
        {
          return mt;
        }

        var ft = mt.MakeGenericType(ga.ToArray());
        return ft;
      }
      else if (type is SymbolId)
      {
        var st = SymbolTable.IdToString((SymbolId)type);
        return ScanForType(st);
      }
      else if (type is string)
      {
        return ScanForType(type as string);
      }
      else
      {
        return null;
      }
    }

    protected static Type ScanForType(string name)
    {
      var t = GetTypeFast(GetTypeName(name, ""));
      if (t != null)
      {
        return t;
      }

      foreach (var ns in namespaces.Keys)
      {
        t = GetTypeFast(GetTypeName(name, ns));
        if (t != null)
        {
          return t;
        }
      }

      return null;
    }

    public static Type ExtractTypeInfo(object rtype)
    {
      Type t;
      string type;
      bool inferred;
      ExtractTypeInfo(rtype, out t, out type, out inferred);
      return t;
    }


    protected static void ExtractTypeInfo(object rtype, out Type t, out string type, out bool inferred)
    {
      inferred = false;
      t = null;
      type = null;

      if (rtype is SymbolId)
      {
        var tt = Builtins.SymbolValue(rtype);
        t = ReadType(tt);

        if (t == null)
        {
          ClrSyntaxError("ExtractTypeInfo", "type not found", tt);
        }
        type = t.FullName;
      }
      // quoted
      else
      {
        object stype = Builtins.Second(rtype);

        if (stype is SymbolId)
        {
          type = SymbolTable.IdToString((SymbolId)stype);
          t = ReadType(stype);

          if (t == null)
          {
            ClrSyntaxError("ExtractTypeInfo", "type not found", type);
          }

        }
        else if (stype is string)
        {
          type = stype as string;
          t = ReadType(type);

          if (t == null)
          {
            ClrSyntaxError("ExtractTypeInfo", "type not found", type);
          }

        }
        else if (stype is Cons)
        {
          t = ReadType(stype);
          type = t.FullName;

          if (t == null)
          {
            ClrSyntaxError("ExtractTypeInfo", "type not found", stype);
          }
        }
        else
        {
          type = "inferred";
          inferred = true;
        }
      }
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
      if (t.IsAssignableFrom(e.Type))
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
            // prevent boxing
            if (e is UnaryExpression && e.Type == typeof(object))
            {
              var ue = (UnaryExpression)e;
              if (t.IsAssignableFrom(ue.Operand.Type))
              {
                return ue.Operand;
              }
            }
            if (t.IsArray && t != typeof(byte[]) && t != typeof(char[]))
            {
              return Ast.SimpleCallHelper(Helpers_RequiresArray.MakeGenericMethod(t.GetElementType()), e);
            }
            else
            {
              if (e is ConstantExpression && ((ConstantExpression)e).Value == null)
              {
                return e;
              }

              if (TypeHelpersEnabled)
              {
                return Ast.SimpleCallHelper(Helpers_Requires.MakeGenericMethod(t), e);
              }
              else
              {
                return Ast.ConvertHelper(e, t);
              }
            }
          }
    }
  }

  [Generator("clr-type-of-internal")]
  public sealed class ClrTypeOfGenerator : ClrGenerator
  {
    public override Expression Generate(object args, CodeBlock cb)
    {
      var to = Builtins.First(args);
      Type t = ExtractTypeInfo(to);
      
      if (t == null)
      {
        ClrSyntaxError("clr-type-of", "type not found", to, Cons.FromList(namespaces.Keys));
      }
      return Ast.Constant(t);
    }
  }

  [Generator("clr-namespaces-internal")]
  public sealed class ClrNamespacesGenerator : ClrGenerator
  {
    public override Expression Generate(object args, CodeBlock cb)
    {
      return Ast.Constant(new IronSchemeConstant(Cons.FromList(namespaces.Keys), cb));
    }
  }

  [Generator("clr-field-get-internal")]
  public sealed class ClrFieldGetGenerator : ClrGenerator
  {
    // (clr-field-get type field-name obj )
    public override Expression Generate(object args, CodeBlock cb)
    {
      Type t;
      string type;
      bool inferred;

      object rtype = Builtins.First(args);

      ExtractTypeInfo(rtype, out t, out type, out inferred);

      string member = SymbolTable.IdToString((SymbolId)Builtins.Second(Builtins.Second(args)));

      BindingFlags bf = BindingFlags.Instance;

      Expression instance = GetAst(Builtins.Third(args), cb);

      if (instance is ConstantExpression && ((ConstantExpression)instance).Value == null)
      {
        bf = BindingFlags.Static;
        instance = null;

        if (inferred)
        {
          ClrSyntaxError("clr-field-get", "type inference not possible on static member", member);
        }
      }
      else if (inferred)
      {
        if (instance is UnaryExpression && instance.Type == typeof(object))
        {
          var ue = (UnaryExpression)instance;
          instance = ue.Operand;
        }
        t = instance.Type;
      }
      else
      {
        instance = ConvertToHelper(t, instance);
      }

      type = t.Name;

      FieldInfo fi = t.GetField(member, BindingFlags.Public | bf | BindingFlags.IgnoreCase | BindingFlags.FlattenHierarchy);

      if (fi == null)
      {
        ClrSyntaxError("clr-field-get", "field not found on type: " + type, args, member);
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
      Type t = null;
      string type = null;
      bool inferred = false;

      object rtype = Builtins.First(args);

      ExtractTypeInfo(rtype, out t, out type, out inferred);

      string member = SymbolTable.IdToString((SymbolId)Builtins.Second(Builtins.Second(args)));

      BindingFlags bf = BindingFlags.Instance;

      Expression instance = GetAst(Builtins.Third(args), cb);

      if (instance is ConstantExpression && ((ConstantExpression)instance).Value == null)
      {
        bf = BindingFlags.Static;
        instance = null;

        if (inferred)
        {
          ClrSyntaxError("clr-field-set!", "type inference not possible on static member", member);
        }
      }
      else if (inferred)
      {
        if (instance is UnaryExpression && instance.Type == typeof(object))
        {
          var ue = (UnaryExpression)instance;
          instance = ue.Operand;
        }
        t = instance.Type;

      }
      else
      {
        instance = ConvertToHelper(t, instance);
      }
      type = t.Name;


      FieldInfo fi = t.GetField(member, BindingFlags.Public | bf | BindingFlags.IgnoreCase | BindingFlags.FlattenHierarchy);

      if (fi == null)
      {
        ClrSyntaxError("clr-field-set!", "field not found on type: " + type, args);
      }

      Expression value = GetAst(Builtins.Car(Builtins.LastPair(args)), cb);

      return Ast.Comma(Ast.AssignField(instance,fi, value), Ast.ReadField(null, Unspecified));
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
      Type t = null;
      string type = null;
      bool inferred = false;

      object rtype = Builtins.First(args);

      ExtractTypeInfo(rtype, out t, out type, out inferred);

      string member = null;
      var marg = Builtins.Second(args);

      object memobj = null;
      Type[] argtypes = null;
      Type[] gentypes = null;

      if (marg is SymbolId)
      {
        var mem = Builtins.SymbolValue(marg);

        if (mem is Cons)
        {
          ExtractMethodInfo(mem as Cons, ref member, ref argtypes, ref gentypes);
        }
        else
        {
          ClrSyntaxError("clr-call", "type member not supported", mem);
        }
      }
      else
      {
        memobj = Builtins.Second(marg);

        member = memobj is SymbolId ? SymbolTable.IdToString((SymbolId)memobj) : "";

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
        else if (memobj is Cons)
        {
          ExtractMethodInfo(memobj as Cons, ref member, ref argtypes, ref gentypes);
        }
      }

      Expression instance = GetAst(Builtins.Third(args), cb);

      CallType ct = CallType.ImplicitInstance;

      if (instance is ConstantExpression && ((ConstantExpression)instance).Value == null)
      {
        ct = CallType.None;

        if (inferred)
        {
          ClrSyntaxError("clr-call", "type inference not possible on static member", member);
        }
      }
      else if (inferred)
      {
        if (instance is UnaryExpression && instance.Type == typeof(object))
        {
          var ue = (UnaryExpression)instance;
          instance = ue.Operand;
        }
        t = instance.Type;
      }
      else
      {
        instance = ConvertToHelper(t, instance);
      }

      type = t.Name;

      if (member == "get_item")
      {
        if (Attribute.IsDefined(t, typeof(DefaultMemberAttribute)))
        {
          var dma = Attribute.GetCustomAttribute(t, typeof(DefaultMemberAttribute)) as DefaultMemberAttribute;
          member = "get_" + dma.MemberName;
        }
      }
      else if (member == "set_item")
      {
        if (Attribute.IsDefined(t, typeof(DefaultMemberAttribute)))
        {
          var dma = Attribute.GetCustomAttribute(t , typeof(DefaultMemberAttribute)) as DefaultMemberAttribute;
          member = "set_" + dma.MemberName;
        }
      }

      Expression[] arguments = GetAstListNoCast(Cdddr(args) as Cons, cb);

      List<MethodBase> candidates = new List<MethodBase>();

      BindingFlags bf = BindingFlags.Public | (ct == CallType.None ? BindingFlags.Static : BindingFlags.Instance) | BindingFlags.FlattenHierarchy;

      foreach (MethodInfo mi in t.GetMember(member, MemberTypes.Method, bf))
      {
        if (mi.ContainsGenericParameters)
        {
          if (gentypes != null && mi.GetGenericArguments().Length == gentypes.Length)
          {
            candidates.Add(mi.MakeGenericMethod(gentypes));
            continue;
          }
        }
        candidates.Add(mi);    
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
              types[i] = ScanForType(typeargs[i]);
            }
          }
        }
      }
      else if (argtypes != null)
      {
        for (int i = 0; i < types.Length; i++)
        {
          types[i] = argtypes[i];
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

      ClrSyntaxError("clr-call", "member could not be resolved on type: " + type, args, member);

      return null;
    }

    private static void ExtractMethodInfo(Cons mcc, ref string member, ref Type[] argtypes, ref Type[] gentypes)
    {
      member = SymbolTable.IdToString((SymbolId)mcc.car);

      mcc = mcc.cdr as Cons;

      if (mcc.car is object[])
      {
        var gargs = new List<Type>();

        foreach (var ga in mcc.car as object[])
        {
          var tt = ReadType(ga);
          if (tt == null)
          {
            ClrSyntaxError("clr-call", "type not found", ga);
          }
          gargs.Add(tt);
        }

        gentypes = gargs.ToArray();

        mcc = mcc.cdr as Cons;
      }

      if (mcc != null)
      {
        var targs = new List<Type>();

        foreach (var arg in mcc)
        {
          var tt = ReadType(arg);
          if (tt == null)
          {
            ClrSyntaxError("clr-call", "type not found", arg);
          }
          targs.Add(tt);
        }

        argtypes = targs.ToArray();
      }
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
        ClrSyntaxError("clr-using", "namespace is not a symbol", name);
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
        ClrSyntaxError("clr-reference", "reference is not a symbol or a string", name);
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
      Type t;
      string type;
      bool inferred;

      object rtype = Builtins.First(args);

      ExtractTypeInfo(rtype, out t, out type, out inferred);

      if (t == null)
      {
        ClrSyntaxError("clr-is", "type not found", type);
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
      Type t;
      string type;
      bool inferred;

      object rtype = Builtins.First(args);

      ExtractTypeInfo(rtype, out t, out type, out inferred);

      if (t == null)
      {
        ClrSyntaxError("clr-cast", "type not found", type);
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
      Type t;
      string type;
      bool inferred;

      object rtype = Builtins.First(args);

      ExtractTypeInfo(rtype, out t, out type, out inferred);

      if (t == null)
      {
        ClrSyntaxError("clr-new-array", "type not found", type);
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
      Type t;
      string type;
      bool inferred;

      object rtype = Builtins.First(args);

      ExtractTypeInfo(rtype, out t, out type, out inferred);

      if (t == null)
      {
        ClrSyntaxError("clr-new", "type not found", type);
      }

      Expression[] arguments = GetAstListNoCast(Builtins.Cdr(args) as Cons, cb);

      List<MethodBase> candidates = new List<MethodBase>();

      foreach (ConstructorInfo c in t.GetConstructors())
      {
        bool add = true;

        foreach (var pi in c.GetParameters())
        {
          if (pi.ParameterType.IsPointer)
          {
            add = false;
            break;
          }
        }

        if (add)
        {
          candidates.Add(c);
        }
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

      ClrSyntaxError("clr-new", "constructor could not be resolved on type: " + type, args);

      return null;
    }
  }

}
