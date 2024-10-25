#region License
/* Copyright (c) 2007-2016 Llewellyn Pritchard
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Collections.Generic;
using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.InteropServices;
using IronScheme.Runtime;
using Microsoft.Scripting;
using Microsoft.Scripting.Ast;

namespace IronScheme.Compiler
{
  abstract class FFIGenerator : SimpleGenerator
  {
    protected static string Unquote(object o)
    {
      if (o is SymbolId)
      {
        var rtype = Builtins.SymbolValue(o);
        if (rtype is SymbolId)
        {
          return SymbolTable.IdToString((SymbolId)rtype);
        }
        else if (rtype is string)
        {
          return rtype as string;
        }
        else if (rtype is Cons)
        {
          Cons r = rtype as Cons;
          return "errror!";
        }
        else
        {
          return "error!";
        }
      }
      else
      {
        Cons c = o as Cons;
        c = c.cdr as Cons;
        if (c.car is string)
        {
          return c.car as string;
        }
        else if (c.car is SymbolId)
        {
          return SymbolTable.IdToString((SymbolId)c.car);
        }
        else
        {
          return "error!";
        }
      }
    }

    protected static MethodInfo ConvertTo = typeof(Helpers).GetMethod("FFIConvertTo");
    protected static MethodInfo ConvertFrom = typeof(Helpers).GetMethod("FFIConvertFrom");
    protected static MethodInfo FFIDelegate = typeof(Helpers).GetMethod("FFIDelegate");
    protected static MethodInfo FFIFunctionPointer = typeof(Helpers).GetMethod("FFIFunctionPointer");

    protected static Expression MakeConvertTo(Expression expr, Type t)
    {
      return Ast.Call(ConvertTo.MakeGenericMethod(t), expr);
    }

    protected static Expression MakeConvertFrom(Expression expr, Type t)
    {
      return Ast.Call(ConvertFrom.MakeGenericMethod(t), expr);
    }

    protected delegate Expression PInvokeHandler(Expression[] args);

    protected static CodeBlock GenerateInvoke(string proc, PInvokeHandler pinvokecall, Type returntype, List<Type> paramtypes, CodeBlock cb)
    {
      CodeBlock call = Ast.CodeBlock(proc);
      call.Parent = cb;

      int count = 0;

      List<Variable> pars = new List<Variable>();
      List<Variable> vars = new List<Variable>();

      foreach (object t in paramtypes)
      {
        var name = SymbolTable.StringToId("arg" + count++);
        Variable var = Variable.Parameter(call, name, typeof(object));
        call.AddParameter(var);

        pars.Add(var);
      }

      count = 0;

      foreach (Type t in paramtypes)
      {
        var name = SymbolTable.StringToId("par" + count++);
        Variable var = Create(name, call, t);

        vars.Add(var);
      }

      Variable ret = returntype == typeof(void) ? null : Create(SymbolTable.StringToId("ret"), call, returntype);

      List<Statement> stmts = new List<Statement>();

      for (int i = 0; i < count; i++)
      {
        var s = Ast.Write(vars[i], MakeConvertTo(Ast.Read(pars[i]), paramtypes[i]));
        stmts.Add(s);
      }

      Expression[] arguments = new Expression[vars.Count];

      for (int i = 0; i < vars.Count; i++)
      {
        arguments[i] = Ast.Read(vars[i]);
      }

      var fficall = pinvokecall(arguments);

      if (ret == null)
      {
        stmts.Add(Ast.Statement(fficall));
        stmts.Add(Ast.Return(Ast.ReadField(null, Unspecified)));
      }
      else
      {
        stmts.Add(Ast.Write(ret, fficall));
        stmts.Add(Ast.Return(MakeConvertFrom(Ast.Read(ret), returntype)));
      }

      call.Body = Ast.Block(stmts);

      return call;
    }

    protected static CodeBlock GenerateCallback(string proc, PInvokeHandler pinvokecall, Type returntype, List<Type> paramtypes, CodeBlock cb)
    {
      CodeBlock call = Ast.CodeBlock(proc, returntype);
      call.Parent = cb;

      int count = 0;

      List<Variable> pars = new List<Variable>();

      foreach (Type t in paramtypes)
      {
        var name = SymbolTable.StringToId("arg" + count++);
        Variable var = Variable.Parameter(call, name, t);
        call.AddParameter(var);

        pars.Add(var);
      }

      Variable ret = returntype == typeof(void) ? null : Create(SymbolTable.StringToId("ret"), call, typeof(object));

      List<Statement> stmts = new List<Statement>();

      Expression[] arguments = new Expression[pars.Count];

      for (int i = 0; i < pars.Count; i++)
      {
        arguments[i] = MakeConvertFrom(Ast.Read(pars[i]), paramtypes[i]);
      }

      var fficall = pinvokecall(arguments);

      if (ret == null)
      {
        stmts.Add(Ast.Statement(fficall));
      }
      else
      {
        stmts.Add(Ast.Write(ret, fficall));
        stmts.Add(Ast.Return(MakeConvertTo(Ast.Read(ret), returntype)));
      }

      call.Body = Ast.Block(stmts);

      return call;
    }

    protected Type GetFFIType(string p)
    {
      switch (p)
      {
        case "void":
          return typeof(void);
        case "intptr":
        case "void*":
          return typeof(IntPtr);
        case "int8":
          return typeof(sbyte);
        case "int16":
          return typeof(short);
        case "int":
        case "int32":
          return typeof(int);
        case "int64":
          return typeof(long);
        case "uint8":
          return typeof(byte);
        case "uint16":
          return typeof(ushort);
        case "uint32":
          return typeof(uint);
        case "uint64":
          return typeof(ulong);
        case "uintptr":
          return typeof(UIntPtr);
        case "float":
        case "float32":
          return typeof(float);
        case "double":
        case "float64":
          return typeof(double);
        case "string":
        case "char*":
          return typeof(string);
        default:
          SyntaxError("GetFFIType", "unknown ffi type", p, p);
          return typeof(void);
      }
    }

    protected static Type MakeDelegateType(Type returntype, List<Type> paramtypes)
    {
      ModuleBuilder dynamicMod = ScriptDomainManager.CurrentManager.Snippets.Assembly.ModuleBuilder;

      TypeBuilder tb = dynamicMod.DefineType("delegate-maker" + Guid.NewGuid(), TypeAttributes.Public | TypeAttributes.Sealed, typeof(MulticastDelegate));

      tb.DefineConstructor(MethodAttributes.RTSpecialName | MethodAttributes.SpecialName | MethodAttributes.Public | MethodAttributes.HideBySig, CallingConventions.Standard,
        new Type[] { typeof(object), typeof(IntPtr) }).SetImplementationFlags(MethodImplAttributes.Runtime);

      var inv = tb.DefineMethod("Invoke", MethodAttributes.Public | MethodAttributes.Virtual | MethodAttributes.NewSlot | MethodAttributes.HideBySig
       , CallingConventions.Standard ,returntype,null, new Type[] { typeof(System.Runtime.CompilerServices.CallConvCdecl)}, paramtypes.ToArray(), null, null);

      inv.SetImplementationFlags(MethodImplAttributes.Runtime);
      
      var t = tb.CreateType();
      return t;
    }
  }

  [Generator("ffi-callback-internal")]
  sealed class FFICallbackGenerator : FFIGenerator
  {
    //(import (ironscheme clr))
    //(define f (ffi-callback int32 (void* uint16)))
    public override Expression Generate(object args, CodeBlock cb)
    {
      Cons c = args as Cons;
      Type returntype = GetFFIType(Unquote(c.car));
      c = c.cdr as Cons;

      List<Type> paramtypes = new List<Type>();

      while (c != null)
      {
        paramtypes.Add(GetFFIType(Unquote(c.car)));
        c = c.cdr as Cons;
      }

      CodeBlock outer = Ast.CodeBlock("outer");
      outer.Parent = cb;

      Variable proc = Variable.Parameter(outer, SymbolTable.StringToId("proc"), typeof(object));
      outer.AddParameter(proc);

      Type sig = MakeDelegateType(returntype, paramtypes);

      CodeBlock inner = GenerateCallback("ffi-callback",
        a => MakeCallBack(proc, a),
        returntype,
        paramtypes,
        outer);

      var del = Ast.CodeBlockReference(inner, sig);

      outer.Body = Ast.Block(
        Ast.Statement(MakeClosure(inner, false)),
        Ast.Return(Ast.Call(FFIFunctionPointer, Ast.Constant(sig), del, Ast.CodeContext())));

      return MakeClosure(outer, false);
    }

    static Expression MakeCallBack(Variable proc, Expression[] a)
    {
      var procr = Ast.ConvertHelper(Ast.Read(proc), typeof(Callable));
      MethodInfo call = GetCallable(a.Length);
      var expr = Ast.Call(procr, call, a);
      return expr;
    }


  }

  [Generator("ffi-callout-internal")]
  sealed class FFICalloutGenerator : FFIGenerator
  {
    //(import (ironscheme clr))
    //(define f (ffi-callout int32 (void* uint16)))
    public override Expression Generate(object args, CodeBlock cb)
    {
      Cons c = args as Cons;
      Type returntype = GetFFIType(Unquote(c.car));
      c = c.cdr as Cons;

      List<Type> paramtypes = new List<Type>();

      while (c != null)
      {
        paramtypes.Add(GetFFIType(Unquote(c.car)));
        c = c.cdr as Cons;
      }

      CodeBlock outer = Ast.CodeBlock("outer");
      outer.Parent = cb;

      Variable ptr = Variable.Parameter(outer, SymbolTable.StringToId("ptr"), typeof(object));
      outer.AddParameter(ptr);

      CodeBlock inner = GenerateInvoke("ffi-callout",
        a => MakePointerCall(ptr, returntype, paramtypes, a),
        returntype,
        paramtypes,
        outer);

      outer.Body = Ast.Return(MakeClosure(inner, false));

      return MakeClosure(outer, false);
    }

    static Expression MakePointerCall(Variable ptr, Type returntype, List<Type> paramtypes, Expression[] a)
    {
      Type sig = MakeDelegateType(returntype, paramtypes);
      var gm = FFIDelegate.MakeGenericMethod(sig);
      var expr = Ast.Call(gm, MakeConvertTo(Ast.Read(ptr), typeof(IntPtr)));
      return Ast.Call(expr, sig.GetMethod("Invoke"), a);
    }
  }

  [Generator("pinvoke-call-internal")]
  sealed class PInvokeCallGenerator : FFIGenerator
  {
    //(import (ironscheme clr))
    //(define malloc (pinvoke-call msvcrt malloc void* (uint32)))
    public override Expression Generate(object args, CodeBlock cb)
    {
      Cons c = args as Cons;
      string lib = Unquote(c.car);
      c = c.cdr as Cons;
      string proc = Unquote(c.car);
      c = c.cdr as Cons;
      Type returntype = GetFFIType(Unquote(c.car));
      c = c.cdr as Cons;

      List<Type> paramtypes = new List<Type>();

      while (c != null)
      {
        paramtypes.Add(GetFFIType(Unquote(c.car)));
        c = c.cdr as Cons;
      }

      CodeBlock call = GenerateInvoke(proc,
        a => PInvokeCall(lib, proc, returntype, paramtypes, a), 
        returntype, 
        paramtypes, 
        cb);

      return Ast.Call(Closure_Make, Ast.CodeBlockExpression(call, false), Ast.Constant(paramtypes.Count));
  
    }

    static MethodCallExpression PInvokeCall(string lib, string proc, Type returntype, List<Type> paramtypes, Expression[] arguments)
    {
      MethodInfo m = MakePInvokeStub(lib, proc, returntype, paramtypes.ToArray());

      var fficall = Ast.Call(m, arguments);
      return fficall;
    }

 
    public static MethodInfo MakePInvokeStub(string DllPath, string EntryPoint, Type returnType, Type[] parameterTypes)
    {
      ModuleBuilder dynamicMod = ScriptDomainManager.CurrentManager.Snippets.Assembly.ModuleBuilder;

      TypeBuilder tb = dynamicMod.DefineType("pinvoke-" + EntryPoint + Guid.NewGuid(), TypeAttributes.Public);

      MethodBuilder dynamicMethod = tb.DefineMethod(EntryPoint,
        MethodAttributes.Static | MethodAttributes.Public | MethodAttributes.PinvokeImpl, CallingConventions.Standard,
        returnType, parameterTypes);

      var ci = typeof(DllImportAttribute).GetConstructor( new Type[] { typeof(string) });
      dynamicMethod.SetCustomAttribute(new CustomAttributeBuilder(ci, new object[] { DllPath }));
      
      var t = tb.CreateType();
      
      MethodInfo mi = t.GetMethod(EntryPoint);
      try
      {
        //Marshal.Prelink(mi);
      }
      catch (Exception ex)
      {
        AssertionViolation("MakePInvokeStub", ex.Message, DllPath, EntryPoint);
      }
      return mi;
    }
  }
}
