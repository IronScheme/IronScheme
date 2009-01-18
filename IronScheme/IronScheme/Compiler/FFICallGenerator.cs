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
using Microsoft.Scripting.Ast;
using IronScheme.Runtime;
using System.Reflection.Emit;
using System.Reflection;
using System.Runtime.InteropServices;
using Microsoft.Scripting;

namespace IronScheme.Compiler
{
  [Generator("ffi-call-internal")]
  public sealed class FFICallGenerator : SimpleGenerator
  {
    static string Unquote(object o)
    {
      Cons c = o as Cons;
      c = c.cdr as Cons;
      return c.car as string;
    }

    //(import (ironscheme clr))
    //(define malloc (ffi-callout msvcrt malloc void* (uint32)))
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

      MethodInfo m = MakePInvokeStub(lib, proc,  returntype, paramtypes.ToArray());

      CodeBlock call = Ast.CodeBlock(proc);

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
        var s = Ast.Write(vars[i], Ast.ConvertHelper(Ast.Read(pars[i]), paramtypes[i]));
        stmts.Add(s);
      }

      Expression[] arguments = new Expression[vars.Count];

      for (int i = 0; i < vars.Count; i++)
      {
        arguments[i] = Ast.Read(vars[i]);
      }

      var fficall = Ast.Call(m, arguments);

      if (ret == null)
      {
        stmts.Add(Ast.Statement(fficall));
        stmts.Add(Ast.Return(Ast.ReadField(null, Unspecified)));
      }
      else
      {
        stmts.Add(Ast.Write(ret, fficall));
        stmts.Add(Ast.Return(Ast.ConvertHelper(Ast.Read(ret), typeof(object))));
      }

      call.Body = Ast.Block(stmts);

      return Ast.Call(Closure_Make, Ast.CodeContext(), Ast.CodeBlockExpression(call, false));
    }

    private Type GetFFIType(string p)
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
        case "uintpr":
          return typeof(UIntPtr);
        case "float32":
          return typeof(float);
        case "float64":
          return typeof(double);
        default:
          Builtins.SyntaxError("GetFFIType", "unknown ffi type", p, p);
          return typeof(void);
      }
    }

    public static MethodInfo MakePInvokeStub(string DllPath, string EntryPoint, Type returnType, Type[] parameterTypes)
    {
      AssemblyName asmName = new AssemblyName(DllPath + "-" + EntryPoint);
      AssemblyBuilder dynamicAsm = AppDomain.CurrentDomain.DefineDynamicAssembly(asmName, AssemblyBuilderAccess.Run);
      ModuleBuilder dynamicMod = dynamicAsm.DefineDynamicModule(DllPath + "-" + EntryPoint);//,, DllPath + "-" + EntryPoint + ".dll");

      TypeBuilder tb = dynamicMod.DefineType("errr", TypeAttributes.Public);

      MethodBuilder dynamicMethod = tb.DefineMethod(EntryPoint,
        MethodAttributes.Static | MethodAttributes.Public | MethodAttributes.PinvokeImpl, CallingConventions.Standard,
        returnType, parameterTypes);

      var ci = typeof(DllImportAttribute).GetConstructor( new Type[] { typeof(string) });
      dynamicMethod.SetCustomAttribute(new CustomAttributeBuilder(ci, new object[] { DllPath }));
      
      var t = tb.CreateType();
      
      //dynamicAsm.Save(DllPath + "-" + EntryPoint + ".dll", PortableExecutableKinds.ILOnly, ImageFileMachine.I386);

      MethodInfo mi = t.GetMethod(EntryPoint);
      return mi;
    }
  }
}
