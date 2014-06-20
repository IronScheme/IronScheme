#region License
/* Copyright (c) 2007-2014 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Collections.Generic;
using System.Reflection;
using System.Reflection.Emit;
using System.Text.RegularExpressions;
using IronScheme.Compiler;
using IronScheme.Runtime.R6RS;
using Microsoft.Scripting;
using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Generation;
using System.Diagnostics;

namespace IronScheme.Runtime
{
  partial class BuiltinEmitters
  {
    [InlineEmitter("record-predicate")]
    public static Expression RecordPredicate(Expression[] obj)
    {
      if (obj.Length == 1)
      {
        var rtd = Unwrap(obj[0]);
        if (rtd is BoundExpression)
        {
          //if (!IronScheme.Compiler.Generator.VarHint.IsEmpty && IronScheme.Compiler.Generator.VarHint.ToString().Contains("stx?")) Debugger.Break();

          var rtdname = ((BoundExpression)rtd).Variable.Name;
          var e = Ast.Constant(new RecordPredicateConstant 
          { 
            RtdSymbol = rtdname, 
            NameHint = IronScheme.Compiler.Generator.VarHint, 
            NameHint2 = IronScheme.Compiler.Generator.VarHint2 
          });
          return Ast.Comma(e, Ast.Call(typeof(Records).GetMethod("RecordPredicate"), obj));
        }
      }
      return null;
    }

    [InlineEmitter("record-accessor")]
    public static Expression RecordAccessor(Expression[] obj)
    {
      if (obj.Length == 2)
      {
        var rtd = Unwrap(obj[0]);
        var index = Unwrap(obj[1]);

        if (rtd is BoundExpression && index is ConstantExpression)
        {
          var rtdname = ((BoundExpression)rtd).Variable.Name;
          var i = (int) ((ConstantExpression)index).Value;
          var e = Ast.Constant(new RecordAccessorConstant 
          { 
            RtdSymbol = rtdname, 
            Index = i, 
            NameHint = IronScheme.Compiler.Generator.VarHint ,
            NameHint2 = IronScheme.Compiler.Generator.VarHint2
          });
          return Ast.Comma(e, Ast.Call(typeof(Records).GetMethod("RecordAccessor"), obj));
        }
      }
      return null;
    }

    [InlineEmitter("record-mutator")]
    public static Expression RecordMutator(Expression[] obj)
    {
      if (obj.Length == 2)
      {
        var rtd = Unwrap(obj[0]);
        var index = Unwrap(obj[1]);

        if (rtd is BoundExpression && index is ConstantExpression)
        {
          var rtdname = ((BoundExpression)rtd).Variable.Name;
          var i = (int)((ConstantExpression)index).Value;
          var e = Ast.Constant(new RecordMutatorConstant 
          { 
            RtdSymbol = rtdname, 
            Index = i, 
            NameHint = IronScheme.Compiler.Generator.VarHint,
            NameHint2 = IronScheme.Compiler.Generator.VarHint2
          });

          return Ast.Comma(e, Ast.Call(typeof(Records).GetMethod("RecordMutator"), obj));
        }
      }
      return null;
    }

    //rather complicated... :\

    //[InlineEmitter("record-constructor")]
    //public static Expression RecordConstructor(Expression[] obj)
    //{
    //  return null;
    //}

    //[InlineEmitter("make-record-constructor-descriptor")]
    //public static Expression MakeRecordConstructorDescriptor(Expression[] obj)
    //{
    //  if (obj.Length == 3)
    //  {
    //    var rtd = Unwrap(obj[0]);
    //    var parentrcd = Unwrap(obj[1]);
    //    var protocol = Unwrap(obj[2]);

    //    if (rtd is BoundExpression && parentrcd is ConstantExpression && protocol is BoundExpression)
    //    {
    //      var rtdname = ((BoundExpression)rtd).Variable.Name;
    //      var pcdname = ((ConstantExpression)parentrcd).Value;
    //      var prtname = ((BoundExpression)protocol).Variable.Name;
    //      var e = Ast.Constant(new RecordConstructorDescriptorConstant
    //      {
    //        RtdSymbol = rtdname,
    //        ParentRcd = pcdname,
    //        Protocol = protocol,
    //        NameHint = IronScheme.Compiler.Generator.VarHint,
    //      });

    //      return Ast.Comma(e, Ast.Call(typeof(Records).GetMethod("MakeRecordConstructorDescriptor"), obj));
    //    }
    //  }
    //  return null;
    //}

    [InlineEmitter("make-record-type-descriptor")]
    public static Expression MakeRecordTypeDescriptor(Expression[] obj)
    {
      if ((obj.Length == 6 || obj.Length == 7) && IronScheme.Compiler.Generator.VarHint != SymbolId.Empty)
      {
        try
        {
          var name = Unwrap(obj[0]);
          var parent = Unwrap(obj[1]);
          var uid = Unwrap(obj[2]);
          var issealed = Unwrap(obj[3]);
          var isopaque = Unwrap(obj[4]);
          var fields = obj[5];
          if (fields is BoundExpression)
          {
            return null;
          }

          if (name is BoundExpression)
          {
            return null;
          }
          
          var rname = ((ConstantExpression)name).Value;
          var ruid = ((ConstantExpression)uid).Value;
          var rsealed = ((ConstantExpression)issealed).Value;
          var ropaque = ((ConstantExpression)isopaque).Value;

          object[] rfields = { };

          if (fields is NewArrayExpression)
          {
            var ff = ((NewArrayExpression)fields).Expressions;
            var dfields = new Expression[ff.Count];
            ff.CopyTo(dfields, 0);

            rfields = Array.ConvertAll(dfields, x => ((ConstantExpression)x).Value);
          }

          object[] tfields = Array.ConvertAll(rfields, x => SymbolTable.StringToObject("Object"));

          if (obj.Length == 7)
          {
            var ftypes = obj[6];

            if (ftypes is NewArrayExpression)
            {
              var ff = ((NewArrayExpression)ftypes).Expressions;
              var dfields = new Expression[ff.Count];
              ff.CopyTo(dfields, 0);

              tfields = Array.ConvertAll(dfields, x => ((ConstantExpression) ((UnaryExpression)x).Operand).Value);
            }

          }

          if (!Builtins.IsTrue(ruid))
          {
            ruid = Guid.NewGuid().ToString(); //TODO: recall why this was done :\ Change to gensym if possible
            obj[2] = Ast.Convert(Ast.Constant(ruid), typeof(object));
          }

          object par = null;

          if (parent is BoundExpression)
          {
            par = ((BoundExpression)parent).Variable.Name;
          }

          var rtdc = new RecordTypeDescriptorConstant
          {
            RecordName = rname,
            Uid = ruid,
            Sealed = rsealed,
            Opaque = ropaque,
            Parent = par,
            Fields = rfields,
            FieldTypes = tfields,
            NameHint = IronScheme.Compiler.Generator.VarHint,
          };

          var at = rtdc.Generate();

          if (at != null)
          {
            ClrGenerator.AddCompileTimeType(at);
          }

          var e = Ast.Constant(rtdc);

          return Ast.Comma(e, Ast.Call(typeof(Records).GetMethod("MakeRecordTypeDescriptor"), obj));
        }
        catch
        {
          throw;
          //kaboom, redirect to runtime
        }
      }

      return null;
    }
  }
}

namespace IronScheme.Runtime.R6RS
{
  [CLSCompliant(false)]
  public class RecordTypeDescriptor
  {
    internal Type type;

    public string Name { get; internal set; }

    internal bool @sealed, opaque, generative;

    public object Sealed { get { return Builtins.GetBool(@sealed); } }
    public object Opaque { get { return Builtins.GetBool(opaque); } }
    public object Generative { get { return Builtins.GetBool(generative); } }

    public Callable Constructor { get; internal set; }
    
    internal MethodInfo predicate;

    public object Predicate { get; internal set; }
    
    public object uid;
    
    internal CodeGen cg;

    internal ConstructorInfo DefaultConstructor
    {
      get
      {
        if (cg == null)
        {
          return type.GetConstructors()[0];
        }
        else
        {
          return cg.MethodBase as ConstructorInfo;
        }
      }
    }

    public RecordTypeDescriptor Parent {get; internal set;}

    internal FieldDescriptor[] fields;

    public FieldDescriptor[] Fields
    {
      get { return fields; }
    }

    internal AssemblyGen ag;
    internal TypeGen tg;

    public int TotalFieldCount
    {
      get
      {
        return fields.Length + (Parent == null ? 0 : Parent.TotalFieldCount);
      }
    }

    public IEnumerable<FieldDescriptor> GetAllFields()
    {
      if (Parent != null)
      {
        foreach (FieldDescriptor fd in Parent.GetAllFields())
        {
          yield return fd;
        }
      }
      foreach (FieldDescriptor fd in fields)
      {
        yield return fd;
      }
    }

    public static RecordTypeDescriptor Create(Type type, string name, string uid, RecordTypeDescriptor parentrtd)
    {
      var rtd = new RecordTypeDescriptor
      {
        type = type,
        Name = name,
        predicate = type.GetMethod(name + "?"),
        uid = uid,
        @sealed = type.IsSealed,
        Parent = parentrtd

      };

      Records.typedescriptors[type] = rtd;

      MethodInfo ci = type.GetMethod("make");
      var pari = ci.GetParameters();
      int pcount = pari.Length;

      if (pcount < 9 && !(pcount == 1 && pari[0].ParameterType == typeof(object[])))
      {
        rtd.Constructor = CreateCallable(ci);
      }
      else
      {
        rtd.Constructor = Closure.Create(Delegate.CreateDelegate(typeof(CallTargetN), ci)) as Callable;
      }

      rtd.Predicate = Closure.Create(Delegate.CreateDelegate(typeof(CallTarget1), rtd.predicate)) as Callable;

      var flds = new List<FieldDescriptor>();

      foreach (FieldInfo fi in type.GetFields(BindingFlags.Public | BindingFlags.Instance | BindingFlags.DeclaredOnly))
      {
        var fd = new FieldDescriptor { Name = fi.Name };

        fd.field = fi;

        var pi = type.GetProperty(fi.Name);

        fd.accessor = pi.GetGetMethod();

        fd.Accessor = CreateCallable(fd.accessor);

        if (pi.CanWrite)
        {
          fd.mutator = pi.GetSetMethod();

          fd.Mutator = CreateCallable(fd.mutator);
        }
        else
        {
          fd.Mutator = Builtins.FALSE;
        }

        flds.Add(fd);
      }

      rtd.fields = flds.ToArray();

      return rtd;
    }

    static Type[] GetTypeSpec(MethodInfo mi)
    {
      List<Type> types = new List<Type>();

      foreach (var v in mi.GetParameters())
      {
        types.Add(v.ParameterType);
      }

      types.Add(mi.ReturnType);
      return types.ToArray();
    }

    static Type GetGenericType(string typename, Type[] types)
    {
      int l = types.Length;
      var functype = ClrGenerator.GetTypeFast(typename + "`" + l).MakeGenericType(types);
      return functype;
    }


    static Type GetClosureType(MethodInfo mi)
    {
      Type[] types = GetTypeSpec(mi);

      var functype = GetGenericType("IronScheme.Runtime.Typed.TypedClosure", types);

      return functype;
    }

    static Type GetDelegateType(MethodInfo mi)
    {
      Type[] types = GetTypeSpec(mi);

      var functype = GetGenericType("IronScheme.Runtime.Typed.Func", types);

      return functype;
    }

    static Callable CreateCallable(MethodInfo mi)
    {
      var dt = GetDelegateType(mi);
      var d = Delegate.CreateDelegate(dt, mi);
      var ct = GetClosureType(mi);
      return Activator.CreateInstance(ct, d) as Callable;
    }

    public Type Finish()
    {
      if (type is TypeBuilder)
      {
        type = tg.FinishType();

        Records.typedescriptors[type] = this;

        MethodInfo ci = type.GetMethod("make");
        var pari = ci.GetParameters();
        int pcount = pari.Length;

        if (pcount < 9 && !(pcount == 1 && pari[0].ParameterType == typeof(object[])))
        {
          Constructor = CreateCallable(ci);
        }
        else
        {
          Constructor = Closure.Create(Delegate.CreateDelegate(typeof(CallTargetN), ci)) as Callable;
        }

        // update fields
        predicate = type.GetMethod(predicate.Name);
        Predicate = Closure.Create(Delegate.CreateDelegate(typeof(CallTarget1), predicate)) as Callable;

        foreach (FieldDescriptor fd in fields)
        {
          fd.field = type.GetField(fd.field.Name, BindingFlags.Public | BindingFlags.Instance | BindingFlags.DeclaredOnly);
          fd.accessor = type.GetMethod(fd.accessor.Name);

          fd.Accessor = CreateCallable(fd.accessor);

          if (fd.mutable)
          {
            fd.mutator = type.GetMethod(fd.mutator.Name);

            fd.Mutator = CreateCallable(fd.mutator);
          }
          else
          {
            fd.Mutator = Builtins.FALSE;
          }
        }
      }

      return type;
    }

    public override string ToString()
    {
      return string.Format("rtd: {0}", Name);
    }
  }

  [CLSCompliant(false)]
  public class FieldDescriptor
  {
    public string Name { get; internal set; }
    
    internal bool mutable;

    public object Mutable { get { return Builtins.GetBool(mutable); } }

    internal FieldInfo field;
    internal MethodInfo accessor, mutator;

    public Callable Accessor { get; internal set; }
    public object Mutator { get; internal set; }
    public Type Type 
    {
      get { return field.FieldType; } 
    }

    public override string ToString()
    {
      return string.Format("fd: {0}", Name);
    }
  }

  public class RecordConstructorDescriptor
  {
    public RecordTypeDescriptor type;
    public Callable protocol;
    public RecordConstructorDescriptor parent;
    internal CodeGen cg;

    public override string ToString()
    {
      return string.Format("rcd: {0}", type.Name);
    }
  }



  public class Records : Builtins
  {
    static readonly Dictionary<string, RecordTypeDescriptor> nongenerative = new Dictionary<string, RecordTypeDescriptor>();

    static Regex assnamefix = new Regex(@"[\\/:]", RegexOptions.Compiled);

    static string MakeSafe(Match m)
    {
      var c = m.Value[0];
      return string.Format("%{0:X}{1:X}", c / 16, c % 16);
    }

    [Builtin("make-record-type-descriptor")]
    public static object MakeRecordTypeDescriptor(object name, object parent, object uid, object issealed, object isopaque, object fields)
    {
      var ftypes = Array.ConvertAll((object[]) fields, x => SymbolTable.StringToObject("Object"));
      return MakeRecordTypeDescriptorTyped(name, parent, uid, issealed, isopaque, fields, ftypes);
    }
    

    [Builtin("make-record-type-descriptor")]
    public static object MakeRecordTypeDescriptorTyped(object name, object parent, object uid, object issealed, object isopaque, object fields, object fieldtypes)
    {
      AssemblyGen ag = ScriptDomainManager.Options.DebugMode ?
        ScriptDomainManager.CurrentManager.Snippets.DebugAssembly :
        ScriptDomainManager.CurrentManager.Snippets.Assembly;

      var rtd = GenerateRecordTypeDescriptor(ag, name, parent, uid, issealed, isopaque, fields, fieldtypes); 
      rtd.Finish();
      return rtd;
    }

    [CLSCompliant(false)]
    public static RecordTypeDescriptor GenerateRecordTypeDescriptor(AssemblyGen ag, object name, object parent, object uid, object issealed, object isopaque, object fields, object fieldtypes)
    {
      string n = SymbolTable.IdToString(RequiresNotNull<SymbolId>(name));
      string id = uid is SymbolId ? SymbolTable.IdToString(RequiresNotNull<SymbolId>(uid)): uid as string;

      if (id != null)
      {
        RecordTypeDescriptor ngrtd;
        if (nongenerative.TryGetValue(n + id, out ngrtd))
        {
          return ngrtd;
        }

        var type = ClrGenerator.GetTypeFast("record." + id + "." + n.Replace("&", "$").Replace("*", "$")); // TODO: Make me better

        if (type != null)
        {
          return RecordTypeDescriptor.Create(type, n, id, parent as RecordTypeDescriptor);
        }
      }

      bool @sealed = RequiresNotNull<bool>(issealed);
      bool opaque = RequiresNotNull<bool>(isopaque);

      RecordTypeDescriptor prtd = parent as RecordTypeDescriptor; // can be #f

      Type parenttype = typeof(object);

      if (prtd != null)
      {
        parenttype = prtd.type;
      }
      else if (n == "&condition")
      {
        parenttype = typeof(Condition);
      }

      TypeAttributes attrs = TypeAttributes.Public | TypeAttributes.Serializable;

      var rtd = new RecordTypeDescriptor
        {
          Name = n,
          @sealed = @sealed,
          opaque = opaque,
          ag = ag,
          Parent = prtd,
          uid = uid,
          generative = id == null || uid is string,
        };

      if (@sealed)
      {
        attrs |= TypeAttributes.Sealed;
      }

      object gid = (object)id ?? Guid.NewGuid();
      var ns = "record." + gid;
      var typename = ns + "." + n.Replace("&", "$").Replace("*", "$"); // TODO: Make me better

      TypeGen tg = ag.DefinePublicType(typename, parenttype, attrs);

      rtd.tg = tg;
      rtd.type = tg.TypeBuilder;

      if (id != null)
      {
        nongenerative[n + id] = rtd;
      }

      if (parenttype.IsSubclassOf(typeof(Condition)))
      {
        SetSymbolValueFast(SymbolTable.StringToObject(n + "-rtd"), rtd);
      }

      GeneratePredicate(n, rtd, tg);

      GenerateFields(fields, n, rtd, tg, fieldtypes);

      GenerateConstructor(rtd, tg, parenttype);

      return rtd;
    }

    static void GeneratePredicate(string n, RecordTypeDescriptor rtd, TypeGen tg)
    {
      // predicate

      MethodBuilder pb = tg.TypeBuilder.DefineMethod(n + "?", MethodAttributes.Public | MethodAttributes.Static,
        typeof(object), new Type[] { typeof(object) });

      pb.DefineParameter(1, ParameterAttributes.None, "obj");

      ILGenerator pgen = pb.GetILGenerator();
      pgen.Emit(OpCodes.Ldarg_0);
      pgen.Emit(OpCodes.Isinst, tg.TypeBuilder);
      pgen.Emit(OpCodes.Ldnull);
      pgen.Emit(OpCodes.Cgt_Un);
      pgen.Emit(OpCodes.Call, typeof(RuntimeHelpers).GetMethod("BooleanToObject"));
      pgen.Emit(OpCodes.Ret);

      rtd.predicate = pb;
    }

    static void GenerateFields(object fields, string n, RecordTypeDescriptor rtd, TypeGen tg, object fieldtypes)
    {
      object[] f = RequiresNotNull<object[]>(fields);
      object[] ftypes = RequiresNotNull<object[]>(fieldtypes);

      List<FieldDescriptor> rtd_fields = new List<FieldDescriptor>();

      for (int i = 0; i < f.Length; i++)
      {
        Cons c = (Cons) f[i];
        Type t = ClrGenerator.ExtractTypeInfo(List(SymbolTable.StringToObject("quote"), ftypes[i]));

        if (t == null)
        {
          ClrGenerator.ClrSyntaxError("GenerateFields", "type not found", ftypes[i]);
        }

        string fname = SymbolTable.IdToString(RequiresNotNull<SymbolId>(Second(c)));
        // we use standard names here, they will be mapped to the given names
        string aname = n + "-" + fname;
        string mname = n + "-" + fname + "-set!";

        var fd = new FieldDescriptor { Name = fname };

        FieldAttributes fattrs = FieldAttributes.Public | FieldAttributes.InitOnly;
        if (c.car == SymbolTable.StringToObject("mutable"))
        {
          fd.mutable = true;
          fattrs &= ~FieldAttributes.InitOnly;
        }
        FieldSlot s = tg.AddField(t, fname, fattrs) as FieldSlot;

        fd.field = s.Field;

        PropertyBuilder pi = tg.TypeBuilder.DefineProperty(fname, PropertyAttributes.None, t, new Type[0]);

        // accesor 

        MethodBuilder ab = tg.TypeBuilder.DefineMethod(aname, MethodAttributes.Public | MethodAttributes.Static,
          t, new Type[] { tg.TypeBuilder });

        ab.DefineParameter(1, ParameterAttributes.None, n);

        ILGenerator agen = ab.GetILGenerator();
        agen.Emit(OpCodes.Ldarg_0);
        //agen.Emit(OpCodes.Castclass, tg.TypeBuilder);
        agen.Emit(OpCodes.Ldfld, fd.field);
        agen.Emit(OpCodes.Ret);

        fd.accessor = ab;
        pi.SetGetMethod(ab);

        // mutator
        if (fd.mutable)
        {
          MethodBuilder mb = tg.TypeBuilder.DefineMethod(mname, MethodAttributes.Public | MethodAttributes.Static,
            typeof(object), new Type[] { tg.TypeBuilder, t });

          mb.DefineParameter(1, ParameterAttributes.None, n);

          ILGenerator mgen = mb.GetILGenerator();
          mgen.Emit(OpCodes.Ldarg_0);
          //mgen.Emit(OpCodes.Castclass, tg.TypeBuilder);
          mgen.Emit(OpCodes.Ldarg_1);
          mgen.Emit(OpCodes.Stfld, fd.field);
          mgen.Emit(OpCodes.Ldsfld, Compiler.Generator.Unspecified);
          mgen.Emit(OpCodes.Ret);

          fd.mutator = mb;
          pi.SetSetMethod(mb);
        }

        rtd_fields.Add(fd);
      }

      rtd.fields = rtd_fields.ToArray();
    }

    static void GenerateConstructor(RecordTypeDescriptor rtd, TypeGen tg, Type parenttype)
    {
      // constructor logic
      {
        List<Type> paramtypes = new List<Type>();
        List<FieldDescriptor> allfields = new List<FieldDescriptor>(rtd.GetAllFields());

        int diff = allfields.Count - rtd.Fields.Length;

        foreach (FieldDescriptor var in allfields)
        {
          paramtypes.Add(var.Type);
        }

        List<Type> parenttypes = new List<Type>();

        for (int i = 0; i < diff; i++)
        {
          parenttypes.Add(typeof(object)); //TODO: fix this, it looks broken
        }

        if (paramtypes.Count < 9)
        {
          CodeGen cg = tg.DefineConstructor(paramtypes.ToArray());

          CodeGen mk = tg.DefineMethod(MethodAttributes.Public | MethodAttributes.Static, "make",
           tg.TypeBuilder, paramtypes.ToArray(), allfields.ConvertAll(x => x.Name).ToArray());


          for (int i = 0; i < allfields.Count; i++)
          {
            cg.DefineParameter(i + 1, ParameterAttributes.None, allfields[i].Name);
          }

          int fi = 0;

          cg.EmitThis();


          for (fi = 0; fi < diff; fi++)
          {
            cg.EmitArgGet(fi);
            mk.EmitArgGet(fi);
          }

          // improve get constructor to look for protected constructors too
          cg.Emit(OpCodes.Call, (rtd.Parent == null ? parenttype.GetConstructor(Type.EmptyTypes) : rtd.Parent.DefaultConstructor));

          foreach (FieldDescriptor fd in rtd.Fields)
          {
            cg.EmitThis();
            cg.EmitArgGet(fi);
            cg.EmitFieldSet(fd.field);

            mk.EmitArgGet(fi);

            fi++;
          }

          mk.EmitNew(cg.MethodBase as ConstructorInfo);
          mk.EmitReturn();

          cg.EmitReturn();

          rtd.cg = cg;
        }
        else
        {

          CodeGen cg = tg.DefineConstructor(paramtypes.ToArray());
          CodeGen mk = tg.DefineMethod(MethodAttributes.Public | MethodAttributes.Static, "make",
           tg.TypeBuilder, new Type[] { typeof(object[]) }, new string[] { "args" });


          for (int i = 0; i < allfields.Count; i++)
          {
            cg.DefineParameter(i + 1, ParameterAttributes.None, allfields[i].Name);
          }

          int fi = 0;

          cg.EmitThis();


          for (fi = 0; fi < diff; fi++)
          {
            cg.EmitArgGet(fi);

            mk.EmitArgGet(0);
            mk.EmitConstant(fi);
            mk.Emit(OpCodes.Ldelem, typeof(object));
          }

          cg.Emit(OpCodes.Call, (rtd.Parent == null ? typeof(object).GetConstructor(Type.EmptyTypes) : rtd.Parent.DefaultConstructor));

          foreach (FieldDescriptor fd in rtd.Fields)
          {
            cg.EmitThis();
            cg.EmitArgGet(fi);
            cg.EmitFieldSet(fd.field);

            mk.EmitArgGet(0);
            mk.EmitConstant(fi);
            mk.Emit(OpCodes.Ldelem, typeof(object));

            fi++;
          }

          mk.EmitNew(cg.MethodBase as ConstructorInfo);
          mk.EmitReturn();

          cg.EmitReturn();

          rtd.cg = cg;
        }
      }
    }

    [Builtin("make-record-constructor-descriptor")]
    public static object MakeRecordConstructorDescriptor(object rtd, object parent_constructor_descriptor, object protocol)
    {
      RecordTypeDescriptor t = RequiresNotNull<RecordTypeDescriptor>(rtd);

      RecordConstructorDescriptor rcd = new RecordConstructorDescriptor();
      rcd.cg = t.cg;
      rcd.type = t;
      rcd.protocol = protocol as Callable;
      rcd.parent = parent_constructor_descriptor as RecordConstructorDescriptor;

      if (t.type.IsSubclassOf(typeof(Condition)))
      {
        SetSymbolValueFast(SymbolTable.StringToObject(t.Name + "-rcd"), rcd);
      }
      
      return rcd;
    }

    [Builtin("record-predicate")]
    public static Callable RecordPredicate(object rtd)
    {
      RecordTypeDescriptor t = RequiresNotNull<RecordTypeDescriptor>(rtd);
      return Closure.Create(Delegate.CreateDelegate(typeof(CallTarget1), t.predicate));
    }

    [Builtin("record-constructor")]
    public static Callable RecordConstructor(object cd)
    {
      RecordConstructorDescriptor ci = RequiresNotNull<RecordConstructorDescriptor>(cd);
      Type tt = ci.type.Finish();

      // this is foo.make(params object[] args) calls constructor(s).
      Callable pp = ci.type.Constructor as Callable;

      RecordConstructorDescriptor rcd = ci;

      List<Callable> init = new List<Callable>();

      while (rcd != null)
      {
        if (rcd.protocol != null)
        {
          init.Add(rcd.protocol);
        }

        rcd = rcd.parent;
      }

      if (init.Count == 0)
      {
        CallTargetN np = delegate(object[] args)
        {
          if (ci.type.TotalFieldCount != args.Length)
          {
            return AssertionViolation(ci.type.Name,
              string.Format("Incorrect number of arguments, expected {0} got {1}", ci.type.TotalFieldCount, args.Length), args);
          }
          return pp.Call(args);
        };
        return Closure.Create(np);
      }
      else
      {
        init.Reverse();

        CallTargetN np = delegate(object[] args)
        {
          Callable ppp = pp;

          List<object> allargs = new List<object>();
          int i = init.Count;
          Callable collector = null;
          CallTargetN xxx = delegate(object[] margs)
          {
            allargs.AddRange(margs);
            if (i == 0)
            {
              if (ci.type.TotalFieldCount != allargs.Count)
              {
                return AssertionViolation(ci.type.Name,
                  string.Format("Incorrect number of arguments, expected {0} got {1}", ci.type.TotalFieldCount, allargs.Count), allargs.ToArray());
              }
              return pp.Call(allargs.ToArray());
            }
            else
            {
              i--;
              return collector;
            }
          };
          ppp = collector = Closure.Create(xxx) as Callable;

          foreach (Callable ctr in init)
          {
            ppp = ctr.Call(ppp) as Callable;
          }

          object result = ppp.Call(args);

          if (result == collector)
          {
            result = collector.Call();
          }
          return result;
        };

        return Closure.Create(np);
      }
    }

    [Builtin("record-accessor")]
    public static Callable RecordAccessor(object rtd, object k)
    {
      RecordTypeDescriptor t = RequiresNotNull<RecordTypeDescriptor>(rtd);
      int i = RequiresNotNull<int>(k);

      if (i >= t.Fields.Length)
      {
        AssertionViolation("record-accessor", "invalid field index", rtd, k);
      }

      return t.fields[i].Accessor;
    }

    [Builtin("record-mutator")]
    public static object RecordMutator(object rtd, object k)
    {
      RecordTypeDescriptor t = RequiresNotNull<RecordTypeDescriptor>(rtd);
      int i = RequiresNotNull<int>(k);

      if (i >= t.Fields.Length)
      {
        AssertionViolation("record-mutator", "invalid field index", rtd, k);
      }

      return t.fields[i].Mutator;
    }

    internal readonly static Dictionary<Type, RecordTypeDescriptor> typedescriptors = new Dictionary<Type, RecordTypeDescriptor>();


    [Builtin("record?")]
    public static object IsRecord(object obj)
    {
      if (obj != null)
      {
        RecordTypeDescriptor rtd;
        if (typedescriptors.TryGetValue(obj.GetType(), out rtd) && !rtd.opaque)
        {
          return TRUE;
        }
      }
      return FALSE;
    }

    [Builtin("record-rtd")]
    public static object RecordRtd(object obj)
    {
      if (obj != null)
      {
        RecordTypeDescriptor rtd;
        if (typedescriptors.TryGetValue(obj.GetType(), out rtd) && !rtd.opaque)
        {
          return rtd;
        }
      }
      return AssertionViolation("record-rtd", "not a record", obj);
    }

  }
}

