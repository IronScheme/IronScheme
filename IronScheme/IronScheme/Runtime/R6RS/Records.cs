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
using System.Text.RegularExpressions;
using IronScheme.Compiler;
using IronScheme.Runtime.R6RS;
using Microsoft.Scripting;
using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Generation;
using System.Diagnostics;
using Microsoft.Scripting.Generation.Slots;

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
    public Callable DefaultConstructor { get; private set; }
    public Callable DefaultInit { get; private set; }

    internal MethodInfo predicate;

    public object Predicate { get; internal set; }
    
    public object uid;
    
    internal CodeGen cg;

    internal ConstructorInfo GetDefaultConstructor()
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

    public static RecordTypeDescriptor Create(Type type, string name, string uid, bool opaque, RecordTypeDescriptor parentrtd)
    {
      var rtd = new RecordTypeDescriptor
      {
        type = type,
        Name = name,
        predicate = type.GetMethod(name + "?"),
        uid = uid,
        @sealed = type.IsSealed,
        opaque = opaque,
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

      rtd.DefaultConstructor = CreateCallable(type.GetMethod("$make", Type.EmptyTypes));
      rtd.DefaultInit = CreateCallable(type.GetMethod("$init"));

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

      if (types.Length > 9 || (types.Length == 2 && types[0] == typeof(object[])))
      {
        return typeof(CallTargetN);
      }

      var functype = GetGenericType("IronScheme.Runtime.Typed.Func", types);

      return functype;
    }

    static Callable CreateCallable(MethodInfo mi)
    {
      var dt = GetDelegateType(mi);
      var d = Delegate.CreateDelegate(dt, mi);
      if (d is CallTargetN)
      {
        return Closure.Create((CallTargetN)d);
      }
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

        DefaultConstructor = CreateCallable(type.GetMethod("$make", Type.EmptyTypes));
        DefaultInit = CreateCallable(type.GetMethod("$init"));

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

    internal static void ClearTypesFrom(AssemblyGen ag)
    {
      var types = new List<String>();

      foreach (var kvp in nongenerative)
      {
        if (kvp.Value.ag == ag)
        {
          types.Add(kvp.Key);
        }
      }

      foreach (var rtd in types)
      {
        nongenerative.Remove(rtd);
      }
    }

    static Regex assnamefix = new Regex(@"[\\/:]", RegexOptions.Compiled);

    static string MakeSafe(Match m)
    {
      var c = m.Value[0];
      return string.Format("%{0:X}{1:X}", c / 16, c % 16);
    }

    [Builtin("make-record-type-descriptor")]
    public static object MakeRecordTypeDescriptor(object name, object parent, object uid, object issealed, object isopaque, object fields)
    {
      var ftypes = Array.ConvertAll((object[])fields, x => SymbolTable.StringToObject("Object"));
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
      string id = uid is SymbolId ? SymbolTable.IdToString(RequiresNotNull<SymbolId>(uid)) : uid as string;
      bool opaque = RequiresNotNull<bool>(isopaque);

      if (id != null)
      {
        RecordTypeDescriptor ngrtd;
        if (nongenerative.TryGetValue(n + id, out ngrtd))
        {
          // this is all nice and well, but when the caller is from a disk assembly, after it has been compiled, there will be a mismatch
          // this is bit hard to check...
          if (ngrtd.ag == ag)
          {
            return ngrtd;
          }
        }

        var type = ClrGenerator.GetTypeFast("record." + id + "." + n.Replace("&", "$").Replace("*", "$")); // TODO: Make me better

        if (type != null)
        {
          return RecordTypeDescriptor.Create(type, n, id, opaque, parent as RecordTypeDescriptor);
        }
      }

      bool @sealed = RequiresNotNull<bool>(issealed);

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

      TypeGen tg = ag.DefineType(typename, parenttype, attrs);

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
      pgen.Emit(OpCodes.Call, typeof(RuntimeHelpers).GetMethod(nameof(RuntimeHelpers.BooleanToObject)));
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
        Cons c = (Cons)f[i];
        // check for recursive definition
        Type t = rtd.Name == SymbolTable.IdToString((SymbolId)ftypes[i]) ?
          rtd.tg.TypeBuilder :
          ClrGenerator.ExtractTypeInfo(List(SymbolTable.StringToObject("quote"), ftypes[i]));

        // can this ever be null given ExtractTypeInfo throws?
        if (t == null)
        {
          ClrGenerator.ClrSyntaxError("GenerateFields", "type not found", ftypes[i]);
        }

        string fname = SymbolTable.IdToString(RequiresNotNull<SymbolId>(Second(c)));
        // we use standard names here, they will be mapped to the given names
        string aname = n + "-" + fname;
        string mname = n + "-" + fname + "-set!";

        var fd = new FieldDescriptor { Name = fname };

        FieldAttributes fattrs = FieldAttributes.Public;// | FieldAttributes.InitOnly;
        if (c.car == SymbolTable.StringToObject("mutable"))
        {
          fd.mutable = true;
          //fattrs &= ~FieldAttributes.InitOnly;
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

    static CodeGen MakeInitMethod(RecordTypeDescriptor rtd, TypeGen tg)
    {
      var initTypes = new List<Type>();
      initTypes.Add(tg.TypeBuilder);
      initTypes.AddRange(Array.ConvertAll(rtd.Fields, f => f.Type));

      var initNames = new List<string>();
      initNames.Add("$this");
      initNames.AddRange(Array.ConvertAll(rtd.Fields, f => f.Name));

      CodeGen init = null;

      if (rtd.Fields.Length == 0)
      {
        init = tg.DefineMethod(MethodAttributes.Public | MethodAttributes.Static, "$init", tg.TypeBuilder, initTypes.ToArray(), initNames.ToArray());
        init.EmitArgGet(0);
      }
      else if (initTypes.Count < 9)
      {
        init = tg.DefineMethod(MethodAttributes.Public | MethodAttributes.Static, "$init", tg.TypeBuilder, initTypes.ToArray(), initNames.ToArray());

        int ii = 1;

        foreach (FieldDescriptor fd in rtd.Fields)
        {
          init.EmitArgGet(0);
          init.EmitArgGet(ii);
          init.EmitFieldSet(fd.field);
          ii++;
        }

        init.EmitArgGet(0);
      }
      else
      {
        init = tg.DefineMethod(MethodAttributes.Public | MethodAttributes.Static, "$init", tg.TypeBuilder, new Type[] { typeof(object[]) }, new string[] { "args" });

        var local = init.DeclareLocal(tg.TypeBuilder);
        init.EmitArgGet(0);
        init.EmitConstant(0);
        init.Emit(OpCodes.Ldelem, typeof(object));
        init.Emit(OpCodes.Castclass, tg.TypeBuilder);
        init.Emit(OpCodes.Stloc, local);

        int ii = 1;

        foreach (FieldDescriptor fd in rtd.Fields)
        {
          init.Emit(OpCodes.Ldloc, local);

          init.EmitArgGet(0);
          init.EmitConstant(ii);
          init.Emit(OpCodes.Ldelem, typeof(object));

          init.EmitFieldSet(fd.field);
          ii++;
        }

        init.Emit(OpCodes.Ldloc, local);
      }

      init.EmitReturn();

      return init;
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

        var dctor = tg.DefineConstructor(Type.EmptyTypes);
        dctor.EmitThis();
        dctor.Emit(OpCodes.Call, parenttype.GetConstructor(Type.EmptyTypes));
        dctor.EmitReturn();

        CodeGen dmk = tg.DefineMethod(MethodAttributes.Public | MethodAttributes.Static, "$make",
           tg.TypeBuilder, Type.EmptyTypes, new string[0]);
        dmk.EmitNew(dctor.MethodBase as ConstructorInfo);
        dmk.EmitReturn();

        CodeGen init = MakeInitMethod(rtd, tg);

        if (paramtypes.Count == 0)
        {
          CodeGen mk = tg.DefineMethod(MethodAttributes.Public | MethodAttributes.Static, "make",
           tg.TypeBuilder, paramtypes.ToArray(), allfields.ConvertAll(x => x.Name).ToArray());

          int fi = 0;

          for (fi = 0; fi < diff; fi++)
          {
            mk.EmitArgGet(fi);
          }

          foreach (FieldDescriptor fd in rtd.Fields)
          {
            mk.EmitArgGet(fi);

            fi++;
          }

          mk.EmitNew(dctor.MethodBase as ConstructorInfo);
          mk.EmitReturn();

          rtd.cg = dctor;
        }
        else if (paramtypes.Count < 9)
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
          cg.Emit(OpCodes.Call, (rtd.Parent == null ? parenttype.GetConstructor(Type.EmptyTypes) : rtd.Parent.GetDefaultConstructor()));

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

          cg.Emit(OpCodes.Call, (rtd.Parent == null ? typeof(object).GetConstructor(Type.EmptyTypes) : rtd.Parent.GetDefaultConstructor()));

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

    static Callable MakeProtocolCallChain(RecordConstructorDescriptor rcd, object instance)
    {
      CallTargetN ipc = delegate (object[] iargs)
      {
        var nargs = new List<object>(iargs.Length + 1);
        nargs.Add(instance);
        nargs.AddRange(iargs);
        return rcd.type.DefaultInit.Call(nargs.ToArray());
      };

      CallTargetN ppp = ipc;

      if (rcd.parent != null)
      {
        var parent = MakeProtocolCallChain(rcd.parent, instance);

        CallTargetN rr = delegate (object[] args)
        {
          parent.Call(args);
          return Closure.Create(ipc);
        };

        ppp = rr;
      }

      CallTargetN pc = delegate (object[] args)
      {
        var ppc = Closure.Create(ppp);

        if (rcd.protocol != null)
        {
          ppc = ((Callable)rcd.protocol.Call(ppc));
        }

        ppc.Call(args);

        return instance;
      };

      return Closure.Create(pc);
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
        CallTargetN rc = delegate (object[] args)
        {
          var instance = ci.type.DefaultConstructor.Call();
          var protochain = MakeProtocolCallChain(ci, instance);

          protochain.Call(args);

          return instance;
        };

        return Closure.Create(rc);
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

