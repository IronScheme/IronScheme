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
using Microsoft.Scripting.Generation;
using Microsoft.Scripting;
using System.Reflection;
using Microsoft.Scripting.Utils;
using System.Reflection.Emit;
using System.Diagnostics;

namespace IronScheme.Runtime.R6RS
{
  class RecordTypeDescriptor
  {
    public Type type;
    public string name;
    public bool @sealed, opaque;
    public ICallable constructor;
    public MethodInfo predicate;
    public object uid;
    public bool generative;
    public RecordConstructorDescriptor rcd;

    public RecordTypeDescriptor parent;

    public List<FieldDescriptor> fields = new List<FieldDescriptor>();

    internal AssemblyGen ag;
    internal TypeGen tg;

    public IEnumerable<FieldDescriptor> GetAllFields()
    {
      if (parent != null)
      {
        foreach (FieldDescriptor fd in parent.GetAllFields())
        {
          yield return fd;
        }
      }
      foreach (FieldDescriptor fd in fields)
      {
        yield return fd;
      }
    }

    public Type Finish()
    {
      if (type is TypeBuilder)
      {
        type = tg.FinishType();

        Records.typedescriptors[type] = this;

        // this is just all wrong wrong wrong!!!
        Assembly genass = ag.DumpAndLoad();

        MethodInfo ci = type.GetMethod("make");

        constructor = Closure.Make(null, Delegate.CreateDelegate(typeof(CallTargetN), ci));

        // update fields
        predicate = type.GetMethod(predicate.Name);

        foreach (FieldDescriptor fd in fields)
        {
          fd.field = type.GetField(fd.field.Name, BindingFlags.Public | BindingFlags.Instance | BindingFlags.DeclaredOnly);
          fd.accessor = type.GetMethod(fd.accessor.Name);
          if (fd.mutable)
          {
            fd.mutator = type.GetMethod(fd.mutator.Name);
          }
        }
      }

      return type;
    }

    public override string ToString()
    {
      return string.Format("rtd: {0}", name);
    }
  }

  class FieldDescriptor
  {
    public string name;
    public bool mutable;

    public FieldInfo field;
    public MethodInfo accessor, mutator;


    public override string ToString()
    {
      return string.Format("fd: {0}", name);
    }
  }

  class RecordConstructorDescriptor
  {
    public RecordTypeDescriptor type;
    public ICallable protocol;
    public RecordConstructorDescriptor parent;
    internal CodeGen cg;

    public override string ToString()
    {
      return string.Format("rcd: {0}", type.name);
    }
  }

  public class Records : Builtins
  {
    static Dictionary<string, RecordTypeDescriptor> nongenerative = new Dictionary<string, RecordTypeDescriptor>();

    internal static Dictionary<string, ICallable> printers = new Dictionary<string, ICallable>();

    [Builtin("make-record-printer")]
    public static object MakeRecordPrinter(object typename, object proc)
    {
      string fn = SymbolTable.IdToString(RequiresNotNull<SymbolId>(typename));
      printers[fn] = RequiresNotNull<ICallable>(proc);
      return Unspecified;
    }


    [Builtin("record-type-descriptor?")]
    public static object IsRecordTypeDescriptor(object obj)
    {
      return GetBool(obj is RecordTypeDescriptor);
    }

    [Builtin("make-record-type-descriptor")]
    public static object MakeRecordTypeDescriptor(object name, object parent, object uid, object issealed, object isopaque, object fields)
    {
      string n = SymbolToString(name) as string;
      string id = uid is SymbolId ? SymbolToString(uid) as string : null;

      if (id != null)
      {
        RecordTypeDescriptor ngrtd;
        if (nongenerative.TryGetValue(n + id, out ngrtd))
        {
          return ngrtd;
        }
      }

      string assname = n;

      if (id != null)
      {
        assname = id;
      }
      else
      {
        assname = assname + "-" + Guid.NewGuid();
      }

      AssemblyGen ag = new AssemblyGen(n.Replace("/", "#"), ".", n.Replace("/", "#") + ".dll", AssemblyGenAttributes.None);

      bool @sealed = RequiresNotNull<bool>(issealed);
      bool opaque = RequiresNotNull<bool>(isopaque);

      RecordTypeDescriptor prtd = parent as RecordTypeDescriptor; // can be #f

      Type parenttype = typeof(object);

      if (prtd != null)
      {
        parenttype = prtd.Finish();
      }
      else if (n == "&condition")
      {
        parenttype = typeof(Condition);
      }

      TypeAttributes attrs = TypeAttributes.Public | TypeAttributes.Serializable;

      RecordTypeDescriptor rtd = new RecordTypeDescriptor();
      rtd.name = n;
      rtd.@sealed = @sealed;
      rtd.opaque = opaque;
      rtd.ag = ag;
      rtd.parent = prtd;
      rtd.uid = uid;
      rtd.generative = id == null;

      if (@sealed)
      {
        attrs |= TypeAttributes.Sealed;
      }

      TypeGen tg = ag.DefinePublicType(n.Replace("&", "$"), parenttype, attrs);

      rtd.tg = tg;
      rtd.type = tg.TypeBuilder;

      // predicate

      MethodBuilder pb = tg.TypeBuilder.DefineMethod(n + "?", MethodAttributes.Public | MethodAttributes.Static, 
        typeof(object), new Type[] { typeof(object) });

      ILGenerator pgen = pb.GetILGenerator();
      pgen.Emit(OpCodes.Ldarg_0);
      pgen.Emit(OpCodes.Isinst, tg.TypeBuilder);
      pgen.Emit(OpCodes.Ldnull);
      pgen.Emit(OpCodes.Cgt_Un);
      pgen.Emit(OpCodes.Call, typeof(RuntimeHelpers).GetMethod("BooleanToObject"));
      pgen.Emit(OpCodes.Ret);

      rtd.predicate = pb;

      object[] f = RequiresNotNull<object[]>(fields);

      foreach (Cons c in f)
      {
        string fname = SymbolToString(Second(c)) as string;
        // we use standard names here, they will be mapped to the given names
        string aname = n + "-" + fname;
        string mname = n + "-" + fname + "-set!";

        FieldDescriptor fd = new FieldDescriptor();
        fd.name = fname;

        FieldAttributes fattrs = FieldAttributes.Public | FieldAttributes.InitOnly;
        if ((bool)IsEqual(c.car,SymbolTable.StringToId("mutable")))
        {
          fd.mutable = true;
          fattrs &= ~FieldAttributes.InitOnly;
        }
        FieldSlot s = tg.AddField(typeof(object), fname, fattrs) as FieldSlot;

        fd.field = s.Field;

        // accesor 

        MethodBuilder ab = tg.TypeBuilder.DefineMethod(aname, MethodAttributes.Public | MethodAttributes.Static, 
          typeof(object), new Type[] { typeof(object)  });

        ILGenerator agen = ab.GetILGenerator();
        agen.Emit(OpCodes.Ldarg_0);
        agen.Emit(OpCodes.Castclass, tg.TypeBuilder);
        agen.Emit(OpCodes.Ldfld, fd.field);
        agen.Emit(OpCodes.Ret);

        fd.accessor = ab;

        // mutator
        if (fd.mutable)
        {
          MethodBuilder mb = tg.TypeBuilder.DefineMethod(mname, MethodAttributes.Public | MethodAttributes.Static, 
            typeof(object), new Type[] { typeof(object), typeof(object) });

          ILGenerator mgen = mb.GetILGenerator();
          mgen.Emit(OpCodes.Ldarg_0);
          mgen.Emit(OpCodes.Castclass, tg.TypeBuilder);
          mgen.Emit(OpCodes.Ldarg_1);
          mgen.Emit(OpCodes.Stfld, fd.field);
          mgen.Emit(OpCodes.Ldsfld, Compiler.Generator.Unspecified);
          mgen.Emit(OpCodes.Ret);

          fd.mutator = mb;
        }

        rtd.fields.Add(fd);
      }

      if (parenttype != typeof(Condition) && !parenttype.IsSubclassOf(typeof(Condition)))
      {
        CodeGen ts = tg.DefineMethodOverride(parenttype.GetMethod("ToString", Type.EmptyTypes));

        ts.EmitThis();
        ts.EmitCall(typeof(Builtins).GetMethod("WriteFormat"));
        ts.EmitReturn();
      }

      if (id != null)
      {
        nongenerative[n + id] = rtd;
      }

      if (parenttype.IsSubclassOf(typeof(Exception)))
      {
        SetSymbolValue(Context, SymbolTable.StringToId(n + "-rtd"), rtd);
      }

      return rtd;
    }

    [Builtin("make-record-constructor-descriptor")]
    public static object MakeRecordConstructorDescriptor(object rtd, object parent_constructor_descriptor, object protocol)
    {
      RecordTypeDescriptor t = RequiresNotNull<RecordTypeDescriptor>(rtd);

      if (!(t.type is TypeBuilder))
      {
        return t.rcd;
      }

      List<Type> paramtypes = new List<Type>();
      List<FieldDescriptor> allfields = new List<FieldDescriptor>(t.GetAllFields());

      int diff = allfields.Count - t.fields.Count;

      foreach (FieldDescriptor var in allfields)
	    {
	      paramtypes.Add(typeof(object));
      }

      List<Type> parenttypes = new List<Type>();

      for (int i = 0; i < diff; i++)
      {
        parenttypes.Add(typeof(object));
      }

      CodeGen cg = t.tg.DefineConstructor(paramtypes.ToArray());
      CodeGen mk = t.tg.DefineMethod(MethodAttributes.Public | MethodAttributes.Static, "make", 
        t.tg.TypeBuilder, new Type[] { typeof(object[]) }, new string[] { "args" });

      for (int i = 0; i < allfields.Count; i++)
			{
        cg.DefineParameter(i + 1, ParameterAttributes.None, allfields[i].name); 
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

      cg.Emit(OpCodes.Call,  (t.parent == null ? typeof(object) : t.parent.type).GetConstructor(parenttypes.ToArray()));


      foreach (FieldDescriptor fd in t.fields)
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

      RecordConstructorDescriptor rcd = new RecordConstructorDescriptor();
      rcd.cg = cg;
      rcd.type = t;
      rcd.protocol = protocol as ICallable;
      rcd.parent = parent_constructor_descriptor as RecordConstructorDescriptor;

      rcd.type.Finish();

      t.rcd = rcd;

      if (t.type.IsSubclassOf(typeof(Exception)))
      {
        SetSymbolValue(Context, SymbolTable.StringToId(t.name + "-rcd"), rcd);
      }
      
      return rcd;
    }

    [Builtin("record-predicate")]
    public static object RecordPredicate(object rtd)
    {
      RecordTypeDescriptor t = RequiresNotNull<RecordTypeDescriptor>(rtd);
      return Closure.Make(Context, Delegate.CreateDelegate(typeof(CallTarget1), t.predicate));
    }

    [Builtin("record-constructor")]
    public static object RecordConstructor(object cd)
    {
      RecordConstructorDescriptor ci = RequiresNotNull<RecordConstructorDescriptor>(cd);
      Type tt = ci.type.Finish();

      ICallable pp = ci.type.constructor;

      if (ci.parent != null)
      {
        CallTargetN n = null;
        n = delegate(object[] parentargs)
        {
          Type t = ci.type.Finish();

          if (ci.protocol != null)
          {
            CallTargetN m = delegate(object[] args)
            {
              if (ci.parent.protocol != null)
              {
                ICallable parent_protocol = ci.parent.protocol.Call(SymbolValue(Context, SymbolTable.StringToId("vector"))) as ICallable;
                parentargs = parent_protocol.Call(parentargs) as object[];
              }
              object[] allargs = new object[parentargs.Length + args.Length];
              Array.Copy(parentargs, allargs, parentargs.Length);
              Array.Copy(args, 0, allargs, parentargs.Length, args.Length);

              return pp.Call(allargs);
            };

            return Closure.Make(Context, m);
          }
          else
          {
            if (ci.parent.protocol != null)
            {
              ICallable parent_protocol = ci.parent.protocol.Call(SymbolValue(Context, SymbolTable.StringToId("vector"))) as ICallable;
              parentargs = parent_protocol.Call(parentargs) as object[];
            }
            return pp.Call(parentargs);
          }
        };

        ICallable nn = Closure.Make(Context, n);

        if (ci.protocol != null)
        {
          return ci.type.constructor = ci.protocol.Call(nn) as ICallable;
        }

        return nn;
      }

      if (ci.protocol != null)
      {
        return ci.type.constructor = ci.protocol.Call(pp) as ICallable;
      }

      return pp;
    }

    [Builtin("record-accessor")]
    public static object RecordAccessor(object rtd, object k)
    {
      RecordTypeDescriptor t = RequiresNotNull<RecordTypeDescriptor>(rtd);
      int i = RequiresNotNull<int>(k);

      MethodInfo am = t.fields[i].accessor;

      return Closure.Make(Context, Delegate.CreateDelegate(typeof(CallTarget1), am));
    }

    [Builtin("record-mutator")]
    public static object RecordMutator(object rtd, object k)
    {
      RecordTypeDescriptor t = RequiresNotNull<RecordTypeDescriptor>(rtd);
      int i = RequiresNotNull<int>(k);

      MethodInfo mm = t.fields[i].mutator;

      return Closure.Make(Context, Delegate.CreateDelegate(typeof(CallTarget2), mm));
    }

    internal readonly static Dictionary<Type, RecordTypeDescriptor> typedescriptors = new Dictionary<Type, RecordTypeDescriptor>();

    internal static bool IsRecordAny(object obj)
    {
      if (obj != null)
      {
        RecordTypeDescriptor rtd;
        if (typedescriptors.TryGetValue(obj.GetType(), out rtd))
        {
          return true;
        }
      }
      return false;
    }

    internal static string PrintRecord(object rec)
    {
      RecordTypeDescriptor rtd;
      if (typedescriptors.TryGetValue(rec.GetType(), out rtd))
      {
        List<string> fields = new List<string>();
        GetFields(rec, rtd, fields);
        return string.Format("#[{0}{1}]", rtd.name, string.Join("", fields.ToArray()));
      }
      return "not a record!!";
    }

    static void GetFields(object rec, RecordTypeDescriptor rtd, List<string> fields)
    {
      if (rtd.parent != null)
      {
        GetFields(rec, rtd.parent, fields);
      }
      foreach (FieldDescriptor fd in rtd.fields)
      {
        fields.Add(string.Format(" {0}:{1}", fd.name, WriteFormat(fd.accessor.Invoke(null, new object[] { rec }))));
      }
    }

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
      return FALSE;
    }

    [Builtin("record-type-name")]
    public static object RecordTypeName(object rtd)
    {
      RecordTypeDescriptor r = RequiresNotNull<RecordTypeDescriptor>(rtd);
      return r.type.Name;
    }

    [Builtin("record-type-parent")]
    public static object RecordTypeParent(object rtd)
    {
      RecordTypeDescriptor r = RequiresNotNull<RecordTypeDescriptor>(rtd);
      return r.parent ?? FALSE;
    }

    [Builtin("record-type-uid")]
    public static object RecordTypeUid(object rtd)
    {
      RecordTypeDescriptor r = RequiresNotNull<RecordTypeDescriptor>(rtd);
      return r.uid ?? FALSE;
    }

    [Builtin("record-type-generative?")]
    public static object IsRecordTypeGenerative(object rtd)
    {
      RecordTypeDescriptor r = RequiresNotNull<RecordTypeDescriptor>(rtd);
      return GetBool(r.generative);
    }

    [Builtin("record-type-sealed?")]
    public static object IsRecordTypeSealed(object rtd)
    {
      RecordTypeDescriptor r = RequiresNotNull<RecordTypeDescriptor>(rtd);
      return GetBool(r.type.IsSealed);
    }

    [Builtin("record-type-opaque?")]
    public static object IsRecordTypeOpaque(object rtd)
    {
      RecordTypeDescriptor r = RequiresNotNull<RecordTypeDescriptor>(rtd);
      return GetBool(r.opaque);
    }

    [Builtin("record-type-field-names")]
    public static object RecordTypeFieldNames(object rtd)
    {
      RecordTypeDescriptor r = RequiresNotNull<RecordTypeDescriptor>(rtd);
      List<object> names = new List<object>();

      foreach (FieldDescriptor fd in r.fields)
      {
        names.Add(SymbolTable.StringToId(fd.name));
      }
      return names.ToArray();
    }

    [Builtin("record-type-mutable?")]
    public static object IsRecordFieldMutable(object rtd, object k)
    {
      RecordTypeDescriptor r = RequiresNotNull<RecordTypeDescriptor>(rtd);
      int i = RequiresNotNull<int>(k);
      return GetBool(r.fields[i].mutable);
    }
  }
}

