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
using System.Text.RegularExpressions;
using System.Collections;

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

    public Type Finish()
    {
      if (type is TypeBuilder)
      {
        type = tg.FinishType();

        Records.typedescriptors[type] = this;

        MethodInfo ci = type.GetMethod("make");
        Constructor = Closure.Create(null, Delegate.CreateDelegate(typeof(CallTargetN), ci)) as Callable;

        // update fields
        predicate = type.GetMethod(predicate.Name);
        Predicate = Closure.CreateStatic(Delegate.CreateDelegate(typeof(CallTarget1), predicate)) as Callable;

        foreach (FieldDescriptor fd in fields)
        {
          fd.field = type.GetField(fd.field.Name, BindingFlags.Public | BindingFlags.Instance | BindingFlags.DeclaredOnly);
          fd.accessor = type.GetMethod(fd.accessor.Name);

          fd.Accessor = Closure.CreateStatic(Delegate.CreateDelegate(typeof(CallTarget1), fd.accessor));

          if (fd.mutable)
          {
            fd.mutator = type.GetMethod(fd.mutator.Name);

            fd.Mutator = Closure.CreateStatic(Delegate.CreateDelegate(typeof(CallTarget2), fd.mutator));
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

    public object Accessor { get; internal set; }
    public object Mutator { get; internal set; }

    public override string ToString()
    {
      return string.Format("fd: {0}", Name);
    }
  }

  class RecordConstructorDescriptor
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
    static Dictionary<string, RecordTypeDescriptor> nongenerative = new Dictionary<string, RecordTypeDescriptor>();

    static Regex assnamefix = new Regex(@"[\\/:]", RegexOptions.Compiled);

    static string MakeSafe(Match m)
    {
      var c = m.Value[0];
      return string.Format("%{0:X}{1:X}", c / 16, c % 16);
    }

    [Builtin("make-record-type-descriptor")]
    public static object MakeRecordTypeDescriptor(object name, object parent, object uid, object issealed, object isopaque, object fields)
    {
      string n = SymbolTable.IdToString(RequiresNotNull<SymbolId>(name));
      string id = uid is SymbolId ? SymbolTable.IdToString(RequiresNotNull<SymbolId>(uid)): null;

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

      AssemblyGen ag = ScriptDomainManager.Options.DebugMode ?
        ScriptDomainManager.CurrentManager.Snippets.DebugAssembly :
        ScriptDomainManager.CurrentManager.Snippets.Assembly;

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

      RecordTypeDescriptor rtd = new RecordTypeDescriptor();
      rtd.Name = n;
      rtd.@sealed = @sealed;
      rtd.opaque = opaque;
      rtd.ag = ag;
      rtd.Parent = prtd;
      rtd.uid = uid;
      rtd.generative = id == null;

      if (@sealed)
      {
        attrs |= TypeAttributes.Sealed;
      }

      object gid = (object)id ?? Guid.NewGuid();

      TypeGen tg = ag.DefinePublicType("record." + gid + "." + n.Replace("&", "$"), parenttype, attrs);

      rtd.tg = tg;
      rtd.type = tg.TypeBuilder;

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

      object[] f = RequiresNotNull<object[]>(fields);

      List<FieldDescriptor> rtd_fields = new List<FieldDescriptor>();

      foreach (Cons c in f)
      {
        string fname = SymbolTable.IdToString(RequiresNotNull<SymbolId>(Second(c)));
        // we use standard names here, they will be mapped to the given names
        string aname = n + "-" + fname;
        string mname = n + "-" + fname + "-set!";

        FieldDescriptor fd = new FieldDescriptor();
        fd.Name = fname;

        FieldAttributes fattrs = FieldAttributes.Public | FieldAttributes.InitOnly;
        if (c.car == SymbolTable.StringToObject("mutable"))
        {
          fd.mutable = true;
          fattrs &= ~FieldAttributes.InitOnly;
        }
        FieldSlot s = tg.AddField(typeof(object), fname, fattrs) as FieldSlot;

        fd.field = s.Field;

        PropertyBuilder pi = tg.TypeBuilder.DefineProperty(fname, PropertyAttributes.None, typeof(object), new Type[0]);

        // accesor 

        MethodBuilder ab = tg.TypeBuilder.DefineMethod(aname, MethodAttributes.Public | MethodAttributes.Static,
          typeof(object), new Type[] { typeof(object) });

        ab.DefineParameter(1, ParameterAttributes.None, n);

        ILGenerator agen = ab.GetILGenerator();
        agen.Emit(OpCodes.Ldarg_0);
        agen.Emit(OpCodes.Castclass, tg.TypeBuilder);
        agen.Emit(OpCodes.Ldfld, fd.field);
        agen.Emit(OpCodes.Ret);

        fd.accessor = ab;
        pi.SetGetMethod(ab);

        // mutator
        if (fd.mutable)
        {
          MethodBuilder mb = tg.TypeBuilder.DefineMethod(mname, MethodAttributes.Public | MethodAttributes.Static,
            typeof(object), new Type[] { typeof(object), typeof(object) });

          mb.DefineParameter(1, ParameterAttributes.None, n);

          ILGenerator mgen = mb.GetILGenerator();
          mgen.Emit(OpCodes.Ldarg_0);
          mgen.Emit(OpCodes.Castclass, tg.TypeBuilder);
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

      // constructor logic
      {
        List<Type> paramtypes = new List<Type>();
        List<FieldDescriptor> allfields = new List<FieldDescriptor>(rtd.GetAllFields());

        int diff = allfields.Count - rtd.Fields.Length;

        foreach (FieldDescriptor var in allfields)
        {
          paramtypes.Add(typeof(object));
        }

        List<Type> parenttypes = new List<Type>();

        for (int i = 0; i < diff; i++)
        {
          parenttypes.Add(typeof(object));
        }

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

        cg.Emit(OpCodes.Call, (rtd.Parent == null ? typeof(object).GetConstructor(Type.EmptyTypes) : rtd.Parent.cg.MethodBase as ConstructorInfo));

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

      if (id != null)
      {
        nongenerative[n + id] = rtd;
      }

      // should be internal somehow
      if (parenttype.IsSubclassOf(typeof(Condition)))
      {
        SetSymbolValue(SymbolTable.StringToObject(n + "-rtd"), rtd);
      }

      rtd.Finish();

      return rtd;
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

      // should be internal somehow
      if (t.type.IsSubclassOf(typeof(Condition)))
      {
        SetSymbolValue(SymbolTable.StringToObject(t.Name + "-rcd"), rcd);
      }
      
      return rcd;
    }

    
    [Builtin("record-constructor")]
    public static object RecordConstructor(object cd)
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
#if CPS
        return Closure.Make(Context, OptimizedBuiltins.MakeCPS(np));
#else
        return Closure.Create(Context, np);
#endif
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
          ppp = collector = Closure.Create(Context, xxx) as Callable;

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

        return Closure.Create(Context, np);
      }
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

