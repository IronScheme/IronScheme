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

#if R6RS
using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting.Generation;
using Microsoft.Scripting;
using System.Reflection;
using Microsoft.Scripting.Utils;
using System.Reflection.Emit;

namespace IronScheme.Runtime.R6RS
{
  class RecordTypeDescriptor
  {
    public Type type;
    public bool @sealed, opaque;
    public ICallable constructor;
    public object uid;
    public bool generative;

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

        // update fields
        foreach (FieldDescriptor fd in fields)
        {
          fd.field = type.GetField(fd.name);
        }
      }

      return type;
    }

    public override string ToString()
    {
      return string.Format("rtd: {0}", type.Name);
    }
  }

  class FieldDescriptor
  {
    public string name;
    public bool mutable;

    public FieldInfo field;

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
      return string.Format("rcd: {0}", type.type.Name);
    }
  }

  public class Records : Builtins
  {
    [Builtin("record-type-descriptor?")]
    public static object IsRecordTypeDescriptor(object obj)
    {
      return obj is RecordTypeDescriptor;
    }

    [Builtin("make-record-type-descriptor")]
    public static object MakeRecordTypeDescriptor(object name, object parent, object uid, object issealed, object isopaque, object fields)
    {
      string n = SymbolToString(name) as string;
      string id = SymbolToString(uid) as string;

      string assname = n;

      if (id != null)
      {
        assname = id;
      }
      else
      {
        assname = assname + "-" + Guid.NewGuid();
      }

      AssemblyGen ag = new AssemblyGen(assname, ".", name + ".dll", AssemblyGenAttributes.SaveAndReloadAssemblies);

      bool @sealed = RequiresNotNull<bool>(issealed);
      bool opaque = RequiresNotNull<bool>(isopaque);

      RecordTypeDescriptor prtd = parent as RecordTypeDescriptor; // can be #f

      Type parenttype = typeof(object);

      if (prtd != null)
      {
        parenttype = prtd.Finish();
      }

      TypeAttributes attrs = TypeAttributes.Public;

      RecordTypeDescriptor rtd = new RecordTypeDescriptor();
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

      TypeGen tg = ag.DefinePublicType(n, parenttype, attrs);

      rtd.tg = tg;
      rtd.type = tg.TypeBuilder;

      object[] f = RequiresNotNull<object[]>(fields);

      foreach (Cons c in f)
      {
        string fname = SymbolToString(Second(c)) as string;
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

        rtd.fields.Add(fd);
      }

      return rtd;
    }

    [Builtin("make-record-constructor-descriptor")]
    public static object MakeRecordConstructorDescriptor(object rtd, object parent_constructor_descriptor, object protocol)
    {
      RecordTypeDescriptor t = RequiresNotNull<RecordTypeDescriptor>(rtd);

      List<Type> paramtypes = new List<Type>();
      List<FieldDescriptor> allfields = new List<FieldDescriptor>(t.GetAllFields());

      int diff = allfields.Count - t.fields.Count;

      foreach (object var in allfields)
	    {
	      paramtypes.Add(typeof(object));
      }

      List<Type> parenttypes = new List<Type>();

      for (int i = 0; i < diff; i++)
      {
        parenttypes.Add(typeof(object));
      }

      CodeGen cg = t.tg.DefineConstructor(paramtypes.ToArray());

      for (int i = 0; i < allfields.Count; i++)
			{
        cg.DefineParameter(i + 1, ParameterAttributes.None, allfields[i].name); 
			}

      int fi = 0;

      cg.EmitThis();

      for (fi = 0; fi < diff; fi++)
      {
        cg.EmitArgGet(fi);
      }

      cg.Emit(OpCodes.Call,  (t.parent == null ? typeof(object) : t.parent.type).GetConstructor(parenttypes.ToArray()));


      foreach (FieldDescriptor fd in t.fields)
      {
        cg.EmitThis();
        cg.EmitArgGet(fi);
        cg.EmitFieldSet(fd.field);
        fi++;
      }

      cg.EmitReturn();

      RecordConstructorDescriptor rcd = new RecordConstructorDescriptor();
      rcd.cg = cg;
      rcd.type = t;
      rcd.protocol = protocol as ICallable;
      rcd.parent = parent_constructor_descriptor as RecordConstructorDescriptor;
      
      return rcd;
    }

    [Builtin("record-predicate")]
    public static object RecordPredicate(object rtd)
    {
      RecordTypeDescriptor t = RequiresNotNull<RecordTypeDescriptor>(rtd);

      CallTarget1 p = delegate(object obj)
      {
        // TODO: opaque handling, non needed here
        if (obj == null)
        {
          return t.type == typeof(Cons);
        }

        return t.type.IsInstanceOfType(obj);
      };

      return Closure.Make(Context, p);
    }

    [Builtin("record-constructor")]
    public static object RecordConstructor(object cd)
    {
      RecordConstructorDescriptor ci = RequiresNotNull<RecordConstructorDescriptor>(cd);

      CallTargetN p = delegate(object[] args)
      {
        Type t = ci.type.Finish();

        List<Type> types = new List<Type>();

        foreach (object var in args)
        {
          types.Add(typeof(object));
        }

        ConstructorInfo csi = t.GetConstructor(types.ToArray());

        return csi.Invoke(args);
      };

      ICallable pp = Closure.Make(Context, p);

      if (ci.parent != null && ci.protocol != null)
      {
        CallTargetN n = null;
        n = delegate(object[] parentargs)
        {
          Type t = ci.type.Finish();

          CallTargetN m = delegate(object[] args)
          {
            object[] allargs = new object[parentargs.Length + args.Length];
            Array.Copy(parentargs, allargs, parentargs.Length);
            Array.Copy(args, 0, allargs, parentargs.Length, args.Length);

            return pp.Call(allargs);
          };

          return Closure.Make(Context, m);
        };

        ICallable nn = Closure.Make(Context, n);

        return ci.protocol.Call(nn);
      }

      if (ci.protocol != null)
      {
        
        return ci.type.constructor = ci.protocol.Call(pp) as ICallable;
      }

      return ci.type.constructor = pp;
    }

    [Builtin("record-accessor")]
    public static object RecordAccessor(object rtd, object k)
    {
      RecordTypeDescriptor t = RequiresNotNull<RecordTypeDescriptor>(rtd);
      int i = RequiresNotNull<int>(k);

      CallTarget1 p = delegate(object obj)
      {
        //inside the closure :)
        FieldInfo fi = t.fields[i].field;
        return fi.GetValue(obj);
      };

      return Closure.Make(Context, p);
    }

    [Builtin("record-mutator")]
    public static object RecordMutator(object rtd, object k)
    {
      RecordTypeDescriptor t = RequiresNotNull<RecordTypeDescriptor>(rtd);
      int i = RequiresNotNull<int>(k);

      CallTarget2 p = delegate(object obj, object value)
      {
        //inside the closure :)
        FieldInfo fi = t.fields[i].field;
        fi.SetValue(obj, value);
        return Unspecified;
      };

      return Closure.Make(Context, p);
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
          return true;
        }
      }
      return false;
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
      return false;
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
      return r.parent ?? (object)false;
    }

    [Builtin("record-type-uid")]
    public static object RecordTypeUid(object rtd)
    {
      RecordTypeDescriptor r = RequiresNotNull<RecordTypeDescriptor>(rtd);
      return r.uid ?? (object) false;
    }

    [Builtin("record-type-generative?")]
    public static object IsRecordTypeGenerative(object rtd)
    {
      RecordTypeDescriptor r = RequiresNotNull<RecordTypeDescriptor>(rtd);
      return r.generative;
    }

    [Builtin("record-type-sealed?")]
    public static object IsRecordTypeSealed(object rtd)
    {
      RecordTypeDescriptor r = RequiresNotNull<RecordTypeDescriptor>(rtd);
      return r.type.IsSealed;
    }

    [Builtin("record-type-opaque?")]
    public static object IsRecordTypeOpaque(object rtd)
    {
      RecordTypeDescriptor r = RequiresNotNull<RecordTypeDescriptor>(rtd);
      return r.opaque;
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
      return r.fields[i].mutable;
    }
  }
}
#endif
