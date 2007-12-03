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
        // this is just all wrong wrong wrong!!!
        Assembly genass = ag.DumpAndLoad();

        foreach (Type t in genass.GetExportedTypes())
        {
          if (t.Name == type.Name)
          {
            type = t;
            break;
          }
        }

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
    [Builtin("make-record-type-descriptor")]
    public static object MakeRecordTypeDescriptor(object name, object parent, object uid, object issealed, object isopaque, object fields)
    {
      string n = SymbolToString(name) as string;
      string id = null;// SymbolToString(uid) as string;

      string assname = n;

      if (id != null)
      {
        assname += id;
      }
      else
      {
        assname = assname + Guid.NewGuid();
      }

      AssemblyGen ag = new AssemblyGen(assname, ".", assname + ".dll", AssemblyGenAttributes.SaveAndReloadAssemblies);

      bool @sealed = RequiresNotNull<bool>(issealed);
      bool opaque = RequiresNotNull<bool>(isopaque);

      RecordTypeDescriptor prtd = parent as RecordTypeDescriptor; // can be #f

      //TODO: lookup for parent type

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

        FieldAttributes fattrs = FieldAttributes.Public;
        if ((bool)IsEqual(c.car,SymbolTable.StringToId("mutable")))
        {
          fd.mutable = true;
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
        // TODO: opaque handling
        if (obj == null)
        {
          return t.type == typeof(Cons);
        }

        Type otype = obj.GetType();

        return otype == t.type || t.type.IsSubclassOf(otype);
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

#if I_EVER_FIGURE_THIS_OUT
      if (ci.type.parent != null && ci.protocol != null)
      {
        CallTargetN n = null;

        if (ci.parent != null)
        {
          n = delegate(object[] args)
          {
            Type t = ci.type.Finish();
            int fc = ci.parent.type.fields.Count;

            object[] pargs = new object[fc];
            Array.Copy(args, pargs, fc);

            ci.type.parent.constructor.Call(pargs);

            if (ci.protocol != null)
            {
              return ci.protocol.Call(pp);
            }

            return pp;
          };

          return Closure.Make(Context, n);
        }
      }
#endif


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
  }
}
