using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting.Generation;
using System.Runtime.Serialization.Formatters.Binary;
using System.Reflection.Emit;
using System.IO;
using Microsoft.Scripting.Ast;
using IronScheme.Runtime;
using System.Reflection;
using Microsoft.Scripting.Math;

namespace IronScheme.Compiler
{
  sealed class FractionConstant : CompilerConstant
  {
    Fraction value;
    
    public FractionConstant(Fraction f)
    {
      value = f;
    }

    public override Type Type
    {
      get { return typeof(Fraction); }
    }

    public override void EmitCreation(CodeGen cg)
    {
      cg.EmitConstant(value.Numerator);
      cg.EmitConstant(value.Denominator);
      cg.EmitNew(typeof(Fraction), new Type[] { typeof(BigInteger), typeof(BigInteger) });
    }

    public override object Create()
    {
      return value;  
    }
  }

  sealed class IronSchemeConstant : CompilerConstant
  {
    readonly object value;
    readonly CodeBlock cb;
    static int constantcounter = 0;

    public IronSchemeConstant(object value, CodeBlock cb)
    {
      this.value = value;
      this.cb = cb;
    }

    public override Type Type
    {
      get { return value.GetType(); }
    }

    //static Dictionary<CodeGen, Dictionary<Slot, object>> cache = new Dictionary<CodeGen, Dictionary<Slot, object>>();

    //static Dictionary<Slot, object> GetCache(CodeGen cg)
    //{
    //  Dictionary<Slot, object> r;
    //  if (cache.TryGetValue(cg, out r))
    //  {
    //    return r;
    //  }
    //  return cache[cg] = new Dictionary<Slot, object>();
    //}

    //static Slot GetSlot(CodeGen cg, object value)
    //{
    //  Dictionary<Slot, object> c = GetCache(cg);

    //  foreach (KeyValuePair<Slot, object> kvp in c)
    //  {
    //    if (Builtins.IsTrue(Builtins.IsEquivalent(kvp.Value, value)))
    //    {
    //      return kvp.Key;
    //    }
    //  }

    //  return null;
    //}

    //static void AddSlot(CodeGen cg, Slot slot, object value)
    //{
    //  GetCache(cg)[slot] = value;
    //}


    public override void EmitCreation(CodeGen cg)
    {
      if (cg.IsDynamicMethod)
      {
        throw new NotSupportedException("no can do");
      }
      else
      {
        ModuleBuilder mb = cg.TypeGen.TypeBuilder.Module as ModuleBuilder;
        CodeGen init = cg.TypeGen.TypeInitializer;

        //Slot cs = GetSlot(init, value);

        //if (cs == null)
        //{
          Slot s = cg.TypeGen.AddStaticField(typeof(object), FieldAttributes.InitOnly | FieldAttributes.Private, string.Format("$c${0:X4}", constantcounter++));

          try
          {
            Generator.inconstant = true;
            Expression e = Generator.GetConsList(value as Cons, cb);

            e.Emit(init);
          }
          finally
          {
            Generator.inconstant = false;
          }

          s.EmitSet(init);
          s.EmitGet(cg);

        //  AddSlot(init, s, value);
        //}
        //else
        //{
        //  cs.EmitGet(cg);
        //}
      }
    }

    public override object Create()
    {
      return value;
    }
  }
}
