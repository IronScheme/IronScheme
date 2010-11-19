#region License
/* Copyright (c) 2007,2008,2009,2010 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Reflection;
using System.Reflection.Emit;
using IronScheme.Runtime;
using IronScheme.Runtime.R6RS;
using Microsoft.Scripting;
using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Generation;
using BigInteger = Oyster.Math.IntX;

namespace IronScheme.Compiler
{
  sealed class RecordPredicateConstant : CompilerConstant
  {
    public object RtdSymbol { get; set; }
    public SymbolId NameHint { get; set; }
    public SymbolId NameHint2 { get; set; }


    public override Type Type
    {
      get { return typeof(void); }
    }

    public override void EmitCreation(CodeGen cg)
    {
      if (Builtins.IsTrue(Builtins.IsSymbolBound(RtdSymbol)))
      {
        var rtd = Builtins.SymbolValue(RtdSymbol) as RecordTypeDescriptor;

        BoundExpression.Emitter emit = (x, y) =>
          {
            x.Emit(OpCodes.Isinst, rtd.type);
            x.Emit(OpCodes.Ldnull);
            x.Emit(OpCodes.Cgt_Un);
            x.EmitCall(typeof(RuntimeHelpers).GetMethod("BooleanToObject"), y);
          };

        BoundExpression.Fixups[NameHint] = emit;
        BoundExpression.Fixups[NameHint2] = emit;

      }
    }

    public override object Create()
    {
      throw new NotImplementedException();
    }
  }

  sealed class RecordAccessorConstant : CompilerConstant
  {
    public object RtdSymbol { get; set; }
    public int Index { get; set; }
    public SymbolId NameHint { get; set; }
    public SymbolId NameHint2 { get; set; }
    
    public override Type Type
    {
      get { return typeof(void); }
    }

    public override void EmitCreation(CodeGen cg)
    {
      if (Builtins.IsTrue(Builtins.IsSymbolBound(RtdSymbol)))
      {
        var rtd = Builtins.SymbolValue(RtdSymbol) as RecordTypeDescriptor;

        BoundExpression.Emitter emit = (x, y) =>
        {
          x.EmitCall(rtd.Fields[Index].accessor, y);
        };

        BoundExpression.Fixups[NameHint] = emit;
        BoundExpression.Fixups[NameHint2] = emit;
      }
    }

    public override object Create()
    {
      throw new NotImplementedException();
    }
  }

  sealed class RecordMutatorConstant : CompilerConstant
  {
    public object RtdSymbol { get; set; }
    public int Index { get; set; }
    public SymbolId NameHint { get; set; }
    public SymbolId NameHint2 { get; set; }

    public override Type Type
    {
      get { return typeof(void); }
    }

    public override void EmitCreation(CodeGen cg)
    {
      if (Builtins.IsTrue(Builtins.IsSymbolBound(RtdSymbol)))
      {
        var rtd = Builtins.SymbolValue(RtdSymbol) as RecordTypeDescriptor;

        BoundExpression.Emitter emit = (x, y) =>
        {
          x.EmitCall(rtd.Fields[Index].mutator, y);
        };

        BoundExpression.Fixups[NameHint] = emit;
        BoundExpression.Fixups[NameHint2] = emit;

      }
    }

    public override object Create()
    {
      throw new NotImplementedException();
    }
  }

  sealed class RecordConstructorDescriptorConstant : CompilerConstant
  {
    public object RtdSymbol { get; set; }
    public object ParentRcd { get; set; }
    public object Protocol { get; set; }
    public SymbolId NameHint { get; set; }

    public override Type Type
    {
      get { return typeof(void); }
    }

    public override void EmitCreation(CodeGen cg)
    {
      if (Builtins.IsTrue(Builtins.IsSymbolBound(RtdSymbol)))
      {
        var rtd = Builtins.SymbolValue(RtdSymbol);
        var pcd = ParentRcd;
        var prt = Builtins.SymbolValue(Protocol);
        var cd = Records.MakeRecordConstructorDescriptor(rtd, pcd, prt);

        Builtins.SetSymbolValueFast(NameHint, cd);
      }
    }

    public override object Create()
    {
      throw new NotImplementedException();
    }
  }


  sealed class RecordTypeDescriptorConstant : CompilerConstant
  {
    public object RecordName { get; set; }
    public object Sealed { get; set; }
    public object Opaque { get; set; }
    public object Uid { get; set; }
    public object Parent { get; set; }
    public object[] Fields { get; set; }
    public SymbolId NameHint { get; set; }

    public override Type Type
    {
      get { return typeof(void); }
    }

    public override void EmitCreation(CodeGen cg)
    {
      if (Parent == null || Builtins.IsTrue(Builtins.IsSymbolBound(Parent)))
      {
        var parent = Parent == null ? null : Builtins.SymbolValue(Parent);

        var rtd = Records.GenerateRecordTypeDescriptor(cg.TypeGen.AssemblyGen, RecordName, parent, Uid, Sealed, Opaque,
          Array.ConvertAll(Fields, x => ((IronSchemeConstant)x).value));

        (rtd.type as TypeBuilder).CreateType();

        Builtins.SetSymbolValueFast(NameHint, rtd);

        //Console.WriteLine("Emitting type: {0} ({1})", RecordName, NameHint);
      }
      else
      {
        //Console.WriteLine("================================================================");
        //Console.WriteLine("Parent not bound: {0} of {1}", Parent, RecordName);

        //foreach (var s in BaseHelper.cc.Scope.ModuleScope.Keys)
        //{
        //  Console.WriteLine(s);
        //}

        throw new Exception(string.Format("Parent not bound: {0} of {1}", Parent, RecordName));

      }
    }

    public override object Create()
    {
      throw new NotImplementedException();
    }
  }

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

  sealed class ComplexFractionConstant : CompilerConstant
  {
    ComplexFraction value;

    public ComplexFractionConstant(ComplexFraction f)
    {
      value = f;
    }

    public override Type Type
    {
      get { return typeof(Fraction); }
    }

    public override void EmitCreation(CodeGen cg)
    {
      cg.EmitConstant(new FractionConstant(value.Real));
      cg.EmitConstant(new FractionConstant(value.Imag));
      cg.EmitNew(typeof(ComplexFraction), new Type[] { typeof(Fraction), typeof(Fraction) });
    }

    public override object Create()
    {
      return value;
    }
  }

  sealed class IronSchemeConstant : CompilerConstant
  {
    internal readonly object value;
    readonly CodeBlock cb;
    static int constantcounter = 0;

    public IronSchemeConstant(object value, CodeBlock cb)
    {
      this.value = value;
      this.cb = cb;
    }

    public override Type Type
    {
      get { return typeof(object); }
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
