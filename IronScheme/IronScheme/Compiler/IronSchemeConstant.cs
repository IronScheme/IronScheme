#region License
/* Copyright (c) 2007-2016 Llewellyn Pritchard
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
using System.Diagnostics;

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

        //if (NameHint.ToString().Contains("stx?")) Debugger.Break();
        BoundExpression.Fixups[NameHint] = emit;
        BoundExpression.Fixups[NameHint2] = emit;

        BoundExpression.FixupTypes[NameHint] =
        BoundExpression.FixupTypes[NameHint2] = new Type[] { typeof(object) };


      }
    }

    public override object Create()
    {
      return GetType().Name;
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
          var m = rtd.Fields[Index].accessor;
          x.EmitCall(m, y);
          if (m.ReturnType.IsValueType)
          {
            x.EmitBoxing(m.ReturnType);
          }
        };

        BoundExpression.Fixups[NameHint] = emit;
        BoundExpression.Fixups[NameHint2] = emit;

        var pars = rtd.Fields[Index].accessor.GetParameters();

        BoundExpression.FixupTypes[NameHint] =
        BoundExpression.FixupTypes[NameHint2] =
          Array.ConvertAll(pars, x => x.ParameterType);

      }
    }

    public override object Create()
    {
      return GetType().Name;
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

        var pars = rtd.Fields[Index].mutator.GetParameters();

        BoundExpression.FixupTypes[NameHint] =
        BoundExpression.FixupTypes[NameHint2] =           
          Array.ConvertAll(pars, x => x.ParameterType);

      }
    }

    public override object Create()
    {
      return GetType().Name;
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
      return GetType().Name;
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
    public object[] FieldTypes { get; set; }
    public SymbolId NameHint { get; set; }

    bool created;


    public override Type Type
    {
      get { return typeof(void); }
    }

    public override void EmitCreation(CodeGen cg)
    {
      if (created)
      {
        return;
      }

      if (Parent == null || Builtins.IsTrue(Builtins.IsSymbolBound(Parent)))
      {
        var parent = Parent == null ? null : Builtins.SymbolValue(Parent);

        var rtd = Records.GenerateRecordTypeDescriptor(cg.TypeGen.AssemblyGen, RecordName, parent, Uid, Sealed, Opaque,
          Array.ConvertAll(Fields, x => ((IronSchemeConstant)x).value), FieldTypes);

        (rtd.type as TypeBuilder).CreateType();

        Builtins.SetSymbolValueFast(NameHint, rtd);
      }
      else
      {
        throw new Exception(string.Format("Parent not bound: {0} of {1}", Parent, RecordName));
      }
    }

    public override object Create()
    {
      return GetType().Name;
    }

    public Type Generate()
    {
      var ag = Generator.CurrentAssemblyGen;

      if (Parent == null || Builtins.IsTrue(Builtins.IsSymbolBound(Parent)))
      {
        var parent = Parent == null ? null : Builtins.SymbolValue(Parent);

        var rtd = Records.GenerateRecordTypeDescriptor(ag, RecordName, parent, Uid, Sealed, Opaque,
          Array.ConvertAll(Fields, x => ((IronSchemeConstant)x).value), FieldTypes);

        var type = (rtd.type as TypeBuilder).CreateType();

        created = true;

        Builtins.SetSymbolValueFast(NameHint, rtd);

        return type;
      }
      return null; // cannot create
    }
  }

  sealed class ArrayConstant<T> : CompilerConstant where T : struct
  {
    readonly T[] value;
    readonly int size;

    public ArrayConstant(T[] value)
    {
      this.value = value;
      size = SizeOf();
    }

    public override Type Type
    {
      get { return typeof(T[]); }
    }

    int SizeOf()
    {
      switch (Type.GetTypeCode(typeof(T)))
      {
        case TypeCode.SByte:
        case TypeCode.Byte:
          return 1;
        case TypeCode.Int16:
        case TypeCode.UInt16:
        case TypeCode.Char:
          return 2;
        case TypeCode.Int32:
        case TypeCode.UInt32:
        case TypeCode.Single:
          return 4;
        case TypeCode.Int64:
        case TypeCode.UInt64:
        case TypeCode.Double:
          return 8;
        default:
          throw new NotSupportedException(string.Format("Type '{0}' not supported", typeof(T)));
      }
    }

    public override void EmitCreation(CodeGen cg)
    {
      var size = value.Length * this.size;
      if (size > 0 && size < 0x3f0000) // IL limit
      {
        byte[] data = new byte[size];
        Buffer.BlockCopy(value, 0, data, 0, size);
        var fb = cg.TypeGen.TypeBuilder.DefineInitializedData(Guid.NewGuid().ToString(), data, FieldAttributes.Static);
        cg.EmitInt(value.Length);
        cg.Emit(OpCodes.Newarr, typeof(T));
        cg.Emit(OpCodes.Dup);
        cg.Emit(OpCodes.Ldtoken, fb);
        cg.EmitCall(typeof(System.Runtime.CompilerServices.RuntimeHelpers).GetMethod("InitializeArray"));
      }
      else
      {
        cg.EmitArray(value);
      }
    }

    public override object Create()
    {
      return value;
    }
  }

  sealed class SchemeCharConstant : CompilerConstant
  {
    readonly SchemeChar value;

    public SchemeCharConstant(SchemeChar f)
    {
      value = f;
    }

    public override Type Type
    {
      get { return typeof(SchemeChar); }
    }

    public override void EmitCreation(CodeGen cg)
    {
      cg.EmitConstant(value.Value);
      cg.EmitNew(typeof(SchemeChar), new Type[] { typeof(int) });
    }

    public override object Create()
    {
      return value;
    }
  }

  sealed class FractionConstant : CompilerConstant
  {
    readonly Fraction value;
    
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
    readonly ComplexFraction value;

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

    public override void EmitCreation(CodeGen cg)
    {
      if (cg.IsDynamicMethod)
      {
        throw new NotSupportedException("no can do");
      }
      else
      {
        CodeGen init = cg.TypeGen.TypeInitializer;

        Slot s = cg.TypeGen.AddStaticField(typeof(object), FieldAttributes.InitOnly | FieldAttributes.Private, 
          string.Format("$c${0:X4}", constantcounter++));

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
      }
    }

    public override object Create()
    {
      return value;
    }
  }
}
