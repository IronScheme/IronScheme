#region License
/* Copyright (c) 2007,2008,2009,2010,2011 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.IO;
using System.Reflection.Emit;
using System.Runtime.Serialization.Formatters.Binary;
using Microsoft.Scripting.Generation;

namespace IronScheme.Compiler
{
  sealed class SerializedConstant : CompilerConstant
  {
    readonly object value;
    static int constantcounter = 0;

    public SerializedConstant(object value)
    {
      this.value = value;
    }

    public override Type Type
    {
      get { return value.GetType(); }
    }

    static long totallength = 0;

    public override void EmitCreation(CodeGen cg)
    {
      if (cg.IsDynamicMethod)
      {
        throw new NotSupportedException("no can do");
      }
      else
      {
        ModuleBuilder mb = cg.TypeGen.TypeBuilder.Module as ModuleBuilder;

        //FieldSlot s = cg.TypeGen.AddStaticField(typeof(object), "s11n:" + constantcounter) as FieldSlot;

        MemoryStream s = new MemoryStream();

        bf.Serialize(s, value);
        s.Position = 0;
        totallength += s.Length;
        mb.DefineManifestResource("s11n:" + constantcounter, s, System.Reflection.ResourceAttributes.Public);

        cg.EmitType(cg.TypeGen.TypeBuilder);
        cg.EmitString("s11n:" + constantcounter);
        cg.EmitCall(typeof(Runtime.Helpers).GetMethod("GetConstant"));

        constantcounter++;
      }
    }

    static BinaryFormatter bf = Runtime.Helpers.bf;

    public override object Create()
    {
      return value;
    }
  }
}
