using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting.Generation;
using System.Runtime.Serialization.Formatters.Binary;
using System.Reflection.Emit;
using System.IO;

namespace IronScheme.Compiler
{
  sealed class IronSchemeConstant : CompilerConstant
  {
    readonly object value;
    static int constantcounter = 0;

    public IronSchemeConstant(object value)
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
