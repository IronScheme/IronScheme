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
using System.Runtime.Serialization.Formatters.Binary;
using System.Reflection.Emit;
using System.IO;
using Microsoft.Scripting.Ast;
using IronScheme.Runtime;
using System.Reflection;

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
