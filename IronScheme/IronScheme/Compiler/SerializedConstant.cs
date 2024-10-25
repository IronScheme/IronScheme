#region License
/* Copyright (c) 2007-2016 Llewellyn Pritchard
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.IO;
using System.Reflection;
using System.Reflection.Emit;
using Microsoft.Scripting.Generation;
using Microsoft.Scripting;
using System.Collections.Generic;
using Microsoft.Scripting.Generation.Slots;

namespace IronScheme.Compiler
{
  sealed class SerializedConstant : CompilerConstant
  {
    readonly object value;
    int index = -1;
    readonly bool compress;

    public SerializedConstant(object value)
    {
      this.value = value;
      compress = Runtime.Builtins.compressConstants;
    }

    public override Type Type
    {
      get { return typeof(object); }
    }

    Slot fs;
    LocalBuilder arrloc;

    public override void EmitCreation(CodeGen cg)
    {
      if (cg.IsDynamicMethod)
      {
        throw new NotSupportedException("no can do");
      }
      else
      {
        var tg = cg.TypeGen;
        var ag = tg.AssemblyGen;
        index = tg.ConstantCounter;
        var snippets = ScriptDomainManager.CurrentManager.Snippets;
        if (snippets.Assembly == ag || snippets.DebugAssembly == ag)
        {
          var sym = Runtime.Builtins.GenSym("$z$" + index);

          Runtime.Builtins.SetSymbolValueFast(sym, value);

          cg.EmitSymbolId((SymbolId) sym);
          cg.EmitCall(typeof(Runtime.Builtins), "SymbolValue");
        }
        else
        {
          fs = tg.AddStaticField(typeof(object), FieldAttributes.Private, "$z$" + index);
          tg.SerializedConstants.Add(this);

          var tcg = tg.TypeInitializer;

          if (index == 0)
          {
            arrloc = tcg.DeclareLocal(typeof(object[]));
            // first
            // setup deserializtion to array, then assign to fields
            tcg.EmitType(tg.TypeBuilder);
            tcg.EmitCall(typeof(Runtime.Helpers), "DeserializeAssemblyConstants");

            tcg.Emit(OpCodes.Stloc, arrloc);

            tg.CreatingType += (sender, ea) =>
            {
              var constants = new List<object>();

              foreach (SerializedConstant sc in tg.SerializedConstants)
              {
                constants.Add(sc.value);
              }

              var constantsarray = constants.ToArray();

              MemoryStream s = new MemoryStream();

              bf.Serialize(s, constantsarray);
              s.Position = 0;

              var mb = tg.TypeBuilder.Module as ModuleBuilder;
              PAL.SerializeConstants(s, mb, compress);
            };
          }

          tcg.Emit(OpCodes.Ldloc, ((SerializedConstant) tg.SerializedConstants[0]).arrloc);

          tcg.EmitInt(index);
          tcg.Emit(OpCodes.Ldelem_Ref);

          fs.EmitSet(tcg);
          fs.EmitGet(cg);
        }

        tg.ConstantCounter++;
      }
    }

    static readonly ISerializer bf = Runtime.psyntax.Serialization.SERIALIZER;

    public override object Create()
    {
      return value;
    }
  }
}
