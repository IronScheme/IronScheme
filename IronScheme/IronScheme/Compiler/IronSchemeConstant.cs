using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting.Generation;
using System.Runtime.Serialization.Formatters.Binary;
using System.Reflection.Emit;
using System.IO;
using Microsoft.Scripting.Ast;
using IronScheme.Runtime;

namespace IronScheme.Compiler
{
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

        Slot s = cg.TypeGen.AddStaticField(typeof(object), string.Format("$c${0:X4}", constantcounter++));
        
        Expression e = Generator.GetConsList(value as Cons, cb);
        e.Emit(init);
        s.EmitSet(init);

        s.EmitGet(cg);


      }
    }

    static BinaryFormatter bf = Runtime.Helpers.bf;

    public override object Create()
    {
      return value;
    }
  }
}
