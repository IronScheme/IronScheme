using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting;
using Microsoft.Scripting.Ast;
using System.Reflection;

namespace IronScheme.Compiler
{
  static partial class Generator
  {
    delegate Expression GeneratorHandler(object args, CodeBlock cb);

    readonly static Dictionary<SymbolId, GeneratorHandler> generators = new Dictionary<SymbolId, GeneratorHandler>();

    static void Add(string name, GeneratorHandler handler)
    {
      generators.Add(SymbolTable.StringToId(name), handler);
    }

    public static void AddGenerators(Type generatortype)
    {
      foreach (MethodInfo mi in generatortype.GetMethods())
      {
        if (mi.IsStatic && Attribute.IsDefined(mi, typeof(GeneratorAttribute)))
        {
          GeneratorHandler gh = Delegate.CreateDelegate(typeof(GeneratorHandler), mi) as GeneratorHandler;
          foreach (GeneratorAttribute ga in mi.GetCustomAttributes(typeof(GeneratorAttribute), false))
          {
            string name = ga.Name ?? mi.Name.ToLower();
            Add(name, gh);
          }
        }
      }
    }
  }
}
