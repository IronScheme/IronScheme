using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting;
using System.Reflection;

namespace IronScheme.Compiler
{
  partial class Generator
  {
    readonly static Dictionary<SymbolId, InlineEmitter> inlineemitters = 
      new Dictionary<SymbolId, InlineEmitter>();

    public static void AddInlineEmitters(Type emittertype)
    {
      foreach (MethodInfo mi in emittertype.GetMethods(BindingFlags.DeclaredOnly | BindingFlags.Public | BindingFlags.Static))
      {
        foreach (InlineEmitterAttribute ba in mi.GetCustomAttributes(typeof(InlineEmitterAttribute), false))
        {
          string name = ba.Name ?? mi.Name.ToLower();
          object s = SymbolTable.StringToObject(name);

          inlineemitters[(SymbolId)s] = Delegate.CreateDelegate(typeof(InlineEmitter), mi) as InlineEmitter;
        }
      }
    }
  }
}
