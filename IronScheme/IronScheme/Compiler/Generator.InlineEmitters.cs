using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting;
using System.Reflection;

namespace IronScheme.Compiler
{
  partial class Generator
  {
    readonly static Dictionary<OptimizationLevel, Dictionary<SymbolId, InlineEmitter>> inlineemitters = 
      new Dictionary<OptimizationLevel, Dictionary<SymbolId, InlineEmitter>>();

    public static OptimizationLevel Optimization = OptimizationLevel.None;

    public static void AddInlineEmitters(Type emittertype)
    {
      foreach (MethodInfo mi in emittertype.GetMethods(BindingFlags.DeclaredOnly | BindingFlags.Public | BindingFlags.Static))
      {
        foreach (InlineEmitterAttribute ba in mi.GetCustomAttributes(typeof(InlineEmitterAttribute), false))
        {
          Dictionary<SymbolId, InlineEmitter> dic;
          if (!inlineemitters.TryGetValue(ba.Optimization, out dic))
          {
            inlineemitters[ba.Optimization] = dic = new Dictionary<SymbolId, InlineEmitter>();
          }

          string name = ba.Name ?? mi.Name.ToLower();
          SymbolId s = SymbolTable.StringToId(name);

          inlineemitters[ba.Optimization][s] = Delegate.CreateDelegate(typeof(InlineEmitter), mi) as InlineEmitter;
        }
      }
    }
  }
}
