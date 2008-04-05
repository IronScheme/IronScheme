using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting;
using System.Reflection;

namespace IronScheme.Compiler
{
  partial class Generator
  {
    readonly static Dictionary<SymbolId, InlineEmitter> inlineemitters = new Dictionary<SymbolId, InlineEmitter>();

    public static OptimizationLevel Optimization = OptimizationLevel.Safe;

    public static void AddInlineEmitters(Type emittertype)
    {
      Dictionary<string, MethodBase> all = new Dictionary<string, MethodBase>();

      foreach (MethodInfo mi in emittertype.GetMethods(BindingFlags.DeclaredOnly | BindingFlags.Public | BindingFlags.Static))
      {
        foreach (InlineEmitterAttribute ba in mi.GetCustomAttributes(typeof(InlineEmitterAttribute), false))
        {
          if (ba.Optimization <= Optimization)
          {
            string name = ba.Name ?? mi.Name.ToLower();
            all[name] = mi;
          }
        }
      }

      foreach (string mn in all.Keys)
      {
        SymbolId s = SymbolTable.StringToId(mn);
        inlineemitters[s] = Delegate.CreateDelegate(typeof(InlineEmitter), all[mn] as MethodInfo) as InlineEmitter;
      }
    }
  }
}
