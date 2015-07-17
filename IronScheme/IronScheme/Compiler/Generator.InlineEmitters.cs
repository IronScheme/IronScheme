#region License
/* Copyright (c) 2007-2015 Llewellyn Pritchard
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Collections.Generic;
using System.Reflection;
using Microsoft.Scripting;

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
