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
using Microsoft.Scripting;
using Microsoft.Scripting.Ast;
using System.Reflection;

namespace IronScheme.Compiler
{
  partial class Generator
  {
    public delegate Expression GeneratorHandler(object args, CodeBlock cb);

    readonly static Dictionary<SymbolId, GeneratorHandler> generators = new Dictionary<SymbolId, GeneratorHandler>();

    static void Add(string name, GeneratorHandler handler)
    {
      SymbolId s = SymbolTable.StringToId(name);
      Context.Scope.SetName(s, handler);
      generators[s] = handler;
    }

    public static void AddGenerators(Type generatortype)
    {
      foreach (MethodInfo mi in generatortype.GetMethods(BindingFlags.DeclaredOnly | BindingFlags.Public | BindingFlags.Static))
      {
        if (Attribute.IsDefined(mi, typeof(GeneratorAttribute)))
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
