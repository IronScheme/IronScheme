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
  public interface IGenerator
  {
    Expression Generate(object args, CodeBlock cb);
  }

  public abstract class SimpleGenerator : Generator, IGenerator
  {
    public abstract Expression Generate(object args, CodeBlock cb);
    protected internal static readonly Dictionary<SymbolId, CodeBlockExpression> libraryglobals = new Dictionary<SymbolId, CodeBlockExpression>();
    protected internal static readonly Dictionary<SymbolId, CodeBlockDescriptor[]> libraryglobalsN = new Dictionary<SymbolId, CodeBlockDescriptor[]>();
    protected internal static readonly Dictionary<SymbolId, CodeBlockExpression> libraryglobalsX = new Dictionary<SymbolId, CodeBlockExpression>();

    protected internal static readonly Dictionary<CodeBlockExpression, CodeBlockDescriptor> descriptorshack = new Dictionary<CodeBlockExpression, CodeBlockDescriptor>();

    internal static void ClearGlobals()
    {
      libraryglobals.Clear();
      libraryglobalsN.Clear();
      libraryglobalsX.Clear();
      descriptorshack.Clear();
    }
  }

  partial class Generator
  {
    public static void AddGenerators(CodeContext cc, Assembly assembly)
    {
      foreach (Type t in assembly.GetExportedTypes())
      {
        if (Attribute.IsDefined(t, typeof(GeneratorAttribute)))
        {
          IGenerator g = Activator.CreateInstance(t) as IGenerator;
          
          foreach (GeneratorAttribute ga in t.GetCustomAttributes(typeof(GeneratorAttribute), false))
          {
            string name = ga.Name;
            SymbolId s = SymbolTable.StringToId(name);
            cc.Scope.SetName(s, g);
          }
        }
      }
    }
  }
}
