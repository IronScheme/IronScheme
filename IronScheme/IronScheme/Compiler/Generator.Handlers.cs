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
using System.Text.RegularExpressions;

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

    protected static readonly Regex LOCATIONMATCH = new Regex(
      @"\((?<startline>\d+),(?<startcol>\d+)\)\s-\s\((?<endline>\d+),(?<endcol>\d+)\)",
      RegexOptions.Compiled);

    protected static SourceSpan ExtractLocation(string location)
    {
      var m = LOCATIONMATCH.Match(location);

      return new SourceSpan(
        new SourceLocation(0, Convert.ToInt32(m.Groups["startline"].Value), Convert.ToInt32(m.Groups["startcol"].Value)),
        new SourceLocation(0, Convert.ToInt32(m.Groups["endline"].Value), Convert.ToInt32(m.Groups["endcol"].Value)));
    } 

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
            object s = SymbolTable.StringToObject(name);
            cc.Scope.SetName((SymbolId)s, g);
          }
        }
      }
    }
  }
}
