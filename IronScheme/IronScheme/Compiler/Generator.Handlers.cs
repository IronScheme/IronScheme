#region License
/* Copyright (c) 2007-2016 Llewellyn Pritchard
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Collections.Generic;
using System.Reflection;
using System.Text.RegularExpressions;
using Microsoft.Scripting;
using Microsoft.Scripting.Ast;

namespace IronScheme.Compiler
{
  interface IGenerator
  {
    Expression Generate(object args, CodeBlock cb);
  }

  abstract class SimpleGenerator : Generator, IGenerator
  {
    public abstract Expression Generate(object args, CodeBlock cb);
    protected internal static readonly Dictionary<SymbolId, CodeBlockExpression> libraryglobals = new Dictionary<SymbolId, CodeBlockExpression>();
    protected internal static readonly Dictionary<SymbolId, CodeBlockDescriptor[]> libraryglobalsN = new Dictionary<SymbolId, CodeBlockDescriptor[]>();
    protected internal static readonly Dictionary<SymbolId, CodeBlockExpression> libraryglobalsX = new Dictionary<SymbolId, CodeBlockExpression>();

    protected internal static readonly Dictionary<CodeBlockExpression, CodeBlockDescriptor> descriptorshack = new Dictionary<CodeBlockExpression, CodeBlockDescriptor>();
    protected internal static readonly Dictionary<Expression, CodeBlockDescriptor> descriptorshack2 = new Dictionary<Expression, CodeBlockDescriptor>();

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
      foreach (Type t in assembly.GetTypes())
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
