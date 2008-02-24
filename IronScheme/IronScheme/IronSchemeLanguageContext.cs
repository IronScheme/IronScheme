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
using Microsoft.Scripting.Hosting;
using System.Reflection;
using Microsoft.Scripting.Ast;
using IronScheme.Runtime;
using IronScheme.Compiler;
using Microsoft.Scripting.Generation;
using System.IO;

using System.Diagnostics;
using System.Collections;

using Generator = IronScheme.Compiler.Generator;
using Microsoft.Scripting.Actions;

namespace IronScheme
{
  sealed class IronSchemeLanguageContext : LanguageContext
  {
    internal ScriptEngine se;
    
    public override ScriptEngine Engine
    {
      get { return se; }
    }

    public override bool IsTrue(object arg)
    {
      return Builtins.IsTrue(arg);
    }

    public override bool TryLookupName(CodeContext context, SymbolId name, out object value)
    {
      if (base.TryLookupName(context, name, out value))
      {
        return true;
      }
      if (Compiler.Generator.cc.Scope.TryLookupName(name, out value))
      {
        return true;
      }
      value = name;
      return false;
    }

    protected override Exception MissingName(SymbolId name)
    {
      Builtins.UndefinedError(name);
      return base.MissingName(name);
    }

    public override void UpdateSourceCodeProperties(CompilerContext context)
    {
      try
      {
        base.UpdateSourceCodeProperties(context);
      }
      catch (Runtime.R6RS.CompoundCondition)
      {
        context.SourceUnit.CodeProperties = SourceCodeProperties.IsIncompleteStatement;
      }
    }

    public override CodeBlock ParseSourceCode(CompilerContext context)
    {
      Stopwatch sw = Stopwatch.StartNew();

      try
      {

        switch (context.SourceUnit.Kind)
        {
          case SourceCodeKind.InteractiveCode:
            string code = context.SourceUnit.GetCode();

            if (code.Length < 10)
            {
              code = code.Trim();
            }
            if (code.Length > 0)
            {
              code = string.Format("(eval-r6rs '{0})", code.Trim());
            }

            CodeBlock cb = ParseString(code, context);
            if (cb == null && context.SourceUnit.CodeProperties == null)
            {
              context.SourceUnit.CodeProperties = SourceCodeProperties.IsIncompleteStatement;
            }
            return cb;
          case SourceCodeKind.File:
            using (Stream s = File.OpenRead(context.SourceUnit.Id))
            {
              return ParseStream(s, context);
            }
          default:
            return ParseString(context.SourceUnit.GetCode(), context);
        }
      }
      finally
      {
        Trace.WriteLine(sw.ElapsedMilliseconds, "Parse: " + context.SourceUnit);
      }
    }

    static Parser parser;
    static Scanner scanner;

    static CodeBlock ParseString(string expr, CompilerContext cc)
    {
      if (scanner == null)
      {
        scanner = new Scanner();
      }
      Scanner sc = scanner;
      sc.SourceUnit = cc.SourceUnit;
      sc.Errors = cc.Errors;
      sc.SetSource(expr, 0);
      return Parse(sc, cc);
    }

    static CodeBlock ParseStream(Stream s, CompilerContext cc)
    {
      Scanner sc = new Scanner(s);
      sc.Errors = cc.Errors;
      sc.SourceUnit = cc.SourceUnit;
      return Parse(sc, cc);
    }


    internal static object ReadExpressions(Stream code, CompilerContext cc)
    {
      if (parser == null)
      {
        parser = new Parser();
      }
      Parser p = parser;
      Scanner sc = new Scanner(code);
      p.scanner = sc;
      sc.SourceUnit = cc.SourceUnit;
      sc.Errors = cc.Errors;

      if (p.Parse())
      {
        return p.parsed;
      }
      return null;
    }

    internal static object ReadExpressions(string code, CompilerContext cc)
    {
      if (parser == null)
      {
        parser = new Parser();
      }
      Parser p = parser;
      if (scanner == null)
      {
        scanner = new Scanner();
      }
      Scanner sc = scanner;
      sc.SourceUnit = cc.SourceUnit;
      sc.Errors = cc.Errors;
      sc.SetSource(code, 0);
      p.scanner = sc;

      try
      {
        if (p.Parse())
        {
          return p.parsed;
        }
        return null;
      }
      finally
      {
        Parser.sourcemap.Clear();
      }
    }

    internal static CodeBlock Compile(Cons expr)
    {
      Cons parsed = expr;

      CodeBlock cb = Ast.CodeBlock("__script__");
      cb.IsGlobal = true;

      List<Statement> stmts = new List<Statement>();

      Compiler.Generator.FillBody(cb, stmts, parsed, true);

      Parser.sourcemap.Clear();
      return cb;
    }


    static CodeBlock Parse(Scanner sc, CompilerContext cc)
    {
      if (parser == null)
      {
        parser = new Parser();
      }
      Parser p = parser;
      p.scanner = sc;

      if (p.Parse())
      {
        return Compile(p.parsed);
      }
      Builtins.SyntaxError("parser", "expression could not be parsed", cc.SourceUnit, false);
      return null;
    }


  }
}
