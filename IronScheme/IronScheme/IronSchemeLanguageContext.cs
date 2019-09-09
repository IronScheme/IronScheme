#region License
/* Copyright (c) 2007-2016 Llewellyn Pritchard
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Collections.Generic;
using System.IO;
using IronScheme.Compiler;
using IronScheme.Runtime;
using Microsoft.Scripting;
using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Hosting;

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

    protected override Exception MissingName(SymbolId name)
    {
      Builtins.UndefinedError(name);
      return base.MissingName(name);
    }

    public override void UpdateSourceCodeProperties(CompilerContext context)
    {
      if (parser == null)
      {
        parser = new Parser();
      }
      parser.skipnumbers = true;
      
      try
      {
        base.UpdateSourceCodeProperties(context);
      }
      catch (SyntaxErrorException)
      {
        context.SourceUnit.CodeProperties = SourceCodeProperties.IsIncompleteStatement;
      }
      finally
      {
        parser.skipnumbers = false;
      }
    }

    public override CodeBlock ParseSourceCode(CompilerContext context)
    {
      switch (context.SourceUnit.Kind)
      {
        case SourceCodeKind.InteractiveCode:
          {
            string code = context.SourceUnit.GetCode();

            if (code.Length > 0)
            {
              code = string.Format("(eval-r6rs '(begin {0}))", code + "\n"); // need to deal with comments
            }

            CodeBlock cb = ParseString(code, context);
            if (cb == null && context.SourceUnit.CodeProperties == null)
            {
              context.SourceUnit.CodeProperties = SourceCodeProperties.IsIncompleteStatement;
            }
            return cb;
          }
        case SourceCodeKind.File:
          using (Stream s = File.OpenRead(context.SourceUnit.Id))
          {
            return ParseStream(s, context);
          }
        default:
          {
            string code = context.SourceUnit.GetCode();
            if (code.Length < 10)
            {
              code = code.Trim();
            }
            if (code.Length > 0)
            {
              code = string.Format("(eval-embedded '(begin {0}))", code + "\n");
            }
            CodeBlock cb = ParseString(code, context);
            return cb;
          }
      }
    }

    [ThreadStatic]
    static Parser parser;

    static CodeBlock ParseString(string expr, CompilerContext cc)
    {
      Scanner sc = new Scanner();
      sc.SourceUnit = cc.SourceUnit;
      sc.Errors = cc.Errors;
      sc.SetSource(expr, 0);
      return Parse(sc, cc);
    }

    static CodeBlock ParseStream(Stream s, CompilerContext cc)
    {
      Scanner sc = new Scanner(s, "GUESS");
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
      Scanner sc = new Scanner(code, "GUESS");
      p.scanner = sc;
      sc.SourceUnit = cc.SourceUnit;
      sc.Errors = cc.Errors;

      if (p.Parse())
      {
        return p.parsed;
      }
      return Builtins.LexicalError("invalid syntax", code);
    }

    internal static object ReadExpressions(string code, CompilerContext cc)
    {
      if (parser == null)
      {
        parser = new Parser();
      }
      Parser p = parser;
      Scanner sc = new Scanner();
      sc.SourceUnit = cc.SourceUnit;
      sc.Errors = cc.Errors;
      sc.SetSource(code, 0);
      p.scanner = sc;

      if (p.Parse())
      {
        return p.parsed;
      }
      return Builtins.LexicalError("invalid syntax", code);

    }

    internal static CodeBlock Compile(Cons expr)
    {
      CodeBlock cb = Ast.CodeBlock("__script__");

      Compile(cb, expr);

      return cb;
    }

    static void Compile(CodeBlock cb, Cons expr)
    {
      cb.IsGlobal = true;

      Compiler.Generator.FillBody(cb, new List<Statement>(), expr, true);

      Compiler.Optimizer.Optimize(cb);
    }

    internal static CodeBlock CompileExpr(Cons expr)
    {
      CodeBlock cb = Ast.CodeBlock("eval-core");
      
      Compile(cb, expr);
      
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

      if (p.Parse() && (!cc.SourceUnit.CodeProperties.HasValue || cc.SourceUnit.CodeProperties.Value != SourceCodeProperties.None))
      {
        return Compile( p.parsed);
      }
      return null;
    }
  }
}
