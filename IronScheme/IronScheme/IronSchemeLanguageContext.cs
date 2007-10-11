#region License
/* ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 
 *
 * This source code is subject to terms and conditions of the Microsoft Permissive License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Permissive License.
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
using IronScheme.Ast;
using IronScheme.Runtime;
using IronScheme.Compilers;
using Microsoft.Scripting.Generation;
using IronScheme.Compilers.Stage1;
using System.IO;
using Mast = Microsoft.Scripting.Ast.Ast;
using System.Diagnostics;
using Microsoft.Scripting.Types;
using System.Collections;

namespace IronScheme
{
  class IronSchemeLanguageContext : LanguageContext
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
        if (value is CodeBlock)
        {
          CodeBlock cb = (CodeBlock)value;
        }
        return true;
      }
      BuiltinFunction mb;
      if (Ast.Expression.builtinmap.TryGetValue(SymbolTable.IdToString(name), out mb))
      {
        value = mb;
        // add to scope
        context.ModuleContext.Module.Scope.SetName(name, value);

        return true;
      }

      value = name;
      return false;
    }

    public override CodeBlock ParseSourceCode(CompilerContext context)
    {
      switch (context.SourceUnit.Kind)
      {
        case SourceCodeKind.InteractiveCode:
          CodeBlock cb = ParseString(context.SourceUnit.GetCode(), context);
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

    static bool resolvercleared = false;

    static Parser parser;
    static Scanner scanner;

    static Compilers.Stage2.Parser stage2parser;
    static Compilers.Stage2.Scanner stage2scanner;

    static CodeBlock ParseString(string expr, CompilerContext cc)
    {
      if (scanner == null)
      {
        scanner = new Scanner();
      }
      Scanner sc = scanner;
      sc.SetSource(expr, 0);
      return Parse(sc, cc, false);
    }

    static CodeBlock ParseStream(Stream s, CompilerContext cc)
    {
      Scanner sc = new Scanner(s);
      return Parse(sc, cc, true);
    }

    internal static Ast.Expression ParseExpressionString(string code)
    {
      Ast.Expression expr = ReadExpressionString(code);
      if (expr != null)
      {
        InitStage2();

        stage2scanner.SetSource(new ExpressionList(expr));

        if (stage2parser.Parse())
        {
          if (stage2parser.parsed != null)
          {
            ExpressionList exprlist = stage2parser.parsed;

            foreach (Ast.Expression e in exprlist)
            {
              return e;
            }
          }
        }
      }
      return null;
      
    }

    private static void InitStage2()
    {
      if (stage2parser == null)
      {
        stage2parser = new IronScheme.Compilers.Stage2.Parser();
      }
      if (stage2scanner == null)
      {
        stage2scanner = new IronScheme.Compilers.Stage2.Scanner();
        stage2parser.scanner = stage2scanner;
      }
    }

    internal static Ast.Expression ReadExpressionString(string code)
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
      sc.SetSource(code, 0);
      p.scanner = sc;

      if (p.Parse())
      {
        ExpressionList parsed = Stage15Transform.Transform(p.parsed);
        if (parsed.Count > 0)
        {
          return parsed[0];
        }

      }
      return null;
    }


    static CodeBlock Parse(Scanner sc, CompilerContext cc, bool clearresolver)
    {
      if (parser == null)
      {
        parser = new Parser();
      }
      Parser p = parser;
      p.scanner = sc;

      if (p.Parse())
      {
        ExpressionList parsed = Stage15Transform.Transform(p.parsed);

        InitStage2();

        stage2scanner.SetSource(parsed);

        if (stage2parser.Parse())
        {
          if (clearresolver || !resolvercleared)
          {
            TypeResolver.Initialize(stage2parser.usings, stage2parser.references);
            resolvercleared = true;
          }
          else
          {
            TypeResolver.Add(stage2parser.usings, stage2parser.references);
          }
          CodeBlock cb = Mast.CodeBlock(!clearresolver ? "__toploop__" : "__script__");
          cb.IsGlobal = true;

          List<Statement> stmts = new List<Statement>();
         
          if (stage2parser.parsed != null)
          {
            ExpressionList exprlist = stage2parser.parsed;

            foreach (Ast.Expression expr in exprlist)
            {
              if (expr != null)
              {
                Microsoft.Scripting.Ast.Expression e = expr.GetAst(cb);
                stmts.Add(Mast.Statement(expr.Span, e));
              }
            }

            if (stmts.Count > 0)
            {
              stmts[stmts.Count - 1] = Mast.Return(((ExpressionStatement)stmts[stmts.Count - 1]).Expression);
            }
            else
            {
              stmts.Add(Mast.Return(Mast.Null()));
            }
          }

          cb.Body = Mast.Block(stmts);

          return cb;
        }
      }
      return null;
    }


  }
}
