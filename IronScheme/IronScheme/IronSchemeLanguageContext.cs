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
        return true;
      }
      if (Compiler.Generator.cc.Scope.TryLookupName(name, out value))
      {
        return true;
      }
      value = name;
      return false;
    }

    protected override ModuleGlobalCache GetModuleCache(SymbolId name)
    {
      
      return base.GetModuleCache(name);
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
      return Parse(sc, cc, false);
    }

    static CodeBlock ParseStream(Stream s, CompilerContext cc)
    {
      Scanner sc = new Scanner(s);
      sc.Errors = cc.Errors;
      sc.SourceUnit = cc.SourceUnit;
      return Parse(sc, cc, true);
    }

    internal static object ParseExpressionString(string code, CompilerContext cc)
    {
      object expr = ReadExpressionString(code, cc);
      if (expr != null)
      {
        //InitStage2();

        //stage2scanner.SetSource(new ExpressionList(expr));

        //if (stage2parser.Parse())
        //{
        //  if (stage2parser.parsed != null)
        //  {
        //    ExpressionList exprlist = stage2parser.parsed;

        //    foreach (Syntax e in exprlist)
        //    {
        //      return e;
        //    }
        //  }
        //}
      }
      return null;
      
    }

    internal static object ReadExpressionString(string code, CompilerContext cc)
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
        Cons parsed = p.parsed;
        if (parsed != null)
        {
          if (parsed.Cdr != null)
          {
            ;
          }
          return parsed.Car;
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
        Cons parsed = p.parsed;

        CodeBlock cb = Ast.CodeBlock(!clearresolver ? "__toploop__" : "__script__");
        cb.IsGlobal = true;

        List<Statement> stmts = new List<Statement>();

        Compiler.Generator.InitGlobal(parsed, cb, stmts);

        while (parsed != null)
        {
          object exp = parsed.Car;
          Expression e = Generator.GetAst(exp,cb);
          Statement s = Ast.Statement(e);
          if (exp is Cons && Parser.sourcemap.ContainsKey((Cons)exp))
          {
            s.SetLoc(Parser.sourcemap[(Cons)exp]);
          }
          stmts.Add(s);
          parsed = parsed.Cdr as Cons;
        }

        if (stmts.Count > 0)
        {
          SourceSpan ss = stmts[stmts.Count - 1].Span;
          stmts[stmts.Count - 1] = Ast.Return(((ExpressionStatement)stmts[stmts.Count - 1]).Expression);
          stmts[stmts.Count - 1].SetLoc(ss);
        }
        else
        {
          stmts.Add(Ast.Return(Ast.ReadField(null, Generator.Unspecified)));
        }

        cb.Body = Ast.Block(stmts);

        Parser.sourcemap.Clear();

        return cb;
      }
      return null;
    }


  }
}
