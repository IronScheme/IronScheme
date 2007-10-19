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
using IronScheme.Runtime;
using IronScheme.Compiler;
using Microsoft.Scripting.Generation;
using System.IO;

using System.Diagnostics;
using Microsoft.Scripting.Types;
using System.Collections;

using Generator = IronScheme.Compiler.Generator;

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
      BuiltinFunction bif;
      if (Generator.BuiltinFunctions.TryGetValue(name, out bif))
      {
        value = bif;
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

    static Parser parser;
    static Scanner scanner;

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

    internal static object ParseExpressionString(string code)
    {
      object expr = ReadExpressionString(code);
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

    internal static object ReadExpressionString(string code)
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
        Cons parsed = SyntaxTransform.Transform(p.parsed);
        if (parsed != null)
        {
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
        Cons parsed = SyntaxTransform.Transform(p.parsed);

        CodeBlock cb = Ast.CodeBlock(!clearresolver ? "__toploop__" : "__script__");
        cb.IsGlobal = true;

        List<Statement> stmts = new List<Statement>();

        while (parsed != null)
        {
          object exp = parsed.Car;
          Expression e = Generator.GetAst(exp,cb);
          stmts.Add(Ast.Statement(e));
          parsed = parsed.Cdr as Cons;
        }

        if (stmts.Count > 0)
        {
          stmts[stmts.Count - 1] = Ast.Return(((ExpressionStatement)stmts[stmts.Count - 1]).Expression);
        }
        else
        {
          stmts.Add(Ast.Return(Ast.Null()));
        }

        cb.Body = Ast.Block(stmts);

        return cb;
        //}
      }
      return null;
    }


  }
}
