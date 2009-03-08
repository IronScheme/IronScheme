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

    protected override Exception MissingName(SymbolId name)
    {
      Builtins.UndefinedError(name);
      return base.MissingName(name);
    }

    //static SourceCodeProperties props = SourceCodeProperties.None;

    public override void UpdateSourceCodeProperties(CompilerContext context)
    {
      if (parser == null)
      {
        parser = new Parser();
      }
      parser.skipnumbers = true;
      var prev = context.SourceUnit.CodeProperties;
      try
      {
        //context.SourceUnit.CodeProperties = SourceCodeProperties.None;
        base.UpdateSourceCodeProperties(context);
        //context.SourceUnit.CodeProperties = prev;
      }
      catch (Runtime.R6RS.CompoundCondition)
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
#if DEBUG
      Stopwatch sw = Stopwatch.StartNew();

      try
      {
#endif
        switch (context.SourceUnit.Kind)
        {
          default:
          case SourceCodeKind.InteractiveCode:
            string code = context.SourceUnit.GetCode();

            if (code.Length < 10)
            {
              code = code.Trim();
            }
            if (code.Length > 0)
            {
#if CPS
              code = string.Format("(eval-r6rs identity-for-cps '(begin {0}))", code.Trim());
#else
              code = string.Format("(eval-r6rs '(begin {0}))", code.Trim());
#endif
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
          //default:
            //return ParseString(context.SourceUnit.GetCode(), context);
        }
#if DEBUG
      }
      finally
      {
        Trace.WriteLine(sw.ElapsedMilliseconds, "Parse: " + context.SourceUnit);
      }
#endif
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
      CodeBlock cb = Ast.CodeBlock("__script__");

      Compile(cb, expr);

      Parser.sourcemap.Clear();
      return cb;
    }

    static void Compile(CodeBlock cb, Cons expr)
    {
      cb.IsGlobal = true;

      expr = Compiler.SourceOptimizer.Optimize(expr);

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
        return Compile(p.parsed);
      }
      Builtins.SyntaxError("parser", "expression could not be parsed", cc.SourceUnit, false);
      return null;
    }


  }
}
