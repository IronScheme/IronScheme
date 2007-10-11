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
using Microsoft.Scripting.Ast;
using Mast = Microsoft.Scripting.Ast.Ast;
using System.IO;
using IronScheme.Compilers;
using IronScheme.Ast;
using Microsoft.Scripting.Generation;
using System.Reflection.Emit;
using IronScheme.Compilers.Stage1;

namespace IronScheme.Hosting
{
  [Obsolete]
  public class IronSchemeScriptCompiler
  {
    public IronSchemeScriptCompiler()
    {
      
    }


    //public CodeBlock ParseStatementCode(CompilerContext cc)
    //{
    //  StatementSourceCode su = cc.SourceUnit as StatementSourceCode;
    //  return ParseString(su.GetCode(), cc);
    //}

    //public CodeBlock ParseExpressionCode(CompilerContext cc)
    //{
    //  ExpressionSourceCode su = cc.SourceUnit as ExpressionSourceCode;
    //  return ParseString(su.GetCode(), cc);
    //}

    //public CodeBlock ParseFile(CompilerContext cc)
    //{
    //  SourceFileUnit su = cc.SourceUnit as SourceFileUnit;
    //  using (Stream s = File.OpenRead(su.Path))
    //  {
    //    return ParseStream(s, cc);
    //  }
    //}

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
        if (stage2parser == null)
        {
          stage2parser = new IronScheme.Compilers.Stage2.Parser();
        }
        if (stage2scanner == null)
        {
          stage2scanner = new IronScheme.Compilers.Stage2.Scanner();
          stage2parser.scanner = stage2scanner;
        }

        ExpressionList parsed = Stage15Transform.Transform(p.parsed);

        stage2scanner.SetSource(parsed);

        if (stage2parser.Parse())
        {
          if (clearresolver || !resolvercleared)
          {
            TypeResolver.Initialize(stage2parser.usings);
            resolvercleared = true;
          }
          CodeBlock cb = Mast.CodeBlock(!clearresolver ? "__toploop__" : "__script__");
          cb.IsGlobal = true;
          List<Statement> stmts = new List<Statement>();

          if (stage2parser.parsed != null)
          {
            ExpressionList exprlist = stage2parser.parsed;
            Variable ret = cb.CreateVariable(Symbols.RETVAL, Variable.VariableKind.Global, typeof(object), Mast.Null());


            foreach (Ast.Expression expr in exprlist)
            {

              Microsoft.Scripting.Ast.Expression e = expr.GetAst(cb);
              if (e is VoidExpression)
              {
                stmts.Add(Mast.Statement(expr.Span, e));
              }
              else
              {
                stmts.Add(Mast.Statement(expr.Span, Mast.Assign(ret, e)));
                CodeBlockExpression cbe = e as CodeBlockExpression;
                if (cbe != null)
                {
                  Type ct = typeof(CallTargetWithContextN);
                  IronScheme.Ast.Expression.UpdateReturnValueType(cb, ct);
                }
                else
                {
                  IronScheme.Ast.Expression.UpdateReturnValueType(cb, e.ExpressionType);
                }
              }

            }

            if (stmts.Count > 0)
            {
              stmts.Add(Mast.Return(Mast.Read(ret)));
            }
          }

          cb.Body = Mast.Block(stmts);

          return cb;
        }
      }
      return null;
    }

    //public CodeBlock ParseInteractiveCode(CompilerContext cc, bool allowIncomplete, out InteractiveCodeProperties properties)
    //{
    //  properties = InteractiveCodeProperties.None;
    //  SourceCodeUnit su = cc.SourceUnit as SourceCodeUnit;

    //  CodeBlock cb = ParseString(su.GetCode(), cc);
    //  if (cb == null && allowIncomplete)
    //  {
    //    properties = InteractiveCodeProperties.IsIncompleteStatement;
    //  }
    //  return cb;
    //}
  }
}
