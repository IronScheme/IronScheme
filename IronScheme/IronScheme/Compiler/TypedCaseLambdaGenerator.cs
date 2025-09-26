#region License
/* Copyright (c) 2007-2016 Llewellyn Pritchard
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Collections.Generic;
using IronScheme.Runtime;
using Microsoft.Scripting;
using Microsoft.Scripting.Ast;

namespace IronScheme.Compiler
{

  [Generator("typed-case-lambda")]
  class TypedCaseLambdaGenerator : TypedGenerator
  {
    protected Cons annotations;
    static TypedLambdaGenerator lambdagen;

    public override Expression Generate(object args, CodeBlock c)
    {
      Cons lambdas = args as Cons;

      int arlen = lambdas == null ? 0 : lambdas.Length;

      if (arlen == 1)
      {
        if (lambdagen == null)
        {
          lambdagen = Context.Scope.LookupName(SymbolTable.StringToId("typed-lambda")) as TypedLambdaGenerator;
        }
        return lambdagen.Generate(lambdas.car, c);
      }
      else
      {
        List<CodeBlockDescriptor> cbs = new List<CodeBlockDescriptor>();

        string lambdaname = GetLambdaName(c);

        NameHint = SymbolId.Empty;

        var sh = SpanHint;
        var lh = LocationHint;


        while (lambdas != null)
        {
          var refs = ClrGenerator.SaveReferences();

          object actual = lambdas.car;

          object arg = Builtins.First(actual);
          object typespec = (Builtins.Second(actual));

          Cons body = Builtins.Cdr(Builtins.Cdr(actual)) as Cons;

          var returntype = ClrGenerator.ExtractTypeInfo(Builtins.List(quote, Builtins.Second(typespec)));

          CodeBlock cb = Ast.CodeBlock(SpanHint, lambdaname, returntype);
          NameHint = SymbolId.Empty;
          cb.Filename = lh;
          cb.Parent = c;

          bool isrest = AssignParameters(cb, arg, Builtins.Car(typespec));

          if (isrest)
          {
            cb.Name += "+";
          }

          List<Statement> stmts = new List<Statement>();
          FillBody(cb, stmts, body, true);

          Type dt = GetDelegateType(cb);
          Type ct = GetClosureType(cb);

          var cbe = Ast.CodeBlockExpression(cb, true, dt);
          Expression ex = Ast.New(ct.GetConstructor(new Type[] {  dt }), cbe);

          CodeBlockDescriptor cbd = new CodeBlockDescriptor();
          cbd.arity = isrest ? -cb.ParameterCount : cb.ParameterCount;
          cbd.callable = ex;
          cbd.codeblock = cbe;
          cbd.varargs = isrest;

          descriptorshack2.Add(cbd.callable, cbd);

          cbs.Add(cbd);

          lambdas = lambdas.cdr as Cons;

          ClrGenerator.ResetReferences(refs);
        }

        return MakeTypedCaseClosure(lambdaname, cbs);
      }
    }
  }
}
