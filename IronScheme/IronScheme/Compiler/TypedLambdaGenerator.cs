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
  [Generator("typed-lambda")]
  sealed class TypedLambdaGenerator : TypedGenerator
  {
    public override Expression Generate(object args, CodeBlock c)
    {
      var refs = ClrGenerator.SaveReferences();

      object arg = Builtins.First(args);
      object typespec = (Builtins.Second(args));

      Cons body = Builtins.Cdr(Builtins.Cdr(args)) as Cons;

      var returntype = ClrGenerator.ExtractTypeInfo(Builtins.List(quote, Builtins.Second(typespec)));

      CodeBlock cb = Ast.CodeBlock(SpanHint, GetLambdaName(c), returntype);
      NameHint = SymbolId.Empty;
      cb.Filename = LocationHint;
      cb.Parent = c;

      bool isrest = AssignParameters(cb, arg, Builtins.Car(typespec));

      if (isrest)
      {
        cb.Name += "+";
        cb.IsRest = true;
      }

      List<Statement> stmts = new List<Statement>();
      FillBody(cb, stmts, body, true);

      Type dt = GetDelegateType(cb);
      Type ct = GetClosureType(cb);

      var cbe = Ast.CodeBlockExpression(cb, true, dt);

      Expression ex = Ast.New(ct.GetConstructor(new Type[] { dt, typeof(bool) }), cbe, Ast.Constant(isrest));

      CodeBlockDescriptor cbd = new CodeBlockDescriptor();
      cbd.arity = isrest ? -cb.ParameterCount : cb.ParameterCount;
      cbd.callable = ex;
      cbd.codeblock = cbe;
      cbd.varargs = isrest;

      descriptorshack2.Add(cbd.callable, cbd);

      ClrGenerator.ResetReferences(refs);

      return ex;
    }
  }
}
