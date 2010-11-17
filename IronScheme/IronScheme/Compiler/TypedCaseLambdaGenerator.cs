#region License
/* Copyright (c) 2007,2008,2009,2010 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Collections.Generic;
using IronScheme.Runtime;
using IronScheme.Runtime.psyntax;
using Microsoft.Scripting;
using Microsoft.Scripting.Ast;

namespace IronScheme.Compiler
{
  [Generator("annotated-typed-case-lambda")]
  public sealed class AnnotatedTypedCaseLambdaGenerator : TypedCaseLambdaGenerator
  {
    public override Expression Generate(object args, CodeBlock c)
    {
      Cons a = (Cons)args;
      object an = a.car;
      if (an is Annotation)
      {
        var anno = (Annotation)an;

        if (anno.source is Cons)
        {
          Cons src = anno.source as Cons;
          string filename = src.car as string;
          object location = src.cdr;

          Cons expr = anno.expression as Cons;

          annotations = expr == null ? null : expr.cdr as Cons;

          // bootstrap check
          if (location is string)
          {
            SpanHint = ExtractLocation(location as string);
          }
          else if (location is SourceSpan)
          {
            SpanHint = (SourceSpan)location;
          }

          if (c.Filename == null)
          {
            c.Filename = filename;
          }

          LocationHint = filename;

          return base.Generate(a.cdr, c);
        }
      }
      LocationHint = null;
      SpanHint = SourceSpan.None;
      return base.Generate(a.cdr, c);
    }


  }

  [Generator("typed-lambda")]
  public sealed class TypedLambdaGenerator : SimpleGenerator
  {
    public override Expression Generate(object args, CodeBlock c)
    {
      var refs = ClrGenerator.SaveReferences();

      object arg = Builtins.First(args);
      object typespec = Builtins.Second(Builtins.Second(args));

      Cons body = Builtins.Cdr(Builtins.Cdr(args)) as Cons;

      var returntype = ClrGenerator.ExtractTypeInfo(Builtins.List(quote,  Builtins.Second(typespec)));

      CodeBlock cb = Ast.CodeBlock(SpanHint, GetLambdaName(c), returntype);
      NameHint = SymbolId.Empty;
      cb.Filename = LocationHint;
      cb.Parent = c;

      bool isrest = AssignParameters(cb, arg, Builtins.Car(typespec));

      List<Statement> stmts = new List<Statement>();
      FillBody(cb, stmts, body, true);

      Type dt = GetDelegateType(cb);
      Type ct = GetClosureType(cb);


      Expression ex = Ast.New(ct.GetConstructor( new Type[] { dt }), Ast.CodeBlockExpression(cb, true, dt));

      ClrGenerator.ResetReferences(refs);

      return ex;
    }


    Type GetClosureType(CodeBlock cb)
    {
      Type[] types = GetTypeSpec(cb);

      var functype = GetGenericType("IronScheme.Runtime.Typed.TypedClosure", types);

      return functype as Type;
    }

    Type GetDelegateType(CodeBlock cb)
    {
      Type[] types = GetTypeSpec(cb);

      var functype = GetGenericType("IronScheme.Runtime.Typed.Func", types);

      return functype as Type;
    }

    static Type GetGenericType(string typename, Type[] types)
    {
      int l = types.Length;
      var functype = ClrGenerator.GetTypeFast(typename + "`" + l).MakeGenericType(types);
      return functype;
    }

    static Type[] GetTypeSpec(CodeBlock cb)
    {
      List<Type> types = new List<Type>();

      foreach (var v in cb.Parameters)
      {
        types.Add(v.Type);
      }

      types.Add(cb.ReturnType);
      return types.ToArray();
    }
  }

  [Generator("typed-case-lambda")]
  public class TypedCaseLambdaGenerator : SimpleGenerator
  {
    protected Cons annotations;
    TypedLambdaGenerator lambdagen;
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

          CodeBlock cb = Ast.CodeBlock(sh, lambdaname);
          cb.Filename = lh;
          cb.Parent = c;

          object arg = Builtins.First(actual);
          Cons body = Builtins.Cdr(actual) as Cons;

          bool isrest = AssignParameters(cb, arg);

          List<Statement> stmts = new List<Statement>();
          FillBody(cb, stmts, body, true);

          CodeBlockDescriptor cbd = new CodeBlockDescriptor();
          cbd.arity = isrest ? -cb.ParameterCount : cb.ParameterCount;
          cbd.codeblock = Ast.CodeBlockExpression(cb, false);
          cbd.varargs = isrest;

          descriptorshack.Add(cbd.codeblock, cbd);

          cbs.Add(cbd);

          lambdas = lambdas.cdr as Cons;

          ClrGenerator.ResetReferences(refs);
        }

        return MakeCaseClosure(lambdaname, cbs);
      }
    }
  }
}
