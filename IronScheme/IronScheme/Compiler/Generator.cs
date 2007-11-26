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
using Microsoft.Scripting.Ast;
using IronScheme.Runtime;
using Microsoft.Scripting;
using IronScheme.Hosting;
using Microsoft.Scripting.Actions;
using System.Reflection;
using Microsoft.Scripting.Hosting;
using Microsoft.Scripting.Utils;
using System.Diagnostics;

[assembly: Extension(GeneratorType=typeof(Generator), BuiltinsType=typeof(Builtins))]

namespace IronScheme.Compiler
{

  public partial class Generator
  {
    static Generator()
    {
      Initialize();
    }

    static SourceSpan spanhint;

    protected static SourceSpan SpanHint
    {
      get { return Generator.spanhint; }
      set { Generator.spanhint = value; }
    }

    static int nestinglevel = 0;

    protected static int NestingLevel
    {
      get { return Generator.nestinglevel; }
      set { Generator.nestinglevel = value; }
    }

    static bool macrotrace = false;

    public static bool MacroTrace
    {
      get { return Generator.macrotrace; }
      set { Generator.macrotrace = value; }
    }



    protected internal readonly static FieldInfo Unspecified = typeof(Builtins).GetField("Unspecified");

    protected static Expression GetCons(object args, CodeBlock cb)
    {
      Cons c = args as Cons;
      if (c != null)
      {
        if ((bool)Builtins.IsEqual(c.Car, unquote))
        {
          nestinglevel--;
          try
          {
            if (nestinglevel == 0)
            {
              return GetAst(Builtins.Second(c), cb);
            }
            else
            {
              return GetConsList(c, cb);
            }
          }
          finally
          {
            nestinglevel++;
          }
        }
        if (nestinglevel == 1 && (bool)Builtins.IsEqual(c.Car, quasiquote))
        {
          nestinglevel++;
          try
          {
            return GetConsList(c, cb);
          }
          finally
          {
            nestinglevel--;
          }
        }
        return GetConsList(c, cb);
      }
      object[] v = args as object[];
      if (v != null)
      {
        return GetConsVector(v, cb);
      }
      else
      {
        return Ast.Constant(args);
      }
    }

    protected internal static Expression GetAst(object args, CodeBlock cb)
    {
      Cons c = args as Cons;
      if (c != null)
      {
        if ((bool)Builtins.IsSymbol(c.Car))
        {
          SymbolId f = (SymbolId)c.Car;

          Variable var = FindVar(cb, f);

          if (var != null)
          {
            ;
          }

          object m;

          if (Context.Scope.TryLookupName(f, out m))
          {
            if ((bool)Builtins.IsEqual(define, f) && (bool)Builtins.IsPair(Builtins.First(c.Cdr)))
            {
              Cons ii = c.Cdr as Cons;
              Cons jj = ii.Car as Cons;
              c.Cdr = Builtins.List(jj.Car, Builtins.Append(Builtins.List(lambda, jj.Cdr), ii.Cdr));
            }
            Runtime.Macro macro = m as Runtime.Macro;
            if (macro != null)
            {
              if (macrotrace)
              {
                Debug.WriteLine(Builtins.WriteFormat(c), "macro::in ");
              }
              object result = macro.Invoke(Context, c.Cdr);
              if (!Parser.sourcemap.TryGetValue(c, out spanhint))
              {
                spanhint = SourceSpan.None;
              }
              else if (result is Cons)
              {
                Parser.sourcemap[result as Cons] = spanhint;
                Parser.sourcemap.Remove(c);
              }
              if (macrotrace)
              {
                Debug.WriteLine(Builtins.WriteFormat(result), "macro::out");
              }
              Expression rr = GetAst(result, cb);
              if (spanhint != SourceSpan.Invalid || spanhint != SourceSpan.None)
              {
                rr.SetLoc(spanhint);
              }
              return rr;
            }

            IGenerator gh = m as IGenerator;
            if (gh != null)
            {
              if (!Parser.sourcemap.TryGetValue(c, out spanhint))
              {
                spanhint = SourceSpan.None;
              }
              return gh.Generate(c.Cdr, cb);
            }

            BuiltinMethod bf = m as BuiltinMethod;
            if (bf != null)
            {
              MethodBinder mb = bf.Binder;
              Expression[] pars = GetAstList(c.Cdr as Cons, cb);
              //pars[0] = Ast.RuntimeConstant(bf);
              Type[] types = GetExpressionTypes(pars);
              MethodCandidate mc = mb.MakeBindingTarget(CallType.None, types);
              if (mc == null)
              {
                throw new SyntaxErrorException("no match for " + f + " near: " + Parser.sourcemap[c]);
              }
              if (mc.Target.NeedsContext)
              {
                pars = ArrayUtils.Insert<Expression>(Ast.CodeContext(), pars);
              }
              return Ast.ComplexCallHelper(mc.Target.Method as MethodInfo, pars);
              // TODO: figure out how MethodGroup et al works, perhaps not
              //return Ast.Action.Call(typeof(object), pars);
            }
          }
        }
        Expression ex = Ast.ConvertHelper(GetAst(c.Car, cb), typeof(ICallable));
        Expression[] pp = GetAstList(c.Cdr as Cons, cb);

        Expression r = Ast.Call(ex, ICallable_Call, Ast.NewArray(typeof(object[]), pp));

        if (spanhint != SourceSpan.Invalid || spanhint != SourceSpan.None)
        {
          r.SetLoc(spanhint);
        }


        return r;

        //return Ast.Action.Call(typeof(object), GetAstList(c, cb));
      }
      object[] v = args as object[];
      if (v != null)
      {
        return GetConsVector(v, cb);
      }
      else
      {
        if ((bool)Builtins.IsSymbol(args))
        {
          return Read((SymbolId)args, cb, typeof(object));
        }
        if (args == Builtins.Unspecified)
        {
          return Ast.ReadField(null, Unspecified);
        }
        return Ast.Constant(args);
      }
    }





    //// macro-expand1
    //[Generator("macro-expand1")]
    //public static Expression MacroExpand1(object args, CodeBlock cb)
    //{
    //  args = Builtins.Car(args);
    //  object result = SyntaxExpander.Expand1(args);
    //  return GetCons(result, cb);
    //}



  }

}
