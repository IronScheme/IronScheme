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
        if ((bool)Builtins.IsEqual(c.car, unquote))
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
        if (nestinglevel == 1 && (bool)Builtins.IsEqual(c.car, quasiquote))
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
        if (c != null)
        {
          if (nestinglevel == 0 || nestinglevel == 1073741823)
          {
            return Ast.Constant(new IronSchemeConstant(c));
          }
          else
          {
            ;
          }
        }
        return GetConsList(c, cb);
      }
      object[] v = args as object[];
      if (v != null)
      {
        if (v.Length > 0 && (nestinglevel == 0 || nestinglevel == 1073741823))
        {
          return Ast.Constant(new IronSchemeConstant(v));
        }
        return GetConsVector(v, cb);
      }
      else if (args is Fraction)
      {
        Fraction f = (Fraction) args;
        return Ast.New(Fraction_New, Ast.Constant(f.Numerator), Ast.Constant(f.Denominator));
      }
      else
      {
        return Ast.Constant(args);
      }
    }

    protected readonly static Dictionary<SymbolId, bool> assigns = new Dictionary<SymbolId, bool>();

    protected internal static Expression GetAst(object args, CodeBlock cb)
    {
      Cons c = args as Cons;
      if (c != null)
      {
        if ((bool)Builtins.IsSymbol(c.car))
        {
          SymbolId f = (SymbolId)c.car;

          Variable var = FindVar(cb, f);

          if (var != null && !assigns.ContainsKey(f))
          {
            var = null;
          }

          object m;

          if (Context.Scope.TryLookupName(f, out m))
          {
            if ((bool)Builtins.IsEqual(define, f) && (bool)Builtins.IsPair(Builtins.First(c.cdr)))
            {
              Cons ii = c.cdr as Cons;
              Cons jj = ii.car as Cons;
              c.cdr = Builtins.List(jj.car, Builtins.Append(Builtins.List(lambda, jj.cdr), ii.cdr));
            }
            Runtime.Macro macro = m as Runtime.Macro;
            if (macro != null)
            {
              if (macrotrace)
              {
                Debug.WriteLine(Builtins.WriteFormat(c), "macro::in ");
              }
              object result = macro.Invoke(Context, c.cdr);
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

            //terrible....
            //CodeBlockExpression cbe = m as CodeBlockExpression;
            //if (cbe != null)
            //{
            //  if (cb.Parent != null && cb.Parent.Parent != null && cb.Parent.Parent.Parent == null)
            //  {
            //    Expression[] ppp = GetAstList(c.cdr as Cons, cb);

            //    bool needscontext = true;
            //    MethodInfo dc = GetDirectCallable(needscontext, ppp.Length);
            //    if (needscontext)
            //    {
            //      ppp = ArrayUtils.Insert<Expression>(Ast.CodeContext(), ppp);
            //    }

            //    return Ast.ComplexCallHelper(cbe, dc, ppp);
            //  }
            //}

            if (var == null)
            {

              IGenerator gh = m as IGenerator;
              if (gh != null)
              {
                if (!Parser.sourcemap.TryGetValue(c, out spanhint))
                {
                  spanhint = SourceSpan.None;
                }
                return gh.Generate(c.cdr, cb);
              }

              BuiltinMethod bf = m as BuiltinMethod;
              if (bf != null)
              {
                // check for inline emitter
                InlineEmitter ie;
                if (inlineemitters.TryGetValue(f, out ie))
                {
                  Expression result = ie(GetAstList(c.cdr as Cons, cb));
                  // if null is returned, the method cannot be inlined
                  if (result != null)
                  {
                    return result;
                  }
                }

                MethodBinder mb = bf.Binder;
                Expression[] pars = GetAstList(c.cdr as Cons, cb);

                Type[] types = GetExpressionTypes(pars);
                MethodCandidate mc = mb.MakeBindingTarget(CallType.None, types);
                if (mc == null)
                {
                  Builtins.SyntaxError(SymbolTable.StringToId("generator"), "no match found", args, f);
                }
                if (mc.Target.NeedsContext)
                {
                  pars = ArrayUtils.Insert<Expression>(Ast.CodeContext(), pars);
                }
                return Ast.ComplexCallHelper(mc.Target.Method as MethodInfo, pars);
              }
            }
          }
        }

        Expression[] pp = GetAstList(c.cdr as Cons, cb);

        Expression ex = Unwrap(GetAst(c.car, cb));
        if (ex is MethodCallExpression)
        {
          MethodCallExpression mcexpr = (MethodCallExpression)ex;
          if (mcexpr.Method == Closure_Make)
          {
            CodeBlockExpression cbe = mcexpr.Arguments[1] as CodeBlockExpression;

            bool needscontext = true;
            MethodInfo dc = GetDirectCallable(needscontext, pp.Length);
            if (needscontext)
            {
              pp = ArrayUtils.Insert<Expression>(mcexpr.Arguments[0], pp);
            }
            return Ast.ComplexCallHelper(cbe, dc, pp);
          }
        }

        if (ex is ConstantExpression)
        {
          Builtins.SyntaxError(SymbolTable.StringToId("generator"), "expecting a procedure", c.car, c);
        }

        ex = Ast.ConvertHelper(ex, typeof(ICallable));
        
        MethodInfo call = GetCallable(pp.Length);

        Expression r = pp.Length > 5 ?
          Ast.Call(ex, call, Ast.NewArray(typeof(object[]), pp)) :
          Ast.Call(ex, call, pp);

        if (spanhint != SourceSpan.Invalid || spanhint != SourceSpan.None)
        {
          r.SetLoc(spanhint);
        }

        return r;
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
        if (args is Fraction)
        {
          Fraction f = (Fraction)args;
          return Ast.New(Fraction_New, Ast.Constant(f.Numerator), Ast.Constant(f.Denominator));
        }
        return Ast.Constant(args);
      }
    }

    static Expression Unwrap(Expression ex)
    {
      while (ex is UnaryExpression && ((UnaryExpression)ex).NodeType == AstNodeType.Convert)
      {
        ex = ((UnaryExpression)ex).Operand;
      }

      return ex;
    }

  }

}
