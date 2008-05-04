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
using Microsoft.Scripting.Math;

[assembly: Extension(GeneratorType=typeof(Generator), BuiltinsType=typeof(Builtins))]

namespace IronScheme.Compiler
{

  public partial class Generator
  {
    static Generator()
    {
      Initialize();
    }

    [ThreadStatic]
    static SourceSpan spanhint;

    protected static SourceSpan SpanHint
    {
      get { return Generator.spanhint; }
      set { Generator.spanhint = value; }
    }

    // this is probably not very threadsafe....
    protected static Dictionary<SymbolId, CodeBlockExpression> references = new Dictionary<SymbolId, CodeBlockExpression>();

    protected internal readonly static FieldInfo Unspecified = typeof(Builtins).GetField("Unspecified");

    protected static Expression GetCons(object args, CodeBlock cb)
    {
      Cons c = args as Cons;
      if (c != null)
      {
        if (!IsSimpleCons(c))
        {
          //return Ast.Constant(new IronSchemeConstant(c));
        }
        return GetConsList(c, cb);
      }
      object[] v = args as object[];
      if (v != null)
      {
        return GetConsVector(v, cb);
      }
      else if (args is Fraction)
      {
        Fraction f = (Fraction) args;
        return Ast.New(Fraction_New, Ast.Constant(f.Numerator), Ast.Constant(f.Denominator));
      }
      else
      {
        if (args is long)
        {
          args = (BigInteger)(long)args;
        }
        return Ast.Constant(args);
      }
    }

    static bool IsSimpleCons(Cons c)
    {
      if (c == null)
      {
        return true;
      }
      return !(c.car is Cons) && (c.cdr == null || IsSimpleCons(c.cdr as Cons));
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
            //terrible....
            CodeBlockExpression cbe = m as CodeBlockExpression;
            if (cbe != null)
            {
              Expression[] ppp = GetAstList(c.cdr as Cons, cb);

              bool needscontext = true;
              MethodInfo dc = GetDirectCallable(needscontext, ppp.Length);
              if (needscontext)
              {
                ppp = ArrayUtils.Insert<Expression>(Ast.CodeContext(), ppp);
              }

              return Ast.ComplexCallHelper(cbe, dc, ppp);
            }

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
                    if (result.Type.IsValueType)
                    {
                      result = Ast.Convert(result, typeof(object));
                    }
                    return result;
                  }
                }

                MethodBinder mb = bf.Binder;
                Expression[] pars = GetAstList(c.cdr as Cons, cb);

                Type[] types = GetExpressionTypes(pars);
                MethodCandidate mc = mb.MakeBindingTarget(CallType.None, types);
                if (mc != null)
                {
                  if (mc.Target.NeedsContext)
                  {
                    pars = ArrayUtils.Insert<Expression>(Ast.CodeContext(), pars);
                  }
                  MethodBase meth = mc.Target.Method;

                  return Ast.ComplexCallHelper(meth as MethodInfo, pars);
                }
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
          if (mcexpr.Instance is MethodCallExpression && mcexpr.Method.Name == "Call")
          {
            MethodCallExpression mcei = mcexpr.Instance as MethodCallExpression;
            if (mcei.Method == Closure_Make)
            {
              CodeBlockExpression cbe = mcei.Arguments[1] as CodeBlockExpression;
              Debug.Assert(mcexpr.Arguments.Count == 0);
              bool needscontext = true;
              MethodInfo dc = GetDirectCallable(needscontext, 0);
              ex = Ast.ComplexCallHelper(cbe, dc, mcei.Arguments[0]);
            }
          }
        }
        else if (!(ex is BoundExpression))
        {
          ;
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

    protected static Expression Unwrap(Expression ex)
    {
      while (ex is UnaryExpression && ((UnaryExpression)ex).NodeType == AstNodeType.Convert)
      {
        ex = ((UnaryExpression)ex).Operand;
      }

      return ex;
    }

  }

}
