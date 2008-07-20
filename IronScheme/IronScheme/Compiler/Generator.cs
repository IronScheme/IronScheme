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
        //if (!IsSimpleCons(c))
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
      else if (args is byte[])
      {
        Expression[] ba = Array.ConvertAll<byte, Expression>(args as byte[], delegate(byte b) { return Ast.Constant(b); });
        return Ast.NewArray(typeof(byte[]), ba);
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
        if (Builtins.IsTrue(Builtins.IsSymbol(c.car)))
        {
          SymbolId f = (SymbolId)c.car;

          Variable var = cb.Lookup(f);

          if (var != null && !assigns.ContainsKey(f))
          {
            var = null;
          }

          object m;
          CodeBlockExpression cbe;

          //// needs to do the same for overloads...
          if (SimpleGenerator.libraryglobals.TryGetValue(f, out cbe))
          {
            Expression[] ppp = GetAstList(c.cdr as Cons, cb);

            if (cbe.Block.ParameterCount < 6 && cbe.Block.ParameterCount == ppp.Length)
            {
              return CallNormal(cbe, ppp);
            }
          }

          // varargs
          if (SimpleGenerator.libraryglobalsX.TryGetValue(f, out cbe))
          {
            Expression[] ppp = GetAstList(c.cdr as Cons, cb);

            if (cbe.Block.ParameterCount < 6 && cbe.Block.ParameterCount - 1 <= ppp.Length)
            {
              return CallVarArgs(cbe, ppp);
            }
          }

          // overloads
          CodeBlockDescriptor[] cbd;
          if (SimpleGenerator.libraryglobalsN.TryGetValue(f, out cbd))
          {
            Expression[] ppp = GetAstList(c.cdr as Cons, cb);

            foreach (CodeBlockDescriptor d in cbd)
            {
              if (d.codeblock.Block.ParameterCount < 6)
              {
                if (ppp.Length == d.arity || (d.varargs && ppp.Length > d.arity))
                {
                  if (d.varargs)
                  {
                    return CallVarArgs(d.codeblock, ppp);
                  }
                  else
                  {
                    return CallNormal(d.codeblock, ppp);
                  }
                }
              }
            }
          }

          if (f == SymbolTable.StringToId("call-with-values"))
          {
            Expression[] ppp = GetAstListNoCast(c.cdr as Cons, cb);
            if (ppp.Length == 2 && ppp[1] is MethodCallExpression)
            {
              MethodCallExpression consumer = ppp[1] as MethodCallExpression;

              if (ppp[0] is MethodCallExpression)
              {
                MethodCallExpression producer = ppp[0] as MethodCallExpression;
                if (consumer.Method == Closure_Make && producer.Method == Closure_Make)
                {
                  CodeBlockExpression ccbe = consumer.Arguments[1] as CodeBlockExpression;
                  CodeBlockExpression pcbe = producer.Arguments[1] as CodeBlockExpression;

                  pcbe.Block.Bind();
                  ccbe.Block.Bind();

                  if (ccbe.Block.ParameterCount == 0)
                  {
                    return CallNormal(ccbe);
                  }
                  else if (ccbe.Block.ParameterCount == 1)
                  {
                    return CallNormal(ccbe, CallNormal(pcbe));
                  }
                  else if (ccbe.Block.ParameterCount < 6)
                  {
                    Variable values = cb.CreateTemporaryVariable((SymbolId)Builtins.GenSym("values"), typeof(object[]));

                    Expression valuesarr = Ast.Read(values);

                    Expression[] pppp = new Expression[ccbe.Block.ParameterCount];

                    for (int i = 0; i < pppp.Length; i++)
                    {
                      pppp[i] = Ast.ArrayIndex(valuesarr, Ast.Constant(i));
                    }

                    return Ast.Comma(Ast.Assign(values, Ast.ConvertHelper(CallNormal(pcbe), typeof(object[]))), CallNormal(ccbe, pppp));
                  }
                }
              }
              if (consumer.Method == Closure_Make)
              {
                CodeBlockExpression ccbe = consumer.Arguments[1] as CodeBlockExpression;
                ccbe.Block.Bind();

                Expression producer = ppp[0];

                Expression exx = Ast.ConvertHelper(producer, typeof(ICallable));

                MethodInfo callx = GetCallable(0);

                if (ccbe.Block.ParameterCount == 0)
                {
                  return CallNormal(ccbe);
                }
                else if (ccbe.Block.ParameterCount == 1)
                {
                  return CallNormal(ccbe, Ast.Call(exx, callx));
                }
                else if (ccbe.Block.ParameterCount < 6)
                {
                  Variable values = cb.CreateTemporaryVariable((SymbolId)Builtins.GenSym("values"), typeof(object[]));

                  Expression valuesarr = Ast.Read(values);

                  Expression[] pppp = new Expression[ccbe.Block.ParameterCount];

                  for (int i = 0; i < pppp.Length; i++)
                  {
                    pppp[i] = Ast.ArrayIndex(valuesarr, Ast.Constant(i));
                  }

                  return Ast.Comma(Ast.Assign(values, Ast.ConvertHelper(Ast.Call(exx, callx), typeof(object[]))), CallNormal(ccbe, pppp));
                }
              }
            }
          }

          if (Context.Scope.TryLookupName(f, out m))
          {
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
              Closure clos = m as Closure;
              if (clos != null)
              {
                // no provision for varargs
                MethodInfo[] mis = clos.Targets;
                if (mis.Length > 0)
                {

                  MethodBinder mb = MethodBinder.MakeBinder(binder, SymbolTable.IdToString(f), mis, BinderType.Normal);

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
                // check for overload thing
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

            if (pp.Length < 6 && cbe.Block.ParameterCount == pp.Length)
            {
              return CallNormal(cbe, pp);
            }
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
      else if (args is byte[])
      {
        Expression[] ba = Array.ConvertAll<byte, Expression>(args as byte[], delegate (byte b) { return Ast.Constant(b);});
        return Ast.NewArray(typeof(byte[]), ba);
      }
      else
      {
        if (args is SymbolId)
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

    protected static Expression CallNormal(CodeBlockExpression cbe, params Expression[] ppp)
    {
      bool needscontext = cbe.Block.IsClosure || cbe.Block.ExplicitCodeContextExpression == null; // true;
      int pc = ppp.Length;
      MethodInfo dc = GetDirectCallable(needscontext, pc);
      if (needscontext)
      {
        ppp = ArrayUtils.Insert<Expression>(Ast.CodeContext(), ppp);
      }

      cbe = Ast.CodeBlockReference(cbe.Block, CallTargets.GetTargetType(needscontext, pc, false));

      cbe.Block.Bind();

      return Ast.ComplexCallHelper(cbe, dc, ppp);
    }

    protected static Expression CallVarArgs(CodeBlockExpression cbe, Expression[] ppp)
    {
      bool needscontext = cbe.Block.IsClosure || cbe.Block.ExplicitCodeContextExpression == null;//true;

      int pc = cbe.Block.ParameterCount;

      Expression[] tail = new Expression[ppp.Length - (pc - 1)];

      Array.Copy(ppp, ppp.Length - tail.Length, tail, 0, tail.Length);

      Expression[] nppp = new Expression[pc];

      Array.Copy(ppp, nppp, ppp.Length - tail.Length);

      if (tail.Length > 0)
      {
        nppp[nppp.Length - 1] = Ast.Call(MakeList(tail, true), tail);
      }
      else
      {
        nppp[nppp.Length - 1] = Ast.Null();
      }

      ppp = nppp;

      MethodInfo dc = GetDirectCallable(needscontext, pc);
      if (needscontext)
      {
        ppp = ArrayUtils.Insert<Expression>(Ast.CodeContext(), ppp);
      }

      cbe = Ast.CodeBlockReference(cbe.Block, CallTargets.GetTargetType(needscontext, pc, false));

      cbe.Block.Bind();

      return Ast.ComplexCallHelper(cbe, dc, ppp);
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
