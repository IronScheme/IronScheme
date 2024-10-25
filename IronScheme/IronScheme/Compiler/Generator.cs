#region License
/* Copyright (c) 2007-2016 Llewellyn Pritchard
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Reflection;
using IronScheme.Runtime;
using IronScheme.Runtime.psyntax;
using IronScheme.Runtime.Typed;
using Microsoft.Scripting;
using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Utils;
using BigInteger = Oyster.Math.IntX;
using System.Reflection.Emit;
using System.Text;
using IronScheme.Runtime.R6RS;

namespace IronScheme.Compiler
{

  partial class Generator
  {
    static Generator()
    {
      AllowTransientBinding = true;
      UnaryExpression.Converter = typeof(Helpers).GetMethod(nameof(Helpers.RequiresNotNull)).MakeGenericMethod(typeof(Callable));
      Initialize();
    }

    [ThreadStatic]
    static SourceSpan spanhint;

    internal static bool AllowTransientBinding { get; set; }

    internal static Microsoft.Scripting.Generation.AssemblyGen CurrentAssemblyGen { get; set; }

    protected static SourceSpan SpanHint
    {
      get { return spanhint; }
      set { spanhint = value; }
    }

    internal static string LocationHint { get; set; }

    // this is probably not very threadsafe....
    protected static Dictionary<SymbolId, CodeBlockExpression> references = new Dictionary<SymbolId, CodeBlockExpression>();

    protected internal readonly static FieldInfo Unspecified = typeof(Builtins).GetField(nameof(Builtins.Unspecified));
    protected internal readonly static FieldInfo True = typeof(RuntimeHelpers).GetField(nameof(RuntimeHelpers.True));
    protected internal readonly static FieldInfo False = typeof(RuntimeHelpers).GetField(nameof(RuntimeHelpers.False));
    internal static bool inconstant = false;

    protected static Expression GetCons(object args, CodeBlock cb)
    {
      if (args is Annotation)
      {
        args = ((Annotation)args).stripped;
      }
      Cons c = args as Cons;
      if (c != null)
      {
        if (inconstant)
        {
          return GetConsList(c, cb);
        }
        else
        {
          return Ast.Constant(new IronSchemeConstant(c, cb));
        }
      }
      object[] v = args as object[];
      if (v != null)
      {
        return GetConsVector(v, cb);
      }
      else if (args is byte[])
      {
        return Ast.Constant(new ArrayConstant<byte>((byte[]) args));
      }
      else if (args is int[])
      {
        return Ast.Constant(new ArrayConstant<int>((int[])args));
      }
      else if (args is double[])
      {
        return Ast.Constant(new ArrayConstant<double>((double[])args));
      }
      else if (args is Fraction)
      {
        Fraction f = (Fraction) args;
        return Ast.Constant(new FractionConstant(f));
      }
      else if (args is ComplexFraction)
      {
        ComplexFraction cf = (ComplexFraction)args;
        return Ast.Constant(new ComplexFractionConstant(cf));
      }
      else if (args is SchemeChar)
      {
        SchemeChar f = (SchemeChar)args;
        return Ast.Constant(new SchemeCharConstant(f));
      }
      else if (args is StringBuilder)
      {
        StringBuilder f = (StringBuilder)args;
        return Ast.Constant(f.ToString());
      }
      else if (args is RecordTypeDescriptor || args is RecordConstructorDescriptor || args is Callable)
      {
        //TODO: this is probably very bad, but using it now as it is the best I can do
        var name = Builtins.GenSym();
        Builtins.SetSymbolValueFast(name, args);
        return Ast.SimpleCallHelper(typeof(Builtins).GetMethod("SymbolValue"), Ast.Constant(name));
      }
      else
      {
        if (args is long)
        {
          args = (BigInteger)(long)args;
        }
        if (args != null && args.GetType().Namespace.StartsWith("record."))
        {
          args = new SerializedConstant(args);
        }
        else if (args != null && args is Condition)
        {
          args = new SerializedConstant(args);
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

    protected internal readonly static Dictionary<SymbolId, bool> assigns = new Dictionary<SymbolId, bool>();

    protected internal static Expression GetAst(object args, CodeBlock cb)
    {
      return GetAst(args, cb, false);
    }

    static bool IsSimpleExpression(Expression e)
    {
      if (e is MethodCallExpression)
      {
        return IsSimpleCall((MethodCallExpression)e);
      }

      if (e is UnaryExpression)
      {
        UnaryExpression ue = (UnaryExpression)e;
        if (ue.NodeType == AstNodeType.Convert)
        {
          return IsSimpleExpression(ue.Operand);
        }
        return false;
      }

      if (e is BinaryExpression)
      {
        return IsSimpleExpression(((BinaryExpression)e).Left) &&
          IsSimpleExpression(((BinaryExpression)e).Right);
      }

      if (e is TypeBinaryExpression)
      {
        return IsSimpleExpression(((TypeBinaryExpression)e).Expression);
      }

      if (e is ConstantExpression)
      {
        ConstantExpression ce = (ConstantExpression)e;
        return Builtins.IsTrue(ce.Value);
      }

      if (e is ConditionalExpression)
      {
        ConditionalExpression ce = (ConditionalExpression) e;
        return IsSimpleExpression(ce.Test) && IsSimpleExpression(ce.IfTrue) && IsSimpleExpression(ce.IfFalse);
      }

      if (e is BoundExpression)
      {
        return true;
      }
      else
      {
        return false;
      }
    }

    internal static bool IsTransient(Module m)
    {
      var mb = m as ModuleBuilder;
      if (mb == null)
      {
        var mn = m.Name;
        if (mn == "ironscheme.boot.dll" || mn == "IronScheme.dll")
        {
          return false;
        }
        if (mn == "<In Memory Module>" && m.ScopeName == "RefEmit_InMemoryManifestModule")
        {
          return true;
        }
        if (m.GetType().Name == "InternalModuleBuilder") // nice one .NET 4...
        {
          return true;
        }
        return false;
      }

      return PAL.IsTransient(mb);
    }

    static bool IsSimpleCall(MethodCallExpression mce)
    {
      if (mce.Instance != null && !IsSimpleExpression(mce.Instance))
      {
        return false;
      }

      foreach (var arg in mce.Arguments)
      {
        if (arg is MethodCallExpression)
        {
          MethodCallExpression imce = (MethodCallExpression)arg;

          if (!IsSimpleCall(imce))
          {
            return false;
          }
        }

        if (!IsSimpleExpression(arg))
        {
          return false;
        }

      }

      return true;
    }

    static CodeBlock RewriteBody(CodeBlock cb)
    {
      CodeBlock ncb = Ast.CodeBlock("temp-inline:" + cb.Name, cb.ReturnType);
      ncb.Parent = cb.Parent;

      foreach (var item in cb.Parameters)
      {
        Variable newvar = ncb.CreateParameter(item.Name, item.Type);
        newvar.Lift = item.Lift;
      }

      foreach (var item in cb.Variables)
      {
        Variable newvar = ncb.CreateLocalVariable(item.Name, item.Type);
        newvar.Lift = item.Lift;
      }

      Expression body = ((ReturnStatement)cb.Body).Expression;

      body = RewriteExpression(ncb, body);

      ncb.Body = Ast.Return(body);

      return ncb;
    }

    static Expression RewriteExpression(CodeBlock cb, Expression e)
    {
      if (e is MethodCallExpression)
      {
        MethodCallExpression mce = (MethodCallExpression)e;
        List<Expression> args = new List<Expression>();
        foreach (var arg in mce.Arguments)
        {
          args.Add(RewriteExpression(cb, arg));
        }

        return Ast.Call(RewriteExpression(cb, mce.Instance), mce.Method, args.ToArray());
      }
      if (e is BoundExpression)
      {
        BoundExpression be = (BoundExpression)e;
        return Ast.Read(cb.Lookup(be.Variable.Name));
      }

      if (e is BinaryExpression)
      {
        BinaryExpression be = (BinaryExpression)e;
        return new BinaryExpression(be.NodeType, RewriteExpression(cb, be.Left), RewriteExpression(cb, be.Right));
      }

      if (e is UnaryExpression)
      {
        UnaryExpression ue = (UnaryExpression)e;
        if (ue.NodeType == AstNodeType.Convert)
        {
          return Ast.ConvertHelper(RewriteExpression(cb, ue.Operand), ue.Type);
        }
        return null;
      }

      if (e is TypeBinaryExpression)
      {
        TypeBinaryExpression tbe = (TypeBinaryExpression)e;
        return Ast.TypeIs(RewriteExpression(cb, tbe.Expression), tbe.TypeOperand);
      }

      if (e is ConditionalExpression)
      {
        ConditionalExpression ce = (ConditionalExpression)e;
        return Ast.Condition(RewriteExpression(cb, ce.Test), RewriteExpression(cb, ce.IfTrue), RewriteExpression(cb, ce.IfFalse));
      }

      return e;
    }

    class CompileTimeEvaluationException : Exception
    {
    }

    protected internal static Expression GetAst(object args, CodeBlock cb, bool istailposition)
    {
      var spanHint = SpanHint;

      if (args is Annotation)
      {
        args = ((Annotation)args).stripped;
      }
      Cons c = args as Cons;
      if (c != null)
      {
        // in debug mode, this will be (annotated-call <location> . symbol)
        // TODO: see if the discarded annotation is useful
        if (ScriptDomainManager.Options.DebugMode && c.car is Cons)
        {
          Cons ac = c.car as Cons;
          if (ac.car == SymbolTable.StringToObject("annotated-call"))
          {
            object se = ((Cons)ac.cdr).cdr;
            if (se is SymbolId)
            {
              c.car = se;
            }
          }
        }
        
        if (c.car is SymbolId)
        {
          SymbolId f = (SymbolId)c.car;

          Variable var = cb.Lookup(f);

          if (var != null && !assigns.ContainsKey(f))
          {
            var = null;
          }

          object m;

#if OPTIMIZATIONS

#if !BLAH
          CodeBlockExpression cbe;

          //// needs to do the same for overloads...
          if (SimpleGenerator.libraryglobals.TryGetValue(f, out cbe))
          {
            Expression[] ppp = GetAstListNoCast(c.cdr as Cons, cb);

            if (cbe.Block.ParameterCount < 9 && cbe.Block.ParameterCount == ppp.Length)
            {
              //inline here? we could for simple bodies, but we need to copy the entire structure
              if (!(cbe.Block.HasEnvironment || cbe.Block.IsClosure))
              {
                if (cbe.Block.Body is ReturnStatement)
                {
                  ReturnStatement rs = (ReturnStatement)cbe.Block.Body;

                  if (!cb.IsGlobal && IsSimpleExpression(rs.Expression))
                  {
                    return InlineCall(cb, Ast.CodeBlockExpression(RewriteBody(cbe.Block), false, cbe.IsStronglyTyped), ppp);
                  }
                }
              }
              if (cbe.Block != cb.Parent && cbe.Block != cb) // do TCE later
              {
                return CallNormal(cbe, ppp);
              }
            }
          }

          // varargs
          if (SimpleGenerator.libraryglobalsX.TryGetValue(f, out cbe))
          {
            Expression[] ppp = GetAstListNoCast(c.cdr as Cons, cb);

            if (cbe.Block.ParameterCount < 9 && cbe.Block.ParameterCount - 1 <= ppp.Length)
            {
              //inline here?
              return CallVarArgs(cbe, ppp);
            }
          }

          // overloads
          CodeBlockDescriptor[] cbd;
          if (SimpleGenerator.libraryglobalsN.TryGetValue(f, out cbd))
          {
            Expression[] ppp = GetAstListNoCast(c.cdr as Cons, cb);

            foreach (CodeBlockDescriptor d in cbd)
            {
              if (d.codeblock.Block.ParameterCount < 9)
              {
                if (ppp.Length == d.arity || (d.varargs && ppp.Length > d.arity))
                {
                  if (d.varargs)
                  {
                    //inline here?
                    return CallVarArgs(d.codeblock, ppp);
                  }
                  else
                  {
                    //inline here?
                    //if (d.codeblock.Block != cb.Parent && d.codeblock.Block != cb) // do TCE later, not yet
                    {
                      return CallNormal(d.codeblock, ppp);
                    }
                  }
                }
              }
            }
          }
#endif

          //if (!ScriptDomainManager.Options.DebugMode)
          {

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
                    CodeBlockExpression ccbe = consumer.Arguments[0] as CodeBlockExpression;
                    CodeBlockExpression pcbe = producer.Arguments[0] as CodeBlockExpression;

                    pcbe.Block.Bind();
                    ccbe.Block.Bind();

                    if (ccbe.Block.ParameterCount == 0)
                    {
                      return InlineCall(cb, ccbe);
                    }
                    else if (ccbe.Block.ParameterCount == 1)
                    {
                      return InlineCall(cb, ccbe, Ast.SimpleCallHelper(typeof(Helpers).GetMethod("UnwrapValue"), InlineCall(cb, pcbe)));
                    }
                    else
                    {
                      Variable values = cb.CreateTemporaryVariable((SymbolId)Builtins.GenSym("values"), typeof(object[]));

                      Expression valuesarr = Ast.Read(values);

                      Expression[] pppp = new Expression[ccbe.Block.ParameterCount];

                      for (int i = 0; i < pppp.Length; i++)
                      {
                        pppp[i] = Ast.ArrayIndex(valuesarr, Ast.Constant(i));
                      }

                      return Ast.Comma(
                         Ast.Void(
                          Ast.Write(
                            values, 
                            Ast.ComplexCallHelper(
                              Ast.SimpleCallHelper(typeof(Helpers).GetMethod("WrapValue"), InlineCall(cb, pcbe)), 
                              typeof(MultipleValues).GetMethod("ToArray", new Type[] { typeof(int) }), 
                              Ast.Constant(pppp.Length)))), 
                          InlineCall(cb, ccbe, pppp));
                    }
                  }
                }
                if (consumer.Method == Closure_Make)
                {
                  CodeBlockExpression ccbe = consumer.Arguments[0] as CodeBlockExpression;
                  ccbe.Block.Bind();

                  Expression producer = ppp[0];

                  Expression exx = Ast.ConvertHelper(producer, typeof(Callable));

                  MethodInfo callx = GetCallable(0);

                  if (ccbe.Block.ParameterCount == 0)
                  {
                    return InlineCall(cb, ccbe);
                  }
                  else if (ccbe.Block.ParameterCount == 1)
                  {
                    return InlineCall(cb, ccbe, Ast.SimpleCallHelper(typeof(Helpers).GetMethod("UnwrapValue"), Ast.Call(exx, callx)));
                  }
                  else
                  {
                    Variable values = cb.CreateTemporaryVariable((SymbolId)Builtins.GenSym("values"), typeof(object[]));

                    Expression valuesarr = Ast.Read(values);

                    Expression[] pppp = new Expression[ccbe.Block.ParameterCount];

                    for (int i = 0; i < pppp.Length; i++)
                    {
                      pppp[i] = Ast.ArrayIndex(valuesarr, Ast.Constant(i));
                    }

                    return Ast.Comma(
                        Ast.Void(
                          Ast.Write(
                            values, 
                            Ast.ComplexCallHelper(
                              Ast.SimpleCallHelper(typeof(Helpers).GetMethod("WrapValue"),  
                                                   Ast.Call(exx, callx)), 
                              typeof(MultipleValues).GetMethod("ToArray", new Type[] { typeof(int) }), 
                              Ast.Constant(pppp.Length)))), 
                          InlineCall(cb, ccbe, pppp));
                  }
                }
              }
              else
              {
                ;
              }
            }
          }

#endif
          // this can be enabled once builtins are auto CPS'd.
          // ok I tried, but there are issues still, not sure what
#if OPTIMIZATIONS
          // check for inline emitter
          InlineEmitter ie;
          if (TryGetInlineEmitter(f, out ie))
          {
            Expression result = ie(GetAstList(c.cdr as Cons, cb));
            // if null is returned, the method cannot be inlined
            if (result != null)
            {
              if (spanHint.IsValid)
              {
                result.SetLoc(spanHint);
              }
              return result;
            }
          }
#endif

          if (Context.Scope.TryLookupName(f, out m))
          {
            if (var == null)
            {
              IGenerator gh = m as IGenerator;
              if (gh != null)
              {
                return gh.Generate(c.cdr, cb);
              }

              BuiltinMethod bf = m as BuiltinMethod;
              if (bf != null)
              {
                MethodBinder mb = bf.Binder;
                Expression[] pars = Array.ConvertAll(GetAstList(c.cdr as Cons, cb), e => Unwrap(e));

                if (bf.AllowConstantFold && !Debugger.IsAttached)
                {
                  bool constant = Array.TrueForAll(pars, e => e is ConstantExpression && e.Type != typeof(BigInteger));

                  if (constant)
                  {
                    object[] cargs = Array.ConvertAll(pars, e => GetRuntimeConstant((ConstantExpression)e));
                    CallTarget0 disp = delegate
                    {
                      return bf.Call(cargs);
                    };
                    CallTarget1 handler = delegate(object e)
                    {
                      throw new CompileTimeEvaluationException();
                    };

                    try
                    {
                      object result = WithExceptionHandler(
                        Closure.Create(handler),
                        Closure.Create(disp));
                      var rrrr = GetCons(result, cb);
                      if (spanHint.IsValid)
                      {
                        rrrr.SetLoc(spanHint);
                      }
                      return rrrr;
                    }
                    catch (CompileTimeEvaluationException)
                    {
                    }
                  }
                }

                Type[] types = GetExpressionTypes(pars);
                MethodCandidate mc = mb.MakeBindingTarget(CallType.None, types);
                if (mc != null)
                {
                  if (mc.Target.NeedsContext)
                  {
                    pars = ArrayUtils.Insert<Expression>(Ast.CodeContext(), pars);
                  }
                  MethodBase meth = mc.Target.Method;

                  var rrrr = Ast.ComplexCallHelper(meth as MethodInfo, pars);
                  if (spanHint.IsValid)
                  {
                    rrrr.SetLoc(spanHint);
                  }
                  return rrrr;
                }
              }

              Closure clos = m as Closure;
              if (clos != null && !SetGenerator.IsAssigned(f))
              {

                // no provision for varargs
                MethodInfo[] mis = clos.Targets;
                if (mis.Length > 0)
                {
                  MethodBinder mb = MethodBinder.MakeBinder(binder, SymbolTable.IdToString(f), mis, BinderType.Normal);

                  Expression[] pars = Array.ConvertAll(GetAstList(c.cdr as Cons, cb), e => Unwrap(e));

                  if (clos.AllowConstantFold && !Debugger.IsAttached)
                  {
                    bool constant = Array.TrueForAll(pars, e => e is ConstantExpression && e.Type != typeof(BigInteger));

                    if (constant)
                    {
                      object[] cargs = Array.ConvertAll(pars, e => GetRuntimeConstant((ConstantExpression)e));

                      CallTarget0 disp = delegate
                      {
                        var rrrr = clos.Call(cargs);
                        return rrrr;
                      };

                      CallTarget1 handler = delegate (object e)
                      {
                        throw new CompileTimeEvaluationException();
                      };

                      try
                      {
                        object result = WithExceptionHandler(
                          Closure.Create(handler),
                          Closure.Create(disp));
                        var rrrr = GetCons(result, cb);
                        if (spanHint.IsValid)
                        {
                          rrrr.SetLoc(spanHint);
                        }
                        return rrrr;
                      }
                      catch (CompileTimeEvaluationException)
                      {
                      }
                    }
                  }

                  // exclude transient members if needed
                  if (!AllowTransientBinding)
                  {
                    mis = Array.FindAll(mis, x => !IsTransient(x.Module));
                  }

                  if (mis.Length > 0)
                  {
                    Type[] types = GetExpressionTypes(pars);
                    MethodCandidate mc = mb.MakeBindingTarget(CallType.None, types);
                    if (mc != null)
                    {
                      if (mc.Target.NeedsContext)
                      {
                        pars = ArrayUtils.Insert<Expression>(Ast.CodeContext(), pars);
                      }
                      MethodBase meth = mc.Target.Method;

                      var rrrr = Ast.ComplexCallHelper(meth as MethodInfo, pars);
                      if (spanHint.IsValid)
                      {
                        rrrr.SetLoc(spanHint);
                      }
                      return rrrr;
                    }
                  }
                }
                // check for overload thing
              }
            }
          }
        }

       
        Expression ex = Unwrap(GetAst(c.car, cb));

        // a 'let'
        if (ex is MethodCallExpression)
        {
          var ppp = GetAstList(c.cdr as Cons, cb);
          MethodCallExpression mcexpr = (MethodCallExpression)ex;
          if (mcexpr.Method == Closure_Make)
          {
            CodeBlockExpression cbe = mcexpr.Arguments[0] as CodeBlockExpression;

            if (ppp.Length < 9 && cbe.Block.ParameterCount == ppp.Length)
            {
              return InlineCall(cb, cbe, istailposition, ppp);
            }
          }

          // cater for varargs more efficiently, this does not seem to hit, probably needed somewhere else, hits in ironscheme-test and cut test
          if (mcexpr.Method == Closure_MakeVarArgsX)
          {
            CodeBlockExpression cbe = mcexpr.Arguments[0] as CodeBlockExpression;

            if (ppp.Length < 9 && cbe.Block.ParameterCount <= ppp.Length && !NeedsContext(cbe))
            {
              // TODO: See why having a context fails here
              return CallVarArgs(cbe, ppp);
            }
          }
        }

        if (ex is NewExpression && typeof(ITypedCallable).IsAssignableFrom(ex.Type))
        {
          NewExpression mcexpr = ex as NewExpression;
          CodeBlockExpression cbe = mcexpr.Arguments[0] as CodeBlockExpression;
          if (cbe == null && mcexpr.Arguments[0].Type == typeof(CodeContext) && mcexpr.Arguments[0] is ConstantExpression) // implies null
          {
            cbe = mcexpr.Arguments[1] as CodeBlockExpression;
          }
          if (cbe != null)
          {
            var ppp = GetAstListNoCast(c.cdr as Cons, cb);

            if (ppp.Length < 9 && cbe.Block.ParameterCount == ppp.Length)
            {
              return InlineCall(cb, cbe, istailposition, ppp);
            }
          }
        }

        if (ex is ConstantExpression)
        {
          SyntaxError(SymbolTable.StringToObject("generator"), "expecting a procedure", c.car, c);
        }

        Expression r = null;

        if (ex.Type.Name.StartsWith("TypedClosure"))
        {
          Expression[] pp = GetAstListNoCast(c.cdr as Cons, cb);

          var m = ex.Type.GetMethod("Invoke");
          //TODO: add more checks, should we attempt some casting for types?
          if (m.GetParameters().Length != pp.Length)
          {
            SyntaxError(SymbolTable.StringToObject("apply-typed-lambda"), 
              string.Format("incorrect number of parameters, expected {0} got {1}", m.GetParameters().Length, pp.Length),
              c.car, c);
          }
          r = Ast.SimpleCallHelper(ex, m, pp);
        }
        else
        {
          Expression[] pp = GetAstList(c.cdr as Cons, cb);

          if (ex.Type != typeof(Callable))
          {
            ex = Ast.ConvertHelper(ex, typeof(Callable));
          }

          MethodInfo call = GetCallable(pp.Length);

          r = pp.Length > 8 ?
            Ast.Call(ex, call, Ast.NewArray(typeof(object[]), pp)) :
            Ast.Call(ex, call, pp);
        }

        if (spanHint.IsValid)
        {
          r.SetLoc(spanHint);
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
        Expression[] ba = Array.ConvertAll(args as byte[], b => Ast.Constant(b));
        return Ast.NewArray(typeof(byte[]), ba);
      }
      else
      {
        if (args is SymbolId)
        {
          SymbolId sym = (SymbolId)args;
          if (sym == SymbolTable.StringToId("uninitialized"))
          {
            return Ast.ReadField(null, typeof(Uninitialized), "Instance");
          }
          else
          {
            return Read(sym, cb, typeof(object));
          }
        }
        if (args == Builtins.Unspecified)
        {
          return Ast.ReadField(null, Unspecified);
        }
        if (args is Fraction)
        {
          Fraction f = (Fraction)args;
          return Ast.Constant( new FractionConstant(f));
        }
        if (args is ComplexFraction)
        {
          ComplexFraction f = (ComplexFraction)args;
          return Ast.Constant(new ComplexFractionConstant(f));
        }
        if (args != null && args.GetType().Name == "stx")
        {
          args = new SerializedConstant(args);
        }
        return Ast.Constant(args);
      }
    }

    static object GetRuntimeConstant(ConstantExpression ce)
    {
      if (ce.Value is Microsoft.Scripting.Generation.CompilerConstant)
      {
        var cc = (Microsoft.Scripting.Generation.CompilerConstant)ce.Value;
        return cc.Create();
      }
      return ce.Value;
    }

    protected static Expression InlineCall(CodeBlock parent, CodeBlockExpression cbe, params Expression[] pp)
    {
      return InlineCall(parent, cbe, false, pp);
    }

    protected internal static Expression InlineCall(CodeBlock parent, CodeBlockExpression cbe, bool istailpostion, params Expression[] pp)
    {
      // all var names are unique.
      CodeBlock cb = cbe.Block;

      if (parent.IsGlobal) 
      {
        return CallNormal(cbe, pp);
      }

      List<Statement> assigns = new List<Statement>();
      int i = 0;

      cb.Inlined = true;

      if (parent.Filename == null && cb.Filename != null)
      {
        parent.Filename = cb.Filename;
      }

      var parentvars = new List<Variable>(parent.Variables);

      foreach (Variable p in cb.Parameters)
      {
        SymbolId origname = p.Name;

        p.Name = (SymbolId)Builtins.GenSym(p.Name);
        p.Block = parent;
        p.Kind = Variable.VariableKind.Local;
        parent.AddVariable(p);

        Expression val = Unwrap(pp[i]);
        if (val.Type != typeof(SymbolId) && !Generator.assigns.ContainsKey(origname))
        {
          if (p.Type == typeof(object) && val.Type != typeof(void))
          {
            p.Type = val.Type;
            assigns.Add(Ast.Write(p, val));
          }
          else
          {
            assigns.Add(Ast.Write(p, Ast.ConvertHelper(val, p.Type)));
          }
        }
        else
        {
          if (p.Type == typeof(object) && val.Type != typeof(void))
          {
            assigns.Add(Ast.Write(p, pp[i]));
          }
          else
          {
            assigns.Add(Ast.Write(p, Ast.ConvertHelper(pp[i], p.Type)));
          }
        }
          
        if (p.Lift)
        {
          parent.HasEnvironment = true;
        }
        i++;
      }

      foreach (Variable l in cb.Variables)
      {
        if (l.DefaultValue == null && l.Kind != Variable.VariableKind.Global)
        {
          l.Name = (SymbolId)Builtins.GenSym(l.Name);
        }
        l.Block = parent;
        parent.AddVariable(l);

        if (l.Lift)
        {
          parent.HasEnvironment = true;
        }
      }

      Expression body = RewriteReturn(cb.Body);

      if (assigns.Count > 0)
      {
        return Ast.Comma(Ast.Void(Ast.Block(assigns)), body);
      }
      else
      {
        return body;
      }
    }

    static Statement FlattenStatement(Statement s)
    {
      if (s is BlockStatement)
      {
        BlockStatement bs = (BlockStatement)s;
        return FlattenStatements(bs.Statements);
      }
      return s;
    }

    static Statement FlattenStatements(IEnumerable<Statement> bs)
    {
      List<Statement> stmts = new List<Statement>();
      foreach (Statement o in bs)
      {
        if (o != null)
        {
          stmts.Add(o);
        }
      }

      if (stmts.Count == 0)
      {
        return null;
      }
      else if (stmts.Count == 1)
      {
        return stmts[0];
      }
      else
      {
        return Ast.Block(stmts);
      }
    }

    static Expression RewriteReturn(Statement statement)
    {
      if (statement is BlockStatement)
      {
        BlockStatement bs = (BlockStatement)statement;
        List<Statement> newbody = new List<Statement>(bs.Statements);
        Statement last = newbody[newbody.Count - 1];
        
        newbody.RemoveAt(newbody.Count - 1);

        Statement fb = FlattenStatement(Ast.Block(newbody));

        Expression eb = Ast.Void(fb);

        if (fb is ExpressionStatement)
        {
          eb = ((ExpressionStatement)fb).Expression;
        }

        return Ast.Comma(eb, Unwrap(RewriteReturn(last)));
      }

      if (statement is ReturnStatement)
      {
        Expression e = ((ReturnStatement)statement).Expression;
        if (e is MethodCallExpression)
        {
          ((MethodCallExpression)e).TailCall = false;
        }
        if (e.Type != typeof(object))
        {
          return e;
        }
        else
        {
          return Unwrap(e);
        }
      }

      if (statement is IfStatement)
      {
        IfStatement ifs = (IfStatement)statement;

        Debug.Assert(ifs.Tests.Count == 1);
        var a = Unwrap(RewriteReturn(ifs.Tests[0].Body));
        var b = Unwrap(RewriteReturn(ifs.ElseStatement));
        if (a.Type != b.Type)
        {
          a = Ast.ConvertHelper(a, typeof(object));
          b = Ast.ConvertHelper(b, typeof(object));
        }
        return Ast.Condition(ifs.Tests[0].Test, a, b);
      }

      if (statement is LabeledStatement)
      {
        var ls = statement as LabeledStatement;
        return Ast.Void(ls);
      }

      throw new ArgumentException("Unexpected");
    }


#region Optimized calls

    static bool TryGetInlineEmitter(SymbolId f, out InlineEmitter ie)
    {
      return inlineemitters.TryGetValue(f, out ie);
    }
    
    protected static Expression CallNormal(CodeBlockExpression cbe, params Expression[] ppp)
    {
      bool needscontext = NeedsContext(cbe); // true;
      int pc = ppp.Length;
      MethodInfo dc = GetDirectCallable(needscontext, pc, cbe.Type);

      List<Variable> paruninit = new List<Variable>(cbe.Block.Parameters);

      for (int i = 0; i < ppp.Length; i++)
      {
        if (ppp[i].Type == typeof(Uninitialized))
        {
          paruninit[i].SetUnInitialized();
        }
      }

      if (needscontext)
      {
        ppp = ArrayUtils.Insert<Expression>(Ast.CodeContext(), ppp);
      }

      var delegatetype = cbe.Type != typeof(Delegate) ? cbe.Type : CallTargets.GetTargetType(needscontext, pc, false);

      cbe = Ast.CodeBlockReference(cbe.Block, delegatetype);

      cbe.Block.Bind();

      var r = Ast.ComplexCallHelper(cbe, dc, ppp);
      if (SpanHint.IsValid)
      {
        r.SetLoc(SpanHint);
      }
      return r;
    }

    static bool NeedsContext(CodeBlockExpression cbe)
    {
      return cbe.Block.IsClosure || 
        cbe.Block.ExplicitCodeContextExpression == null && 
        (cbe.Block.Parent != null && !cbe.Block.Parent.IsGlobal);
    }

    protected static Expression CallVarArgs(CodeBlockExpression cbe, Expression[] ppp)
    {
      bool needscontext = NeedsContext(cbe); //true;

      int pc = cbe.Block.ParameterCount;

      Expression[] tail = new Expression[ppp.Length - (pc - 1)];

      Array.Copy(ppp, ppp.Length - tail.Length, tail, 0, tail.Length);

      Expression[] nppp = new Expression[pc];

      Array.Copy(ppp, nppp, ppp.Length - tail.Length);

      if (tail.Length > 0)
      {
        nppp[nppp.Length - 1] = Ast.ComplexCallHelper(MakeList(tail, true), tail);
      }
      else
      {
        nppp[nppp.Length - 1] = Ast.Null();
      }

      ppp = nppp;

      MethodInfo dc = GetDirectCallable(needscontext, pc, cbe.Type);
      if (needscontext)
      {
        ppp = ArrayUtils.Insert<Expression>(Ast.CodeContext(), ppp);
      }

      cbe = Ast.CodeBlockReference(cbe.Block, CallTargets.GetTargetType(needscontext, pc, false));

      cbe.Block.Bind();

      var r = Ast.ComplexCallHelper(cbe, dc, ppp);
      if (SpanHint.IsValid)
      {
        r.SetLoc(SpanHint);
      }
      return r;
    }

#endregion

    protected internal static Expression Unwrap(Expression ex)
    {
      while (ex is UnaryExpression && ex.NodeType == AstNodeType.Convert)
      {
        ex = ((UnaryExpression)ex).Operand;
      }

      return ex;
    }

  }

}
