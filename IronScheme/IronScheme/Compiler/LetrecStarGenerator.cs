#region License
/* Copyright (c) 2007-2014 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System.Collections.Generic;
using IronScheme.Runtime;
using Microsoft.Scripting;
using Microsoft.Scripting.Ast;
using IronScheme.Runtime.Typed;

namespace IronScheme.Compiler
{

  [Generator("letrec*")]
  sealed class LetrecStarGenerator : SimpleGenerator
  {
    public override Expression Generate(object args, CodeBlock c)
    {
      var refs = ClrGenerator.SaveReferences();

      NameHint = SymbolTable.StringToId("");
      CodeBlock cb = Ast.CodeBlock(SpanHint, GetLambdaName(c));
      cb.Parent = c;

      List<Variable> vars = new List<Variable>();
      List<object> defs = new List<object>();

      Cons a = (args as Cons).car as Cons;

      while (a != null)
      {
        Cons d = a.car as Cons;
        Variable r = Create((SymbolId)d.car, cb, typeof(object));
        r.SetUnInitialized();
        vars.Add(r);
        defs.Add(((Cons)d.cdr).car);

        a = a.cdr as Cons;
      }

      List<Statement> stmts = new List<Statement>();

      var notstrict = !ScriptDomainManager.Options.StrictMode;

      for (int i = 0; i < vars.Count; i++)
      {
        NameHint = Builtins.UnGenSymInternal(vars[i].Name);
        //VarHint = vars[i].Name;
        Expression e = GetAst(defs[i], cb);

        if (e is MethodCallExpression)
        {
          MethodCallExpression mce = (MethodCallExpression)e;
          if (mce.Method == Closure_Make || mce.Method == Closure_MakeCase || mce.Method == Closure_MakeVarArgsX)
          {
            if (notstrict)
            {
              vars[i].SetInitialized();
              vars[i].Type = typeof(Callable);
            }
          }
        }
        else if (e is NewExpression)
        {
          var ne = (NewExpression)e;
          if (typeof(ITypedCallable).IsAssignableFrom(ne.Type))
          {
            if (notstrict)
            {
              vars[i].SetInitialized();
              vars[i].Type = ne.Type;
            }
          }
        }

        if (e is UnaryExpression && e.NodeType == AstNodeType.Convert && e.Type != typeof(object))
        {
          if (notstrict)
          {
            vars[i].SetInitialized();
            vars[i].Type = e.Type;
          }
        }
        else if (e.Type.IsValueType)
        {
          e = Ast.ConvertHelper(e, typeof(object));
        }
        stmts.Add(Ast.Write(vars[i], e));
      }

      NameHint = SymbolId.Invalid;

      Cons body = Builtins.Cdr(args) as Cons;

      FillBody(cb, stmts, body, true);

#if OPTIMIZATIONS
      Expression ex = InlineCall(c, Ast.CodeBlockExpression(cb, false));
#else
      Expression ex = Ast.SimpleCallHelper(MakeClosure(cb, false), GetCallable(0));
#endif

      ClrGenerator.ResetReferences(refs);

      return ex;
    }
  }
}
