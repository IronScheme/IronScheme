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

namespace IronScheme.Compiler
{

  [Generator("letrec*")]
  public class LetrecStarGenerator : SimpleGenerator
  {
    int level = 0;
    public override Expression Generate(object args, CodeBlock c)
    {
      level++;
      NameHint = SymbolTable.StringToId("letrec*");
      CodeBlock cb = Ast.CodeBlock(SpanHint, GetLambdaName(c));
      cb.Parent = c;

      List<Variable> vars = new List<Variable>();
      List<object> defs = new List<object>();

      Cons a = (args as Cons).car as Cons;

      while (a != null)
      {
        Cons d = a.car as Cons;

        vars.Add(Create((SymbolId)d.car, cb, typeof(object)));
        defs.Add(((Cons)d.cdr).car);

        a = a.cdr as Cons;
      }

      List<Statement> stmts = new List<Statement>();

      for (int i = 0; i < vars.Count; i++)
      {
        NameHint = vars[i].Name;
        Expression e = GetAst(defs[i], cb);
        //if (e is MethodCallExpression && level == 1)
        //{
        //  MethodCallExpression mce = e as MethodCallExpression;
        //  if (mce.Method == Closure_Make)
        //  {
        //    if (mce.Arguments.Count > 1 && mce.Arguments[1] is CodeBlockExpression)
        //    {
        //      CodeBlockExpression cbe = mce.Arguments[1] as CodeBlockExpression;
        //      int pc = cbe.Block.Parameters.Count;
        //      cbe = Ast.CodeBlockReference(cbe.Block, pc > 5 ? typeof(CallTargetWithContextN) : CallTargets.GetTargetType(true, pc, false));
        //      Context.Scope.SetName(vars[i].Name, cbe);
        //    }
        //  }
        //}
        if (e.Type.IsValueType)
        {
          e = Ast.ConvertHelper(e, typeof(object));
        }
        stmts.Add(Ast.Write(vars[i], e));
      }

      for (int i = 0; i < vars.Count; i++)
      {
        if (Context.Scope.ContainsName(vars[i].Name))
        {
          Context.Scope.RemoveName(vars[i].Name);
        }
      }

      Cons body = Builtins.Cdr(args) as Cons;

      FillBody(cb, stmts, body, true);

      Expression ex = MakeClosure(cb, false);

      ex = Ast.ConvertHelper(ex, typeof(ICallable));

      Expression r =
        Ast.Call(ex, GetCallable(0));

      level--;
      return r;
    }
  }
}
