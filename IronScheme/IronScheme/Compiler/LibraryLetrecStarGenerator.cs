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
using System.Reflection;
using Microsoft.Scripting.Generation;

namespace IronScheme.Compiler
{
  // see expander.ss:3406
  //`(library-letrec* name ,(map list vars locs val-exps) ,body-exp))
  [Generator("library-letrec*")]
  public sealed class LibraryLetrecStarGenerator : SimpleGenerator
  {
    static MethodInfo SetSymbolValue = typeof(Builtins).GetMethod("SetSymbolValue");

    int level = 0;
    public override Expression Generate(object args, CodeBlock c)
    {
      level++;

      Cons name = (args as Cons).car as Cons;

      string[] fullname = Array.ConvertAll<object, string>(Builtins.ListToVector(name), Builtins.SymbolToString);

      string n = string.Join(".", fullname);

      NameHint = SymbolTable.StringToId(n);
      CodeBlock cb = Ast.CodeBlock(SpanHint, GetLambdaName(c));
      cb.IsGlobal = true;

      List<SymbolId> vars = new List<SymbolId>();
      List<Variable> locals = new List<Variable>();
      List<Expression> assignments = new List<Expression>();

      List<object> defs = new List<object>();
      List<object> bodies = new List<object>();

      args = (args as Cons).cdr;

      Cons a = (args as Cons).car as Cons;

      while (a != null)
      {
        Cons d = a.car as Cons;

        SymbolId v = (SymbolId)d.car;
        SymbolId l = (SymbolId)((Cons)d.cdr).car;
        vars.Add(l);
        locals.Add(Create(v, cb, typeof(object)));
        defs.Add(((Cons)((Cons)d.cdr).cdr).car);

        a = a.cdr as Cons;
      }

      List<Statement> stmts = new List<Statement>();

      // pass 1
      for (int i = 0; i < vars.Count; i++)
      {
        if (defs[i] is Cons && Builtins.IsTrue(Builtins.IsEqual(((Cons)defs[i]).car, SymbolTable.StringToId("case-lambda"))))
        {
          Cons cl = defs[i] as Cons;

          object pars = ((Cons)((Cons)cl.cdr).car).car;

          // cant handle varargs or overloads (case-lambda with 2 or more bodies)
          if (((Cons)cl.cdr).cdr == null && pars is Cons && ((Cons)pars).IsProper)
          {

            Cons b = ((Cons)((Cons)cl.cdr).car).cdr as Cons;
            ((Cons)((Cons)cl.cdr).car).cdr = new Cons(Builtins.FALSE);

            bodies.Add(b);
          }
          else
          {
            bodies.Add(null);
          }
        }
        else
        {
          bodies.Add(null);
        }

        NameHint = Builtins.UnGenSym(vars[i]);
        Expression e = GetAst(defs[i], cb);

        if (e.Type.IsValueType)
        {
          e = Ast.ConvertHelper(e, typeof(object));
        }

        assignments.Add(e);

        if (e is MethodCallExpression)
        {
          MethodCallExpression mce = e as MethodCallExpression;
          if (mce.Method == Closure_Make)
          {
            libraryglobals.Add(locals[i].Name, mce.Arguments[1] as CodeBlockExpression);
            libraryglobals.Add(vars[i], mce.Arguments[1] as CodeBlockExpression);
          }
        }
      }

      // pass 2, expand lambda bodies
      for (int i = 0; i < vars.Count; i++)
      {
        Expression e = assignments[i];
        NameHint = Builtins.UnGenSym(vars[i]);

        if (bodies[i] != null)
        {
          Cons b = bodies[i] as Cons;

          CodeBlock cbody = libraryglobals[locals[i].Name].Block;
          cbody.Body = null; 

          FillBody(cbody, new List<Statement>(), b, true);
        }

        stmts.Add(Ast.Statement(Ast.SimpleCallHelper(SetSymbolValue, Ast.CodeContext(), Ast.Constant(vars[i]), Ast.Assign(locals[i], e))));
      }

      // pass 3, remove library locals
      for (int i = 0; i < vars.Count; i++)
      {
        if (libraryglobals.ContainsKey(locals[i].Name))
        {
          libraryglobals.Remove(locals[i].Name);
        }
      }

      NameHint = SymbolId.Invalid;

      Cons body = Builtins.Cdr(args) as Cons;

      FillBody(cb, stmts, body, true);

      MethodInfo dc = GetDirectCallable(true, 0);
      Expression ex = Ast.ComplexCallHelper(Ast.CodeBlockExpression(cb, false), dc, Ast.CodeContext());

      level--;
      return ex;
    }


  }
}
