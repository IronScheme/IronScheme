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

    public override Expression Generate(object args, CodeBlock c)
    {
      Cons name = (args as Cons).car as Cons;

      string[] fullname = Array.ConvertAll<object, string>((object[])Builtins.ListToVector(name), Builtins.SymbolToString);

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

          if (cl.cdr != null)
          {
            // cant handle overloads (case-lambda with 2 or more bodies)
            if (((Cons)cl.cdr).cdr == null)
            {
              Cons b = ((Cons)((Cons)cl.cdr).car).cdr as Cons;
              ((Cons)((Cons)cl.cdr).car).cdr = new Cons(Builtins.FALSE);

              bodies.Add(b);
            }
            else
            {
              List<Cons> cbs = new List<Cons>();

              Cons cc = cl.cdr as Cons;

              while (cc != null)
              {
                Cons b = ((Cons)cc.car).cdr as Cons;
                ((Cons)cc.car).cdr = new Cons(Builtins.FALSE);
                cbs.Add(b);
                cc = cc.cdr as Cons;
              }

              bodies.Add(cbs);
            }
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

        NameHint = SymbolTable.StringToId(n + "::" + Builtins.UnGenSymInternal(vars[i]));
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
          if (mce.Method == Closure_MakeVarArgsX)
          {
            libraryglobalsX.Add(locals[i].Name, mce.Arguments[1] as CodeBlockExpression);
            libraryglobalsX.Add(vars[i], mce.Arguments[1] as CodeBlockExpression);
          }

          if (mce.Method == Closure_MakeCase)
          {
            NewArrayExpression tcs = mce.Arguments[1] as NewArrayExpression;

            List<CodeBlockDescriptor> cdbs = new List<CodeBlockDescriptor>();

            foreach (CodeBlockExpression tc in tcs.Expressions)
            {
              cdbs.Add(descriptorshack[tc]);
            }

            libraryglobalsN.Add(locals[i].Name, cdbs.ToArray());
            libraryglobalsN.Add(vars[i], cdbs.ToArray());
          }
        }
      }

      // pass 2, expand lambda bodies
      for (int i = 0; i < vars.Count; i++)
      {
        Expression e = assignments[i];
        NameHint = SymbolTable.StringToId(n + "::" + Builtins.UnGenSymInternal(vars[i]));

        if (bodies[i] != null)
        {
          CodeBlockDescriptor[] cbds;
          CodeBlockExpression cbe;

          if (libraryglobals.TryGetValue(locals[i].Name, out cbe))
          {
            Cons b = bodies[i] as Cons;
            CodeBlock cbody = cbe.Block;
            cbody.Body = null;

            FillBody(cbody, new List<Statement>(), b, true);
          }
          else if (libraryglobalsX.TryGetValue(locals[i].Name, out cbe))
          {
            Cons b = bodies[i] as Cons;
            CodeBlock cbody = cbe.Block;
            cbody.Body = null;

            FillBody(cbody, new List<Statement>(), b, true);
          }
          else if (libraryglobalsN.TryGetValue(locals[i].Name, out cbds))
          {
            List<Cons> b = bodies[i] as List<Cons>;

            for (int j = 0; j < b.Count; j++)
            {
              CodeBlock cbody = cbds[j].codeblock.Block;
              cbody.Body = null;

              FillBody(cbody, new List<Statement>(), b[j], true);
            }
          }
        }

        stmts.Add(Ast.Statement(Ast.SimpleCallHelper(SetSymbolValue, Ast.Constant(vars[i]), Ast.Assign(locals[i], e))));
      }

      // pass 3, remove library locals
      for (int i = 0; i < vars.Count; i++)
      {
        if (libraryglobals.ContainsKey(locals[i].Name))
        {
          libraryglobals.Remove(locals[i].Name);
        }
        if (libraryglobalsX.ContainsKey(locals[i].Name))
        {
          libraryglobalsX.Remove(locals[i].Name);
        }
        if (libraryglobalsN.ContainsKey(locals[i].Name))
        {
          libraryglobalsN.Remove(locals[i].Name);
        }
      }

      Cons body = Builtins.Cdr(args) as Cons;
      FillBody(cb, stmts, body, true);
      cb.ExplicitCodeContextExpression = Ast.CodeContext();

      Expression ex = CallNormal(Ast.CodeBlockExpression(cb, false));
      NameHint = SymbolId.Invalid;
      return ex;
    }


  }
}
