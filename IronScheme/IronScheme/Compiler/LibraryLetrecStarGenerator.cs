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

      List<object> defs = new List<object>();

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

      for (int i = 0; i < vars.Count; i++)
      {
        NameHint = Builtins.UnGenSym(vars[i]);
        Expression e = GetAst(defs[i], cb);

        if (e.Type.IsValueType)
        {
          e = Ast.ConvertHelper(e, typeof(object));
        }

        stmts.Add(Ast.Statement(Ast.SimpleCallHelper(SetSymbolValue, Ast.CodeContext(), Ast.Constant(vars[i]), Ast.Assign(locals[i], e))));
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
