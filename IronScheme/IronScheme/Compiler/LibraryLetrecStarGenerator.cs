#region License
/* Copyright (c) 2007,2008,2009,2010,2011 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Collections.Generic;
using System.Reflection;
using IronScheme.Runtime;
using IronScheme.Runtime.Typed;
using Microsoft.Scripting;
using Microsoft.Scripting.Ast;

namespace IronScheme.Compiler
{
  // see expander.ss:3406
  //`(library-letrec* name ,(map list vars locs val-exps) ,body-exp))
  [Generator("library-letrec*")]
  public sealed class LibraryLetrecStarGenerator : SimpleGenerator
  {
    static MethodInfo SetSymbolValue = typeof(Builtins).GetMethod("SetSymbolValueFast");

    static string SymbolToString(object sym)
    {
      return SymbolTable.IdToString((SymbolId)sym);
    }

    readonly static object CASELAMBDA = SymbolTable.StringToObject("case-lambda");
    readonly static object ANNCASELAMBDA = SymbolTable.StringToObject("annotated-case-lambda");
    readonly static object TYPEDCASELAMBDA = SymbolTable.StringToObject("typed-case-lambda");
    readonly static object ANNTYPEDCASELAMBDA = SymbolTable.StringToObject("annotated-typed-case-lambda");


    static bool IsLambda(object obj, out bool annotated, out bool typed)
    {
      Cons c = obj as Cons;
      if (c == null)
      {
        annotated = false;
        typed = false;
        return false;
      }
      if (c.car == CASELAMBDA)
      {
        annotated = false;
        typed = false;
        return true;
      }
      if (c.car == ANNCASELAMBDA)
      {
        annotated = true;
        typed = false;
        return true;
      }
      if (c.car == TYPEDCASELAMBDA)
      {
        annotated = false;
        typed = true;
        return true;
      }
      if (c.car == ANNTYPEDCASELAMBDA)
      {
        annotated = true;
        typed = true;
        return true;
      }
      annotated = false;
      typed = false;
      return false;
    }

    static object Copy(object o)
    {
      if (o is Cons)
      {
        Cons c = o as Cons;
        return new Cons(Copy(c.car), Copy(c.cdr));
      }
      if (o is object[])
      {
        object[] a = o as object[];
        return Array.ConvertAll<object,object>(a, Copy);
      }
      return o;
    }

    static int GetDepth(object o)
    {
      if (o is Cons)
      {
        Cons c = o as Cons;
        return Math.Max(1 + GetDepth(c.car), GetDepth(c.cdr));
      }
      return 1;
    }

    public override Expression Generate(object args, CodeBlock c)
    {
      var ns = ClrGenerator.SaveReferences();

      Cons name = (args as Cons).car as Cons;

      string[] fullname = Array.ConvertAll<object, string>((object[])Builtins.ListToVector(name), SymbolToString);

      string n = string.Join(".", fullname);

      NameHint = SymbolTable.StringToId(n);
      CodeBlock cb = Ast.CodeBlock(SpanHint, GetLambdaName(c));
      cb.IsGlobal = true;

      List<SymbolId> vars = new List<SymbolId>();
      List<Variable> locals = new List<Variable>();
      List<Expression> assignments = new List<Expression>();

      List<object> defs = new List<object>();
      List<object> bodies = new List<object>();
      List<object> sources = new List<object>();

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
        sources.Add(null);
        bool annotated, typed;
        if (IsLambda(defs[i], out annotated, out typed))
        {
          Cons cl = defs[i] as Cons;

          
          sources[i] = Copy(cl);

          int depth = GetDepth(sources[i]);

          if (depth > 10)
          {
            sources[i] = null;
          }

          if (annotated)
          {
            cl = cl.cdr as Cons;
          }

          if (cl.cdr != null)
          {
            if (((Cons)cl.cdr).cdr == null)
            {
              Cons bh = (Cons)((Cons)cl.cdr).car;

              if (typed)
              {
                bh = bh.cdr as Cons;
              }

              Cons b = bh.cdr as Cons;
              bh.cdr = new Cons(Builtins.FALSE);

              bodies.Add(b);
            }
            else
            {
              List<Cons> cbs = new List<Cons>();

              Cons cc = cl.cdr as Cons;

              while (cc != null)
              {
                Cons bh = (Cons)cc.car;

                if (typed)
                {
                  bh = bh.cdr as Cons;
                }

                Cons b = bh.cdr as Cons;
                bh.cdr = new Cons(Builtins.FALSE);
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

        VarHint = locals[i].Name;
        VarHint2 = vars[i];

        NameHint = SymbolTable.StringToId(n + "::" + Builtins.UnGenSymInternal(vars[i]));
        Expression e = GetAst(defs[i], cb);
        VarHint = VarHint2 = SymbolId.Empty;

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
            var cbe= mce.Arguments[1] as CodeBlockExpression;
            cbe.Block.Source = sources[i];

            libraryglobals.Add(locals[i].Name, cbe);
            libraryglobals.Add(vars[i], cbe);
          }
          else if (mce.Method == Closure_MakeVarArgsX)
          {
            libraryglobalsX.Add(locals[i].Name, mce.Arguments[1] as CodeBlockExpression);
            libraryglobalsX.Add(vars[i], mce.Arguments[1] as CodeBlockExpression);
          }
          else if (mce.Method == Closure_MakeCase)
          {
            NewArrayExpression tcs = mce.Arguments[1] as NewArrayExpression;

            List<CodeBlockDescriptor> cdbs = new List<CodeBlockDescriptor>();

            foreach (CodeBlockExpression tc in tcs.Expressions)
            {
              //where do we clean this up??
              cdbs.Add(descriptorshack[tc]);
            }

            libraryglobalsN.Add(locals[i].Name, cdbs.ToArray());
            libraryglobalsN.Add(vars[i], cdbs.ToArray());
          }
          else if (mce.Method == Closure_MakeTypedCase)
          {
            NewArrayExpression tcs = mce.Arguments[1] as NewArrayExpression;

            List<CodeBlockDescriptor> cdbs = new List<CodeBlockDescriptor>();

            foreach (Expression tc in tcs.Expressions)
            {
              //where do we clean this up??
              cdbs.Add(descriptorshack2[tc]);
            }

            libraryglobalsN.Add(locals[i].Name, cdbs.ToArray());
            libraryglobalsN.Add(vars[i], cdbs.ToArray());
          }

        }
        else if (e is NewExpression)
        {
          NewExpression ne = e as NewExpression;
          if (typeof(ITypedCallable).IsAssignableFrom(e.Type))
          {
            libraryglobals.Add(locals[i].Name, ne.Arguments[1] as CodeBlockExpression);
            libraryglobals.Add(vars[i], ne.Arguments[1] as CodeBlockExpression);
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

        stmts.Add(Ast.Write(locals[i], e));
        // needed indeed
        stmts.Add(Ast.Statement(Ast.SimpleCallHelper(SetSymbolValue, Ast.Constant(vars[i]), Ast.Read(locals[i]))));
      }

      Cons body = Builtins.Cdr(args) as Cons;
      FillBody(cb, stmts, body, true);
      cb.ExplicitCodeContextExpression = Ast.CodeContext();
      
#if OPTIMIZATIONS
      Expression ex = InlineCall(c, Ast.CodeBlockExpression(cb, false));
#else
      Expression ex = Ast.SimpleCallHelper(MakeClosure(cb, false), GetCallable(0));
#endif

      NameHint = SymbolId.Invalid;
      ClrGenerator.ResetReferences(ns);
      return ex;
    }




  }
}
