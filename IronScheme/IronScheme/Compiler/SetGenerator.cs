#region License
/* Copyright (c) 2007-2015 Llewellyn Pritchard
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System.Collections.Generic;
using System.Reflection;
using IronScheme.Runtime;
using Microsoft.Scripting;
using Microsoft.Scripting.Ast;

namespace IronScheme.Compiler
{
  [Generator("set!")]
  sealed class SetGenerator : SimpleGenerator
  {
    static readonly MethodInfo SetSymbolValue = typeof(Builtins).GetMethod("SetSymbolValueFast");
    readonly static Stack<SymbolId> setstack = new Stack<SymbolId>();

    public static bool IsAssigned(SymbolId s)
    {
      return setstack.Contains(s);
    }

    public override Expression Generate(object args, CodeBlock cb)
    {
      SymbolId s = (SymbolId)Builtins.First(args);

      assigns[s] = true;

      if (libraryglobals.ContainsKey(s))
      {
        libraryglobals.Remove(s);
      }

      if (libraryglobalsN.ContainsKey(s))
      {
        libraryglobalsN.Remove(s);
      }

      if (libraryglobalsX.ContainsKey(s))
      {
        libraryglobalsX.Remove(s);
      }


      setstack.Push(s);

      NameHint = Builtins.UnGenSymInternal(s);
      var prevvh = VarHint;
      VarHint = s;
      Expression value = GetAst(Builtins.Second(args), cb);
      VarHint = prevvh;
      setstack.Pop();

      NameHint = SymbolId.Invalid;

      Variable v = cb.Lookup(s);
      Statement r = null;

      if (v == null)
      {
        r = Ast.Statement(Ast.SimpleCallHelper(SetSymbolValue, Ast.Constant(s), value));
      }
      else
      {
        //Trace.Assert(cb.Parent != null);
        value = Ast.ConvertHelper(value, v.Type);
        r = Ast.Write(v, value);
      }

      if (SpanHint != SourceSpan.Invalid || SpanHint != SourceSpan.None)
      {
        r.SetLoc(SpanHint);
      }

      return Ast.Void(r);
    }
  }
}
