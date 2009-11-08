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
using System.Diagnostics;

namespace IronScheme.Compiler
{
  [Generator("set!")]
  public sealed class SetGenerator : SimpleGenerator
  {
    static MethodInfo SetSymbolValue = typeof(Builtins).GetMethod("SetSymbolValueFast");
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

      Expression value = GetAst(Builtins.Second(args), cb);

      setstack.Pop();

      NameHint = SymbolId.Invalid;

      Variable v = cb.Lookup(s);

      if (value.Type.IsValueType)
      {
        value = Ast.ConvertHelper(value, typeof(object));
      }

      Statement r = null;

      if (v == null)
      {
        r = Ast.Statement(Ast.SimpleCallHelper(SetSymbolValue, Ast.Constant(s), value));
      }
      else
      {
        //Trace.Assert(cb.Parent != null);
        r = Ast.Write(v, value);
      }

      if (SpanHint != SourceSpan.Invalid || SpanHint != SourceSpan.None)
      {
        r.SetLoc(SpanHint);
      }

      return Ast.Void(r);
    }
  }
#if CPS
  [Generator("define")]
  public sealed class DefineGenerator : SimpleGenerator
  {
    static MethodInfo SetSymbolValue = typeof(Builtins).GetMethod("SetSymbolValue");

    public override Expression Generate(object args, CodeBlock cb)
    {
      SymbolId s = (SymbolId)Builtins.First(args);

      assigns[s] = true;

      NameHint = Builtins.UnGenSymInternal(s);

      Expression value = GetAst(Builtins.Second(args), cb);

      NameHint = SymbolId.Invalid;

      if (value.Type.IsValueType)
      {
        value = Ast.ConvertHelper(value, typeof(object));
      }

      Expression r = Ast.SimpleCallHelper(SetSymbolValue, Ast.Constant(s), value);

      return Ast.Comma(r, Ast.ReadField(null, Unspecified));
    }
  }
#endif
}
