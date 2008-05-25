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
  [Generator("set!")]
  public sealed class SetGenerator : SimpleGenerator
  {
    static MethodInfo SetSymbolValue = typeof(Builtins).GetMethod("SetSymbolValue");

    public override Expression Generate(object args, CodeBlock cb)
    {
      SymbolId s = (SymbolId)Builtins.First(args);

      assigns[s] = true;

      NameHint = Builtins.UnGenSym(s);

      Expression value = GetAst(Builtins.Second(args), cb);

      NameHint = SymbolId.Invalid;

      Variable v = FindVar(cb, s);

      if (value.Type.IsValueType)
      {
        value = Ast.ConvertHelper(value, typeof(object));
      }

      Expression r = null;

      if (v == null)
      {
        r = Ast.SimpleCallHelper(SetSymbolValue, Ast.CodeContext(), Ast.Constant(s), value);
      }
      else
      {
        r = Ast.Assign(v, value);
      }

      if (SpanHint != SourceSpan.Invalid || SpanHint != SourceSpan.None)
      {
        r.SetLoc(SpanHint);
      }

      return Ast.Comma(r, Ast.ReadField(null, Unspecified));
    }
  }
}
