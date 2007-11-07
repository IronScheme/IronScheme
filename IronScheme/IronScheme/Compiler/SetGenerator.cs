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
  [Generator("set!")]
  public class SetGenerator : SimpleGenerator
  {
    public override Expression Generate(object args, CodeBlock cb)
    {
      SymbolId s = (SymbolId)Builtins.First(args);

      NameHint = s;

      Expression value = GetAst(Builtins.Second(args), cb);

      NameHint = SymbolId.Invalid;

      Variable v = FindVar(cb, s);

      if (v == null)
      {
        throw new MissingMemberException(string.Format("name '{0}' not defined", SymbolTable.IdToString(s)));
      }

      if (value.Type.IsValueType)
      {
        value = Ast.DynamicConvert(value, typeof(object));
      }

      Expression r = Ast.Comma(Ast.Assign(v, value), Ast.ReadField(null, Unspecified));

      if (cb.IsGlobal)
      {
        object o = r.Evaluate(Context);
      }
      if (SpanHint != SourceSpan.Invalid || SpanHint != SourceSpan.None)
      {
        r.SetLoc(SpanHint);
      }



      return r;
    }
  }
}
