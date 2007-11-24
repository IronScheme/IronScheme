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
  [Generator("define")]
  public class DefineGenerator : SimpleGenerator
  {
    public override Expression Generate(object args, CodeBlock cb)
    {

      object f = Builtins.First(args);
      if (f is SymbolId)
      {
        SymbolId s = (SymbolId)f;

        NameHint = s;

        // create variable before so expr can have access to it
        Variable v = Create(s, cb, typeof(object));

        Expression value = Builtins.Cdr(args) != null ? GetAst(Builtins.Second(args), cb) : Ast.ReadField(null, Unspecified);

        NameHint = SymbolId.Invalid;

        if (value.Type.IsValueType)
        {
          value = Ast.DynamicConvert(value, typeof(object));
        }
        //Expression r = Ast.Comma(Ast.Assign(v, value), Ast.ReadField(null, Unspecified));
        Expression r = Ast.Assign(v, value);
        if (cb.IsGlobal && cb.Name != "__toploop__")
        {
          object o = r.Evaluate(Context);
        }
        if (SpanHint != SourceSpan.Invalid || SpanHint != SourceSpan.None)
        {
          r.SetLoc(SpanHint);
        }
        return r;
      }
      throw new ArgumentException("expected symbol");
    }
  }
}
