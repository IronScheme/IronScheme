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
using System.Diagnostics;


namespace IronScheme.Compiler
{
  ////[Generator("apply-internal")]
  //public class ApplyGenerator : SimpleGenerator
  //{
  //  //procedure:  (apply proc arg1 ... args) 
  //  //Proc must be a procedure and args must be a list. Calls proc with the elements of the list (append (list arg1 ...) args) as the actual arguments.
  //  public override Expression Generate(object args, CodeBlock cb)
  //  {
  //    Cons c = args as Cons;
  //    Expression ex = Ast.ConvertHelper(GetAst(c.Car, cb), typeof(ICallable));
  //    Expression[] pp = GetAstList(c.Cdr as Cons, cb);

  //    Debug.Assert(pp.Length == 1);

  //    Expression r = Ast.SimpleCallHelper(ex, ICallable_Call, 
  //      Ast.Call(Builtins_ListToVector, pp[0]));
  //    return r;
  //  }
  //}
}
