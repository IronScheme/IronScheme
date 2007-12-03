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
using System.Reflection;
using System.Diagnostics;
using Microsoft.Scripting.Ast;
using Microsoft.Scripting;
using IronScheme.Runtime;
using IronScheme.Compiler;

namespace IronScheme.Compiler
{
  [Generator("time")]
  public class TimeGenerator : SimpleGenerator
  {
    static readonly MethodInfo Stopwatch_StartNew = typeof(Stopwatch).GetMethod("StartNew");
    static readonly MethodInfo Stopwatch_Elapsed = typeof(Stopwatch).GetMethod("get_Elapsed");
    static readonly MethodInfo Console_WriteLine = typeof(Console).GetMethod("WriteLine", new Type[] { typeof(object) });

    public override Expression Generate(object args, CodeBlock cb)
    {
      Variable sw = cb.CreateTemporaryVariable(SymbolTable.StringToId("$timer"), typeof(Stopwatch));

      return Ast.Comma(1, Ast.Assign(sw, Ast.Call(Stopwatch_StartNew)),
        Generator.GetAst(Builtins.Car(args), cb),
        Ast.SimpleCallHelper(Console_WriteLine, Ast.SimpleCallHelper(Ast.Read(sw), Stopwatch_Elapsed)));
    }
  }
}
