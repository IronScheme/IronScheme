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

using Generator = IronScheme.Compiler.Generator;

[assembly: Extension]

namespace IronScheme.Diagnostics
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

  [Generator("assert")]
  public class AssertGenerator : SimpleGenerator
  {
    static readonly MethodInfo Trace_Assert = typeof(Trace).GetMethod("Assert", new Type[] { typeof(bool), typeof(string) });

    public override Expression Generate(object args, CodeBlock cb)
    {
      object test = Builtins.First(args);
      string teststr = Builtins.DisplayFormat(test);

      return Ast.Comma(Ast.SimpleCallHelper(Trace_Assert,
        Ast.SimpleCallHelper(Builtins_IsTrue, Generator.GetAst(test, cb)), Ast.Constant(teststr)),
        Ast.ReadField(null, Unspecified));
    }
  }
}
