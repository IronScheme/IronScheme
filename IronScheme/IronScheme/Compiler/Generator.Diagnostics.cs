using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using System.Diagnostics;
using Microsoft.Scripting.Ast;
using Microsoft.Scripting;
using IronScheme.Runtime;

namespace IronScheme.Compiler
{
  static partial class Generator
  {
    static readonly MethodInfo Stopwatch_StartNew = typeof(Stopwatch).GetMethod("StartNew");
    static readonly MethodInfo Stopwatch_Elapsed = typeof(Stopwatch).GetMethod("get_Elapsed");

    // trace timing
    public static Expression Time(object args, CodeBlock cb)
    {
      Variable sw = cb.CreateTemporaryVariable(SymbolTable.StringToId("$timer"), typeof(Stopwatch));

      return Ast.Comma(1, Ast.Assign(sw, Ast.Call(Stopwatch_StartNew)),
        GetAst(Builtins.Car(args), cb),
        Ast.SimpleCallHelper(Builtins_Display, Ast.SimpleCallHelper(Ast.Read(sw), Stopwatch_Elapsed)));

    }

  }
}
