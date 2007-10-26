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

[assembly: Extension(GeneratorType = typeof(IronScheme.Diagnostics.DiagGenerator))]

namespace IronScheme.Diagnostics
{
  public class DiagGenerator : Generator
  {
    static readonly MethodInfo Stopwatch_StartNew = typeof(Stopwatch).GetMethod("StartNew");
    static readonly MethodInfo Stopwatch_Elapsed = typeof(Stopwatch).GetMethod("get_Elapsed");
    static readonly MethodInfo Trace_Assert = typeof(Trace).GetMethod("Assert", new Type[] { typeof(bool), typeof(string) });
    //static readonly MethodInfo Builtins_IsTrue = typeof(Builtins).GetMethod("IsTrue");
    static readonly MethodInfo Console_WriteLine = typeof(Console).GetMethod("WriteLine", new Type[] { typeof(object) });

    // trace timing
    [Generator]
    public static Expression Time(object args, CodeBlock cb)
    {
      Variable sw = cb.CreateTemporaryVariable(SymbolTable.StringToId("$timer"), typeof(Stopwatch));

      return Ast.Comma(1, Ast.Assign(sw, Ast.Call(Stopwatch_StartNew)),
        Generator.GetAst(Builtins.Car(args), cb),
        Ast.SimpleCallHelper(Builtins_Display, Ast.SimpleCallHelper(Ast.Read(sw), Stopwatch_Elapsed)));
    }

    // assert
    [Generator]
    public static Expression Assert(object args, CodeBlock cb)
    {
      object test = Builtins.First(args);
      string teststr = Builtins.DisplayFormat(test);

      return Ast.Comma( Ast.SimpleCallHelper(Trace_Assert,
        Ast.SimpleCallHelper(Builtins_IsTrue, Generator.GetAst(test, cb)), Ast.Constant(teststr)), 
        Ast.ReadField(null, Unspecified));
    }
  }
}
