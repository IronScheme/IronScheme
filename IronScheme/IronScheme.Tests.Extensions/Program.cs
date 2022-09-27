using System;
using System.Diagnostics;
using IronScheme;
using IronScheme.Runtime;

namespace IronSchemes.Tests.Extensions
{
  internal class Program
  {
    static void Main(string[] args)
    {
      HelloWorld();

      Issue136();

      Test1();

      Test2();

      Console.WriteLine();
    }

    private static void Test2()
    {
      "(define foo 1)".Eval();
      var r = "foo".Eval();
      Console.WriteLine(r);

      try
      {
        "(define foo 1)".EvalWithEnvironment("(environment '(rnrs))");
        Debug.Fail("should not get here");
      }
      catch (SchemeException ex)
      {
        Console.WriteLine(ex);
      }

      r= "(let () (define foo 1) foo)".EvalWithEnvironment("(environment '(rnrs))");
      Console.WriteLine(r);

      "(define foo displayln)".EvalWithEnvironment("(new-interaction-environment '(ironscheme))");

      try
      {
        "(define foo displayln)".EvalWithEnvironment("(new-interaction-environment '(rnrs))");
        Debug.Fail("should not get here");
      }
      catch (SchemeException ex)
      {
        Console.WriteLine(ex);
      }

    }

    private static void Test1()
    {
      var envStr = "(environment '(rnrs))";
      var env = envStr.Eval();

      try
      {
        var a1 = "hello world";
        var a2 = 12345678;

        var print = "(lambda (x) (displayln x) x)".Eval();

        Func<object, object> print2 = x =>
        {
          Console.WriteLine(x);
          return x;
        };

        var code = "(let ((l (list {1} {2} {1}))) (cons {0} (map {0} l)))";

        var r1 = code.Eval(print, a1, a2);
        var r2 = code.EvalWithEnvironment(envStr, print, a1, a2);
        var r3 = code.EvalWithEnvironmentInstance(env, print, a1, a2);
        var r4 = code.Eval(print2.ToSchemeProcedure(), a1, a2);
        var r5 = "(displayln {0})".Eval(new object());

        var r6 = "(cons {0})".Eval(new object());
        //Console.WriteLine("", r1, r2, r3, r4, r5);
      }
      catch (SchemeException ex)
      {
        Console.WriteLine(ex);
        "(displayln {0})".EvalWithEnvironment("(environment '(ironscheme))", ex.Condition);
      }
    }

    private static void HelloWorld()
    {
      var res = "{0}".Eval("hello world");
      Console.WriteLine(res);
    }

    static void Issue136()
    {
      var env = "(environment '(rnrs))".Eval();
      var ienv = "(interaction-environment)".Eval();
      var newenv = "(new-interaction-environment)".Eval();

      var r1 = "(+ 1 {0})".Eval(5);
      var r2 = "(+ 1 {0})".EvalWithEnvironment("(environment '(rnrs))", 5);
      var r3 = "(+ 1 {0})".EvalWithEnvironment("(interaction-environment)", 5);

      var r4 = "(+ 1 {0})".EvalWithEnvironmentInstance(env, 5);
      var r5 = "(+ 1 {0})".EvalWithEnvironmentInstance(ienv, 5);
      var r6 = "(+ 1 {0})".EvalWithEnvironmentInstance(newenv, 5);

      Console.WriteLine("", r1, r2, r3, r4, r5, r6);
    }
  }
}
