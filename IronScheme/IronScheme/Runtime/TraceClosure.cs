#region License
/* Copyright (c) 2007-2016 Llewellyn Pritchard
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using Microsoft.Scripting;

namespace IronScheme.Runtime
{
  public sealed class TraceClosure : Closure
  {
    readonly Callable realtarget, filter;
    readonly SymbolId name;

    public SymbolId Name
    {
      get { return name; }
    } 

    public Callable RealTarget
    {
      get { return realtarget; }
    }

    [ThreadStatic]
    static int depth;

    public TraceClosure(Callable realtarget,  SymbolId name, object filter)
    {
      this.realtarget = realtarget;
      this.filter = filter as Callable;
      this.name = name;
    }

    public override object Call(object[] args)
    {
      try
      {
        object ppo;

        if (Builtins.cc.Scope.TryLookupName(SymbolTable.StringToId("trace-printer"), out ppo))
        {
          ppo = (ppo as Callable).Call();
        }
        else
        {
          ppo = "write".Eval();
        }
        Callable pp = ppo as Callable;

        depth++;

        Cons c = Runtime.Cons.FromArray(args), u = c;

        if (filter != null)
        {
          while (c != null)
          {
            c.car = filter.Call(c.car);
            c = c.cdr as Cons;
          }
        }


        object a = args.Length == 1 ? Builtins.Car(u) : u;

        StringWriter pre = new StringWriter();

        pp.Call(a, pre);

        string prefix = new string('|', depth);

        if ((Console.LargestWindowWidth | Console.LargestWindowHeight) == 0)
        {
          Console.Error.WriteLine("{0} -> {1}", prefix, name);
          Console.Error.WriteLine(pre.GetBuffer().TrimEnd(Environment.NewLine.ToCharArray()));

          object result = realtarget.Call(args);

          StringWriter p = new StringWriter();

          pp.Call(filter == null ? result : filter.Call(result), p);

          Console.Error.WriteLine("{0} <- {1}", prefix, name);
          Console.Error.WriteLine(p.GetBuffer().TrimEnd(Environment.NewLine.ToCharArray()));
          return result;
        }
        else
        {

          Console.ForegroundColor = ConsoleColor.Yellow;
          Console.Error.WriteLine("{0} -> {1}", prefix, name);
          Console.ForegroundColor = ConsoleColor.White;
          Console.Error.WriteLine(pre.GetBuffer().TrimEnd(Environment.NewLine.ToCharArray()));
          Console.ForegroundColor = ConsoleColor.Gray;

          object result = realtarget.Call(args);

          StringWriter p = new StringWriter();

          pp.Call(filter == null ? result : filter.Call(result), p);

          Console.ForegroundColor = ConsoleColor.Cyan;
          Console.Error.WriteLine("{0} <- {1}", prefix, name);
          Console.ForegroundColor = ConsoleColor.White;
          Console.Error.WriteLine(p.GetBuffer().TrimEnd(Environment.NewLine.ToCharArray()));
          Console.ForegroundColor = ConsoleColor.Gray;
          return result;
        }
      }
      finally
      {
        depth--;
      }

    }

    public override string ToString()
    {
      return SymbolTable.IdToString(name);
    }
  }
}
