using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting;
using System.IO;

namespace IronScheme.Runtime
{
  sealed class TraceClosure : Closure
  {
    readonly ICallable realtarget, filter;
    readonly SymbolId name;

    public SymbolId Name
    {
      get { return name; }
    } 

    public ICallable RealTarget
    {
      get { return realtarget; }
    }

    [ThreadStatic]
    static int depth;

    public TraceClosure(ICallable realtarget,  SymbolId name, ICallable filter)
    {
      this.realtarget = realtarget;
      this.filter = filter;
      this.name = name;
    }

    public override object Call(object[] args)
    {
      try
      {
        object ppo;

        if (!Builtins.cc.Scope.TryLookupName(SymbolTable.StringToId("pretty-print"), out ppo))
        {
          ppo = Make(Builtins.cc, new CallTarget2(Builtins.Write));
        }
        ICallable pp = ppo as ICallable;

        depth++;

        Cons c = Cons.FromArray(args), u = c;

        if (filter != null)
        {
          while (c != null)
          {
            c.car = filter.Call(c.car);
            c = c.cdr as Cons;
          }
        }

        object a = Builtins.Car(u);

        StringWriter pre = new StringWriter();

        pp.Call(a, pre);

        string prefix = new string('|', depth);
        Console.WriteLine("{0} -> {1}", prefix, pre.ToString());
        object result = realtarget.Call(args);

        StringWriter p = new StringWriter();

        pp.Call(filter == null ? result : filter.Call(result), p);

        Console.WriteLine("{0} <- {1}", prefix, p.ToString());
        return result;
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
