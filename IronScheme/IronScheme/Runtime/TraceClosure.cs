using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting;

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
        depth++;
        string[] sargs = Array.ConvertAll<object, string>(args, delegate(object o)
        {
          if (filter != null)
          {
            o = filter.Call(o);
          }
          return Builtins.DisplayFormat(o);
        });
        string prefix = new string('|', depth);
        Console.WriteLine("{0}({1}{2})", prefix, SymbolTable.IdToString(name), args.Length == 0 ? "" : (" " + string.Join(" ", sargs)));
        object result = realtarget.Call(args);

        Console.WriteLine("{0}{1}", prefix, Builtins.DisplayFormat(filter == null ? result : filter.Call(result)));
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
