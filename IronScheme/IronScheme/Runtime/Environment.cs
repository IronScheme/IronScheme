#region License
/* Copyright (c) 2007-2014 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.IO;
using System.Text;
using Microsoft.Scripting;
using System.Threading;

namespace IronScheme.Runtime
{
  public partial class Builtins
  {
    [Builtin]
    public static object UnGenSym(object symbol)
    {
      var sym = RequiresNotNull<SymbolId>(symbol);

      string ss = SymbolTable.IdToString(sym);
      //name is between 1st and 2nd $
      int start = ss.IndexOf('$') + 1;
      if (start > 0)
      {
        int count = ss.IndexOf('$', start) - start;

        if (count > 0)
        {
          ss = ss.Substring(start, count);
          return SymbolTable.StringToObject(ss);
        }
      }
      return symbol;
    }

    internal static SymbolId UnGenSymInternal(SymbolId sym)
    {
      string ss = SymbolTable.IdToString(sym);
      //name is between 1st and 2nd $
      int start = ss.IndexOf('$') + 1;
      if (start > 0)
      {
        int count = ss.IndexOf('$', start) - start;

        if (count > 0)
        {
          ss = ss.Substring(start, count);
          return SymbolTable.StringToId(ss);
        }
      }
      return sym;
    }

    internal static object CleanWho(object who)
    {
      string name = who.ToString();
      int i = name.LastIndexOf('$');
      if (i < 0)
      {
        return who;
      }
      return SymbolTable.StringToObject(name.Substring(0, i));
    }

    static bool r6rsloaded = false;

    protected static bool IsR6RSLoaded()
    {
      return r6rsloaded || (r6rsloaded = Context.Scope.ModuleScope.ContainsName(SymbolTable.StringToId("r6rs-loaded")));
    }

    static readonly int TICKS = (int)((DateTime.Now.Ticks >> 16) & 0x7FFFFFFF);

    static int anonsymcount = 0;
    static int symcount = 0;

    static readonly char[] symchars = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ@=".ToCharArray(); //64 for ease

    static string PackInt(int i)
    {
      StringBuilder sb = new StringBuilder();
      do
      {
        int r = i % 64;
        sb.Append(symchars[r]);

        i /= 64;
      }
      while (i > 0) ;

      // need to reverse, not really needed, but lets be correct for now
      char[] res = new char[sb.Length];

      for (int j = 0; j < res.Length; j++)
      {
        res[res.Length - j - 1] = sb[j];
      }

      return new string(res);
    }

    static readonly string TICKSTRING = PackInt(TICKS);

    internal static string GenGenSymString()
    {
      int c = Interlocked.Increment(ref anonsymcount);
      return "g$" + c + "$" + TICKSTRING;
    }

    [Builtin]
    public static object GenSym()
    {
      return SymbolTable.StringToObject(GenGenSymString());
    }

    [Builtin]
    public static object GenSym(object name)
    {
      int c = Interlocked.Increment(ref symcount);
      if (name is string)
      {
        string s = RequiresNotNull<string>(name);
        return SymbolTable.StringToObject("g$" + s + "$" + c + "$" + TICKSTRING);
      }
      else
      {
        SymbolId s = UnGenSymInternal(RequiresNotNull<SymbolId>(name));
        return SymbolTable.StringToObject("g$" + s + "$" + c + "$" + TICKSTRING);
      }
    }
  }
}
