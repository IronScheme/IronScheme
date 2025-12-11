#region License
/* Copyright (c) 2007-2016 Llewellyn Pritchard
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.IO;
using System.Text;
using Microsoft.Scripting;
using System.Threading;
using System.Reflection;
using System.Collections.Generic;
using System.Reflection.Emit;

namespace IronScheme.Runtime
{
  public partial class Builtins
  {
    [Builtin]
    public static object Disassemble(object proc)
    {
      return Disassemble(proc, FALSE);
    }

    [Builtin]
    public static object Disassemble(object proc, object argcount)
    {
      var error = "(current-error-port)".Eval<TextWriter>();
      return Disassemble(proc, argcount, error);
    }

    [Builtin]
    public static object Disassemble(object proc, object argcount, TextWriter writer)
    {
      if (proc is Closure)
      {
        var c = RequiresNotNull<Closure>(proc);
        var at = c.AllTargets;
        var tc = at.Length;
        // implies case closure
        if (tc > 1)
        {
          // check for valid arg count
          if (argcount != FALSE)
          {
            int ac = Requires<int>(argcount);
            // now figure out what can be used...

            foreach (var m in at)
            {
              var p = m.GetParameters();
              var pc = p.Length;
              if (p.Length > 0 && p[0].ParameterType == typeof(CodeContext))
              {
                pc -= 1;
              }
              if (pc == ac)
              {
                return DisassembleMethod(m, writer);
              }
            }

            return AssertionViolation("disassemble", "procedure ambiguation failed", proc, argcount);
          }
          else
          {
            return AssertionViolation("disassemble", "procedure ambiguation requires an argument count parameter", proc);
          }
        }
        else if (tc == 0)
        {
          return AssertionViolation("disassemble", "not possible on procedure", proc);
        }
        else // if (tc == 1) // only thing left
        {
          return DisassembleMethod(at[0], writer);
        }
      }
      else
      {
        return AssertionViolation("disassemble", "builtin procedures not supported, consult the source code", proc);
      }
    }

    static object DisassembleMethod(MethodInfo meth, TextWriter writer)
    {
      var name = meth.Name;
      var closures = new List<MethodInfo>();

      writer.WriteLine(meth);

      var locals = meth.GetMethodBody().LocalVariables;

      if (locals.Count > 0)
      {
        writer.WriteLine(".locals init (");

        foreach (var l in locals)
        {
          writer.WriteLine("  {0}", l);
        }

        writer.WriteLine(")");
      }

      foreach (var inst in Reflection.Disassembler.GetInstructions(meth))
      {
        writer.WriteLine(inst);

        if (inst.OpCode == OpCodes.Ldftn && inst.Operand is MethodInfo)
        {
          var mi = (MethodInfo)inst.Operand;
          if (mi.Name.StartsWith(name) && mi.Name != name)
          {
            closures.Add(mi);
          }
        }
      }

      foreach (var c in closures)
      {
        writer.WriteLine();
        writer.WriteLine("// Closure:");
        DisassembleMethod(c, writer);
      }

      return Unspecified;
    }

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
      return r6rsloaded || (r6rsloaded = Context.Scope.ModuleScope.ContainsName(SymbolTable.StringToIdFast("r6rs-loaded")));
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
      return "g$gen" + c + "$" + TICKSTRING;
    }

    [Builtin]
    public static object GenSym()
    {
      return SymbolTable.StringToObjectFast(GenGenSymString());
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
