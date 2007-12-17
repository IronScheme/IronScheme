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

#if R6RS
using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting.Generation;
using Microsoft.Scripting;
using System.Reflection;
using Microsoft.Scripting.Utils;
using System.Reflection.Emit;
using System.Collections;
using System.ComponentModel;

namespace IronScheme.Runtime.R6RS
{
  public class Enums : Builtins
  {
    static int enumcounter = 1;

    readonly static Dictionary<Type, Dictionary<string, int>> enummap = new Dictionary<Type, Dictionary<string, int>>();
    readonly static Dictionary<Type, List<string>> enumordermap = new Dictionary<Type, List<string>>();

    [Builtin("make-enumeration")]
    public static object MakeEnumeration(object symbols)
    {
      Cons symlist = Requires<Runtime.Cons>(symbols);

      List<string> names = new List<string>();

      while (symlist != null)
      {
        names.Add(SymbolToString(symlist.car) as string);
        symlist = symlist.cdr as Cons;
      }

      AssemblyGen ag = new AssemblyGen("enum" + enumcounter, ".", "enum" + enumcounter + ".dll", AssemblyGenAttributes.None);
      TypeGen tg = ag.DefinePublicType("enum" + enumcounter++, typeof(Enum), TypeAttributes.Public | TypeAttributes.Sealed);
      tg.TypeBuilder.SetCustomAttribute(new CustomAttributeBuilder(
        typeof(FlagsAttribute).GetConstructor(Type.EmptyTypes), new object[0]));
      tg.TypeBuilder.DefineField("value__", typeof(int), FieldAttributes.Public | FieldAttributes.RTSpecialName | FieldAttributes.SpecialName);

      Dictionary<string, int> map = new Dictionary<string, int>();
      List<string> order = new List<string>();

      int mask = 1;
      foreach (string n in names)
      {
        order.Add(n);
        map[n] = mask;
        tg.TypeBuilder.DefineField(n, tg.TypeBuilder, FieldAttributes.Static | FieldAttributes.Public | FieldAttributes.Literal).SetConstant(mask);
        mask <<= 1;
      }

      Type t = tg.FinishType();

      enummap[t] = map;
      enumordermap[t] = order;

      return Enum.ToObject(t, (1 << order.Count) - 1);
    }

    [Builtin("enum-set-universe")]
    public static object EnumSetUniverse(object enumset)
    {
      Type t = enumset.GetType();
      return Enum.ToObject(t, (1 << enumordermap[t].Count) - 1);
    }

    [Builtin("enum-set-indexer")]
    public static object EnumSetIndexer(object enumset)
    {
      Type t = enumset.GetType();
      CallTarget1 p = delegate(object symbol)
      {
        int i = enumordermap[t].IndexOf(SymbolTable.IdToString((SymbolId)symbol));
        if (i < 0)
        {
          return false;
        }
        return i;
      };
      return Closure.Make(Context, p);
    }



    [Builtin("enum-set-constructor")]
    public static object EnumSetConstructor(object enumset)
    {
      Type t = enumset.GetType();

      CallTarget1 p = delegate(object symlist)
      {
        int v = 0;
        Cons c = symlist as Cons;

        while (c != null)
        {
          v |= enummap[t][SymbolTable.IdToString((SymbolId)c.car)];
          c = c.cdr as Runtime.Cons;
        }

        return Enum.ToObject(t, v);
      };

      return Closure.Make(Context, p);
    }

    // (enum-set->list enum-set)
    [Builtin("enum-set->list")]
    public static object EnumSetToList(object enumset)
    {
      Type t = enumset.GetType();
      
      Cons head = null, h = null;
      int i = (int)enumset;

      foreach (string s in enumordermap[t])
      {
        if ((enummap[t][s] & i) != 0)
        {
          Cons n = new Cons(SymbolTable.StringToId(s));
          if (h == null)
          {
            h = head = n;
          }
          else
          {
            h = (Cons)(h.cdr = n);
          }
        }
      }

      return head;
    }

    // * (enum-set-member? symbol enum-set)
    [Builtin("enum-set-member?")]
    public static object EnumSetIsMember(object symbol, object enumset)
    {
      string s = SymbolToString(symbol) as string;
      Type t = enumset.GetType();
      int v = (int)enumset;
      return (enummap[t][s] & v) != 0;
    }

    // * (enum-set-subset? enum-set1 enum-set2)
    [Builtin("enum-set-subset?")]
    public static object EnumSetIsSubset(object enumset1, object enumset2)
    {
      Type t1 = enumset1.GetType();
      Type t2 = enumset2.GetType();

      int v1 = (int)enumset1;
      int v2 = (int)enumset2;

      if (t1 == t2)
      {
        return (v1 & v2) == v1;
      }

      foreach (string s in enumordermap[t1])
      {
        if (enummap[t2].ContainsKey(s))
        {
          // all good, now check values
          bool has1 = (enummap[t1][s] & v1) != 0;
          bool has2 = (enummap[t2][s] & v2) != 0;
          if (has1 & !has2)
          {
            return false;
          }
        }
        else
        {
          return false;
        }
      }
      return true;
    }

    // * (enum-set=? enum-set1 enum-set2)
    [Builtin("enum-set=?")]
    public static object EnumSetIsEqual(object enumset1, object enumset2)
    {
      return (bool)EnumSetIsSubset(enumset1, enumset2) && (bool)EnumSetIsSubset(enumset2, enumset1);
    }

    // * (enum-set-union enum-set1 enum-set2)
    [Builtin("enum-set-union")]
    public static object EnumSetUnion(object enumset1, object enumset2)
    {
      RequiresNotNull<Enum>(enumset1);
      RequiresNotNull<Enum>(enumset2);

      Type t1 = enumset1.GetType();
      Type t2 = enumset2.GetType();
      if (t1 != t2)
      {
        return false;
      }
      return Enum.ToObject(t1, (int)enumset1 | (int)enumset2);
    }

    // * (enum-set-intersection enum-set1 enum-set2)
    [Builtin("enum-set-intersection")]
    public static object EnumSetIntersection(object enumset1, object enumset2)
    {
      RequiresNotNull<Enum>(enumset1);
      RequiresNotNull<Enum>(enumset2);

      Type t1 = enumset1.GetType();
      Type t2 = enumset2.GetType();
      if (t1 != t2)
      {
        return false;
      }
      return Enum.ToObject(t1, (int)enumset1 & (int)enumset2);
    }

    // * (enum-set-difference enum-set1 enum-set2)
    [Builtin("enum-set-difference")]
    public static object EnumSetDifference(object enumset1, object enumset2)
    {
      RequiresNotNull<Enum>(enumset1);
      RequiresNotNull<Enum>(enumset2);

      Type t1 = enumset1.GetType();
      Type t2 = enumset2.GetType();
      if (t1 != t2)
      {
        return false;
      }
      return Enum.ToObject(t1, (int)enumset1 ^ (int)enumset2);
    }

    // * (enum-set-complement enum-set)
    [Builtin("enum-set-complement")]
    public static object EnumSetComplement(object enumset)
    {
      return EnumSetDifference(enumset, EnumSetUniverse(enumset));
    }
    
    // * (enum-set-projection enum-set1 enum-set2)
    [Builtin("enum-set-projection")]
    public static object EnumSetProjection(object enumset1, object enumset2)
    {
      Type t1 = enumset1.GetType();
      Type t2 = enumset2.GetType();

      int v = 0;

      foreach (string s in enumordermap[t1])
      {
        if (((int)enumset1 & enummap[t1][s]) != 0)
        {
          if (enummap[t2].ContainsKey(s))
          {
            v |= enummap[t2][s];
          }
        }
      }
      return Enum.ToObject(t2, v);
    }

  }
}
#endif
