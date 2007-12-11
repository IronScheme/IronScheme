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

namespace IronScheme.Runtime.R6RS
{
  public class Enums : Builtins
  {
    //static int enumcounter = 1;

    [Builtin("make-enumeration")]
    public static object MakeEnumeration(object symbols)
    {
      //Cons symlist = Requires<Runtime.Cons>(symbols);

      //List<string> names = new List<string>();

      //while (symlist != null)
      //{
      //  names.Add(SymbolToString(symlist.car) as string);
      //  symlist = symlist.cdr as Cons;
      //}

      //AssemblyGen ag = new AssemblyGen(enumcounter + "", ".", enumcounter + ".dll", AssemblyGenAttributes.None);
      //TypeGen tg = ag.DefinePublicType("enum" + enumcounter++, typeof(Enum), TypeAttributes.Public | TypeAttributes.Sealed);
      //tg.TypeBuilder.DefineField("value__", typeof(int), FieldAttributes.Public | FieldAttributes.RTSpecialName | FieldAttributes.SpecialName);

      //int mask = 1;
      //foreach (string n in names)
      //{
      //  tg.TypeBuilder.DefineField(n, tg.TypeBuilder, FieldAttributes.Static | FieldAttributes.Public | FieldAttributes.Literal).SetConstant(mask);
      //  mask <<= 1;
      //}

      //Type t = tg.FinishType();

      //return t;
      return false;
    }

    [Builtin("enum-set-universe")]
    public static object EnumSetUniverse(object enumset)
    {
      return false;
    }

    [Builtin("enum-set-indexer")]
    public static object EnumSetIndexer(object enumset)
    {
      return false;
    }



    [Builtin("enum-set-constructor")]
    public static object EnumSetConstructor(object enumset)
    {
      //Type t = RequiresNotNull<Type>(enumset);

      //CallTargetN p = delegate(object[] symlist)
      //{
      //  string[] names = Enum.GetNames(t);
      //  foreach (SymbolId s in symlist)
      //  {
      //    e |= Enum.Parse(t, SymbolTable.IdToString(s)) as Enum;
      //  }

      //  return  e;
      //};

      
      //return Closure.MakeVarArgX(Context, p, 1);
      return false;
    }

    // (enum-set->list enum-set)
    /* (enum-set-member? symbol enum-set)
     * (enum-set-subset? enum-set1 enum-set2)
     * (enum-set=? enum-set1 enum-set2)
     * (enum-set-union enum-set1 enum-set2)
     * (enum-set-intersection enum-set1 enum-set2)
     * (enum-set-difference enum-set1 enum-set2)
     * (enum-set-complement enum-set)
     * (enum-set-projection enum-set1 enum-set2)
     */
  }
}
#endif
