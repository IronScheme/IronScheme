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
  #region Support classes

  [CLSCompliant(false)]
  public sealed class ReadOnlyHashtable : Hashtable
  {
    public ReadOnlyHashtable(IDictionary content, IEqualityComparer c)
      : base(c)
    {
      foreach (DictionaryEntry de in content)
      {
        base.Add(de.Key, de.Value);
      }
    }

    public ReadOnlyHashtable(IDictionary content)
    {
      foreach (DictionaryEntry de in content)
      {
        base.Add(de.Key, de.Value);
      }
    }

    public override void Clear()
    {
      Builtins.AssertionViolation("hashtable-clear!", "hashtable is readonly", this);
    }

    public override void Add(object key, object value)
    {
      Builtins.AssertionViolation("hashtable-add!", "hashtable is readonly", this);
    }

    public override void Remove(object key)
    {
      Builtins.AssertionViolation("hashtable-remove!", "hashtable is readonly", this);
    }

    public override object this[object key]
    {
      get
      {
        return base[key];
      }
      set
      {
        Builtins.AssertionViolation("hashtable-set!", "hashtable is readonly", this);
      }
    }
  }

  [CLSCompliant(false)]
  public sealed class HashComparer : IEqualityComparer
  {
    readonly internal Callable hash, equiv;

    public HashComparer(Callable hash, Callable equiv)
    {
      this.hash = hash;
      this.equiv = equiv;
    }

    bool IEqualityComparer.Equals(object x, object y)
    {
      return Builtins.IsTrue(equiv.Call(x, y));
    }

    int IEqualityComparer.GetHashCode(object obj)
    {
      return (int)hash.Call(obj);
    }
  }

  [CLSCompliant(false)]
  public sealed class HashtableEx : Hashtable
  {
    public HashtableEx(IEqualityComparer c)
      : base(c)
    {
    }

    public HashtableEx(int k, IEqualityComparer c)
      : base(k, c)
    {
    }

    public Callable HashFunction
    {
      get
      {
        return (EqualityComparer as HashComparer).hash;
      }
    }

    public Callable EqualityFunction
    {
      get
      {
        return (EqualityComparer as HashComparer).equiv;
      }
    }

    public Hashtable MakeReadOnly()
    {
      return new ReadOnlyHashtable(this, EqualityComparer);
    }
  }

  #endregion

  public class Hashtables : Builtins
  {
    //[Obsolete]
    //[Builtin("make-hashtable")]
    //public static object MakeHashtable(object hashfun, object equivfun)
    //{
    //  HashComparer hc = new HashComparer(hashfun as Callable, equivfun as Callable);
    //  Hashtable ht = new HashtableEx(hc);
    //  return ht;
    //}

    //[Obsolete]
    //[Builtin("make-hashtable")]
    //public static object MakeHashtable(object hashfun, object equivfun, object k)
    //{
    //  if (k == null)
    //  {
    //    return MakeHashtable(hashfun, equivfun);
    //  }
    //  else
    //  {
    //    HashComparer hc = new HashComparer(hashfun as Callable, equivfun as Callable);
    //    Hashtable ht = new HashtableEx((int) k, hc);
    //    return ht;
    //  }
    //}

    //[Obsolete]
    //[Builtin("hashtable-copy")]
    //public static object HashtableCopy(object obj)
    //{
    //  return HashtableCopy(obj, FALSE);
    //}

    //[Obsolete]
    //[Builtin("hashtable-copy")]
    //public static object HashtableCopy(object obj, object mutable)
    //{
    //  Hashtable ht = RequiresNotNull<Hashtable>(obj);
    //  bool m = RequiresNotNull<bool>(mutable);
    //  if (m)
    //  {
    //    return ht.Clone();
    //  }
    //  else
    //  {
    //    if (ht is HashtableEx)
    //    {
    //      return ((HashtableEx)ht).MakeReadOnly();
    //    }
    //    else
    //    {
    //      return new ReadOnlyHashtable(ht);
    //    }
    //  }
    //}

    //[Obsolete]
    //[Builtin("hashtable-keys")]
    //public static object HashtableKeys(object obj)
    //{
    //  Hashtable ht = RequiresNotNull<Hashtable>(obj);
    //  ArrayList keys = new ArrayList(ht.Keys);
    //  return keys.ToArray();
    //}

    [Builtin("hashtable-entries")]
    public static object HashtableEntries(object obj)
    {
      Hashtable ht = RequiresNotNull<Hashtable>(obj);
      ArrayList keys = new ArrayList();
      ArrayList values = new ArrayList();

      foreach (DictionaryEntry de in ht)
      {
        keys.Add(de.Key);
        values.Add(de.Value);
      }
      return Values((object)keys.ToArray(), (object)values.ToArray());
    }

    //[Obsolete]
    //[Builtin("hashtable-mutable?")]
    //public static object IsHashtableMutable(object obj)
    //{
    //  RequiresNotNull<Hashtable>(obj);
    //  return GetBool(!(obj is ReadOnlyHashtable));
    //}

    //[Obsolete]
    //[Builtin("eqv-hash")]
    //public static object EqvHash(object obj)
    //{
    //  return obj.GetHashCode();
    //}

    //[Obsolete]
    //[Builtin("hashtable-equivalence-function")]
    //public static object HashtableEquivalenceFunction(object obj)
    //{
    //  Hashtable ht = RequiresNotNull<Hashtable>(obj);
    //  HashtableEx htx = ht as HashtableEx;
    //  if (htx != null)
    //  {
    //    return htx.EqualityFunction;
    //  }
    //  else
    //  {
    //    return SymbolValue(SymbolTable.StringToObject("eq?"));
    //  }
    //}

    //[Obsolete]
    //[Builtin("hashtable-hash-function")]
    //public static object HashtableHashFunction(object obj)
    //{
    //  Hashtable ht = RequiresNotNull<Hashtable>(obj);
    //  HashtableEx htx = ht as HashtableEx;
    //  if (htx != null)
    //  {
    //    return htx.HashFunction;
    //  }
    //  else
    //  {
    //    return FALSE;
    //  }
    //}

    [Builtin("hashtable-map")]
    public static object HashtableMap(object ht, object proc)
    {
      Hashtable h = RequiresNotNull<Hashtable>(ht);
      Callable c = RequiresNotNull<Callable>(proc);

      object[] result = new object[h.Count];

      int i = 0;

      foreach (DictionaryEntry de in h)
      {
        result[i++] = c.Call(de.Key, de.Value);
      }

      return Runtime.Cons.FromArray(result);
    }

    [Builtin("hashtable-for-each")]
    public static object HashtableForEach(object ht, object proc)
    {
      Hashtable h = RequiresNotNull<Hashtable>(ht);
      Callable c = RequiresNotNull<Callable>(proc);

      foreach (DictionaryEntry de in h)
      {
        c.Call(de.Key, de.Value);
      }

      return Unspecified;
    }

  }
}

