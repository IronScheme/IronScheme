#region License
/* Copyright (c) 2007-2014 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
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

