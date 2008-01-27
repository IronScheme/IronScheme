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
  public class Hashtables : Builtins
  {
    sealed class ReadOnlyHashtable : Hashtable
    {
      public ReadOnlyHashtable(IDictionary content)
      {
        foreach (DictionaryEntry de in content)
        {
          // verify this
          base.Add(de.Key, de.Value);
        }
      }

      public override void Clear()
      {
        AssertionViolation("hashtable-clear!", "hashtable is readonly", this);
      }

      public override void Add(object key, object value)
      {
        AssertionViolation("hashtable-add!", "hashtable is readonly", this);
      }

      public override void Remove(object key)
      {
        AssertionViolation("hashtable-remove!", "hashtable is readonly", this);
      }

      public override object this[object key]
      {
        get
        {
          return base[key];
        }
        set
        {
          AssertionViolation("hashtable-set!", "hashtable is readonly", this);
        }
      }
    }

    sealed class HashComparer : IEqualityComparer
    {
      ICallable hash, equiv;

      public HashComparer(ICallable hash, ICallable equiv)
      {
        this.hash = hash;
        this.equiv = equiv;
      }

      bool IEqualityComparer.Equals(object x, object y)
      {
        return (bool)equiv.Call(x, y);  
      }

      int IEqualityComparer.GetHashCode(object obj)
      {
        return (int)hash.Call(obj);
      }
    }

    [Builtin("make-hashtable")]
    public static object MakeHashtable(object hashfun, object equivfun)
    {
      HashComparer hc = new HashComparer(hashfun as ICallable, equivfun as ICallable);
      Hashtable ht = new Hashtable(hc);
      return ht;
    }

    [Builtin("make-hashtable")]
    public static object MakeHashtable(object hashfun, object equivfun, object k)
    {
      if (k == null)
      {
        return MakeHashtable(hashfun, equivfun);
      }
      else
      {
        HashComparer hc = new HashComparer(hashfun as ICallable, equivfun as ICallable);
        Hashtable ht = new Hashtable((int) k, hc);
        return ht;
      }
    }


    [Builtin("hashtable?")]
    public static object IsHashtable(object obj)
    {
      return obj is Hashtable;
    }

    [Builtin("hashtable-copy")]
    public static object HashtableCopy(object obj)
    {
      Hashtable ht = RequiresNotNull<Hashtable>(obj);
      return new Hashtable(ht);
    }

    [Builtin("hashtable-copy")]
    public static object HashtableCopy(object obj, object mutable)
    {
      Hashtable ht = RequiresNotNull<Hashtable>(obj);
      bool m = RequiresNotNull<bool>(mutable);
      if (m)
      {
        return new Hashtable(ht);
      }
      else
      {
        return new ReadOnlyHashtable(ht);
      }
    }

    [Builtin("hashtable-keys")]
    public static object HashtableKeys(object obj)
    {
      Hashtable ht = RequiresNotNull<Hashtable>(obj);
      ArrayList keys = new ArrayList(ht.Keys);
      return keys.ToArray();
    }

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

    [Builtin("hashtable-mutable?")]
    public static object IsHashtableMutable(object obj)
    {
      RequiresNotNull<Hashtable>(obj);
      return !(obj is ReadOnlyHashtable);
    }

    [Builtin("equal-hash")]
    public static object EqualHash(object obj)
    {
      string r = WriteFormat(obj);
      return r.GetHashCode();
    }

    [Builtin("string-hash")]
    public static object StringHash(object obj)
    {
      string r = RequiresNotNull<string>(obj);
      return r.GetHashCode();
    }

    [Builtin("string-ci-hash")]
    public static object StringCaseInsensitiveHash(object obj)
    {
      string r = RequiresNotNull<string>(obj);
      return StringComparer.CurrentCultureIgnoreCase.GetHashCode(r);
    }

    [Builtin("symbol-hash")]
    public static object SymbolHash(object obj)
    {
      SymbolId s = RequiresNotNull<SymbolId>(obj);
      return s.GetHashCode();
    }
  }
}

