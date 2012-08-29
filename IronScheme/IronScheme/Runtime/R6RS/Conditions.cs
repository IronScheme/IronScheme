#region License
/* Copyright (c) 2007,2008,2009,2010,2011 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Collections;
using System.Collections.Generic;
using Microsoft.Scripting;

namespace IronScheme.Runtime.R6RS
{
  public abstract class Condition
  {
    public Condition()
    {

    }

    public override string ToString()
    {
      var w = new StringWriter();
      "(display {0} {1})".Eval(this, w);
      return w.GetBuffer();
    }
  }

  sealed class CompoundCondition : Condition
  {
    internal object[] conds;
    public CompoundCondition(object[] conds)
    {
      ArrayList inners = new ArrayList();
      foreach (Condition c in conds)
      {
        if (c is CompoundCondition)
        {
          inners.AddRange(((CompoundCondition)c).conds);
        }
        else
        {
          inners.Add(c);
        }
      }
      this.conds = inners.ToArray();
    }

    public override string ToString()
    {
      List<string> c = new List<string>();
      foreach (object e in conds)
      {
        string r = e.ToString();
        if (r.Trim().Length > 0)
        {
          c.Add(r);
        }
      }
      return string.Format("{0}", string.Join("", c.ToArray()));
    }
  }

  public class Conditions : Builtins
  {
    [Builtin("condition")]
    public static object Condition(params object[] conds)
    {
      if (conds.Length == 1)
      {
        return conds[0] as Condition;
      }
      return new CompoundCondition(conds);
    }

    [Builtin("simple-conditions")]
    public static object SimpleConditions(object cond)
    {
      if (cond is CompoundCondition)
      {
        return Runtime.Cons.FromArray(((CompoundCondition)cond).conds);
      }
      return List(cond);
    }

    //(condition-predicate rtd)
    [Builtin("condition-predicate")]
    public static object ConditionPredicate(object rtd)
    {
      RecordTypeDescriptor t = RequiresNotNull<RecordTypeDescriptor>(rtd);

      if (!t.type.IsSubclassOf(typeof(Condition)))
      {
        return AssertionViolation("condition-predicate", "not a valid condition", rtd);
      }

      CallTarget1 p = delegate(object cond)
      {
        CallTarget1 recp = Delegate.CreateDelegate(typeof(CallTarget1), t.predicate) as CallTarget1;

        if (cond is CompoundCondition)
        {
          CompoundCondition cc = (CompoundCondition)cond;
          foreach (object ic in cc.conds)
          {
            if (IsTrue(recp(ic)))
            {
              return TRUE;
            }
          }
          return FALSE;
        }
        else
        {
          return recp(cond);
        }
      };

      return Closure.Create(p);
    }

    //(condition-accessor rtd proc)
    [Builtin("condition-accessor")]
    public static object ConditionAccessor(object rtd, object proc)
    {
      RecordTypeDescriptor t = RequiresNotNull<RecordTypeDescriptor>(rtd);

      if (!t.type.IsSubclassOf(typeof(Condition)))
      {
        return AssertionViolation("condition-accessor", "not a valid condition", rtd);
      }

      Callable c = RequiresNotNull<Callable>(proc);

      CallTarget1 p = delegate(object cond)
      {
        if (cond is CompoundCondition)
        {
          CompoundCondition cc = (CompoundCondition)cond;
          if (cc.conds.Length == 0)
          {
            // error?
            return FALSE;
          }
          else
          {
            foreach (object e in cc.conds)
            {
              if (t.type.IsInstanceOfType(e))
              {
                return c.Call(e);
              }
            }
            return FALSE;
          }
        }
        else
        {
          if (t.type.IsInstanceOfType(cond))
          {
            return c.Call(cond);
          }
          else
          {
            return FALSE;
          }
        }
      };

      return Closure.Create(p);
    }

  }
}

