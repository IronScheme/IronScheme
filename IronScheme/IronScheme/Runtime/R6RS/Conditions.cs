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
  public class Conditions : Builtins
  {
    class CompoundCondition : Exception
    {
      internal object[] conds;
      public CompoundCondition(object[] conds)
      {
        this.conds = conds;
      }
    }

    [Builtin("conditions")]
    public static object MakeConditions(params object[] conds)
    {
      if (conds.Length == 1)
      {
        return conds[0];
      }
      return new CompoundCondition(conds);
    }

    [Builtin("simple-condition")]
    public static object SimpleCondition(object cond)
    {
      if (cond is CompoundCondition)
      {
        return VectorToList(((CompoundCondition)cond).conds);
      }
      return List(cond);
    }

    [Builtin("condition?")]
    public static object IsCondition(object cond)
    {
      return cond is Exception;
    }

    //(condition-predicate rtd)
    [Builtin("condition-predicate")]
    public static object ConditionPredicate(object rtd)
    {
      return false;
    }

    //(condition-accessor rtd proc)
    [Builtin("condition-accessor")]
    public static object ConditionAccessor(object rtd, object proc)
    {
      return false;
    }

  }
}
#endif
