#region License
/* Copyright (c) 2007,2008,2009,2010,2011 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;

namespace IronScheme.Runtime.psyntax
{
  [Serializable]
  public class Annotation
  {
    public readonly object expression;
    public readonly object source;
    public readonly object stripped;

    public Annotation(object expression, object source, object stripped)
    {
      this.expression = expression;
      this.source = source;
      this.stripped = stripped;
    }

    public override string ToString()
    {
      if (source is Cons)
      {
        return ((Cons)source).cdr as string;
      }
      return (source ?? "").ToString(); 
    }
  }

  [Serializable]
  internal class ConsAnnotation : Annotation
  {
    public ConsAnnotation(Cons expression, object source, Cons stripped)
      : base(expression, source, stripped)
    {
    }
  }
}
