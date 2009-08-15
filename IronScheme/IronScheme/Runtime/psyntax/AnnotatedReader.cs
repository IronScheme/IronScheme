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
using Microsoft.Scripting;
using System.IO;

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
