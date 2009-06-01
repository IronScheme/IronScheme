using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting;
using System.IO;

namespace IronScheme.Runtime.psyntax
{
  [Serializable]
  internal class Annotation
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
