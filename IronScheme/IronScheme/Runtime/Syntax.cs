using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting;

namespace IronScheme.Runtime
{
  public class Syntax
  {
    SourceSpan location = SourceSpan.None;

    public SourceSpan Location
    {
      get { return location; }
      set { location = value; }
    }

  }

  public class Syntax<T> : Syntax
  {
    T value;

    public T Value
    {
      get { return this.value; }
      set { this.value = value; }
    }

    public static explicit operator T(Syntax<T> o)
    {
      return o.value;
    }

  }

  public class SyntaxList : Syntax<Cons>, IEnumerable<Syntax>
  {
    public IEnumerator<Syntax> GetEnumerator()
    {
      foreach (Syntax s in Value)
      {
        yield return s;
      }
    }

    System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
    {
      return GetEnumerator();
    }
  }




}

