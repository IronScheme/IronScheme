using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting;

namespace IronScheme.Runtime
{
  public interface ISyntaxObject
  {
    SourceSpan Location { get; set;}
    object Value { get;} // return this
  }

  public class Syntax
  {
    SourceSpan location = SourceSpan.None;

    public SourceSpan Location
    {
      get { return location; }
      set { location = value; }
    }

  }

  public class Syntax<T> : Syntax, ISyntaxObject
  {
    T value;

    public T Value
    {
      get { return this.value; }
      set { this.value = value; }
    }

    object ISyntaxObject.Value
    {
      get { return value; }
    }
  }



}

