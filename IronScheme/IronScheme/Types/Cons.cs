using System;
using System.Collections.Generic;
using System.Text;

namespace IronScheme.Types
{
  public interface Cons
  {
    object Car { get; set; }
    object Cdr { get; set; }
  }

  public class MutableCons : Cons
  {
  }

  public class ImmutableCons : Cons
  {
  }

}
