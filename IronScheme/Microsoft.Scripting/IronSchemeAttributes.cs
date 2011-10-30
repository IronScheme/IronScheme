using System;

namespace IronScheme.Runtime
{
  [AttributeUsage(AttributeTargets.Method)]
  public sealed class UnspecifiedReturnAttribute : Attribute
  {
  }

  [AttributeUsage(AttributeTargets.Method)]
  public sealed class RecursiveAttribute : Attribute
  {
  }

}
