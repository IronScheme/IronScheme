// Gardens Point Parser Generator
// Copyright (c) Wayne Kelly, QUT 2005-2006
// (see accompanying GPPGcopyright.rtf)



namespace gppg
{
  public sealed class Rule
  {
    public readonly int lhs; // symbol
    public readonly int[] rhs; // symbols

    public Rule(int lhs, int[] rhs)
    {
      this.lhs = lhs;
      this.rhs = rhs;
    }
  }
}
