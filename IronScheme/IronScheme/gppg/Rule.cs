// Gardens Point Parser Generator
// Copyright (c) Wayne Kelly, QUT 2005-2006
// (see accompanying GPPGcopyright.rtf)



namespace gppg
{
    public class Rule
    {
        public int lhs; // symbol
		public int[] rhs; // symbols

        public Rule(int lhs, int[] rhs)
        {
            this.lhs = lhs;
            this.rhs = rhs;
        }
    }
}
