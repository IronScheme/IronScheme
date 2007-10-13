// Gardens Point Parser Generator
// Copyright (c) Wayne Kelly, QUT 2005-2006
// (see accompanying GPPGcopyright.rtf)


namespace gppg
{
    public class ParserStack<T>
    {
        public T[] array = new T[1];
		public int top = 0;


        public void Push(T value)
        {
            if (top >= array.Length)
            {
                T[] newarray = new T[array.Length * 2];
                System.Array.Copy(array, newarray, top);
                array = newarray;
            }
            array[top++] = value;
        }

        public T Pop()
        {
            // Temporary test ...
            T rslt = array[--top];
            array[top] = default(T);
            return rslt;
            // return array[--top];
        }

      [System.CLSCompliant(false)]
		public T Top()
		{
			return array[top - 1];
		}

        public bool IsEmpty()
        {
            return top == 0;
        }
	}
}