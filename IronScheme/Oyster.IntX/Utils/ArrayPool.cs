using System;
using System.Collections.Generic;

namespace Oyster.Math
{
	/// <summary>
	/// Generic arrays pool on weak references.
	/// </summary>
	sealed internal class ArrayPool<T>
	{
		#region Fields

		/// <summary>
		/// Singleton instance.
		/// </summary>
		static readonly public ArrayPool<T> Instance = new ArrayPool<T>(
			Constants.MinPooledArraySizeLog2,
			Constants.MaxPooledArraySizeLog2,
			Constants.MaxArrayPoolCount);

		// Minimal and maximal Log2(uint[] length) which will be pooled
		int _minPooledArraySizeLog2;
		int _maxPooledArraySizeLog2;

		// Maximal pool items count
		int _maxPoolCount;

		Stack<WeakReference>[] _pools; // ArrayPool pools

		#endregion Fields

		#region Constructor

		// Singleton
		private ArrayPool(int minPooledArraySizeLog2, int maxPooledArraySizeLog2, int maxPoolCount)
		{
			_minPooledArraySizeLog2 = minPooledArraySizeLog2;
			_maxPooledArraySizeLog2 = maxPooledArraySizeLog2;
			_maxPoolCount = maxPoolCount;

			// Init pools
			_pools = new Stack<WeakReference>[maxPooledArraySizeLog2 - minPooledArraySizeLog2 + 1];
			for (int i = 0; i < _pools.Length; ++i)
			{
				// Calls to those pools are sync in code
				_pools[i] = new Stack<WeakReference>();
			}
		}

		#endregion Constructor

		#region Pool management methods

		/// <summary>
		/// Either returns array of given size from pool or creates it.
		/// </summary>
		/// <param name="length">Array length (always pow of 2).</param>
		/// <returns>Always array instance ready to use.</returns>
		public T[] GetArray(uint length)
		{
			int lengthLog2 = Bits.Msb(length);

			// Check if we can search in pool
			if (lengthLog2 >= _minPooledArraySizeLog2 && lengthLog2 <= _maxPooledArraySizeLog2)
			{
				// Get needed pool
				Stack<WeakReference> pool = _pools[lengthLog2 - _minPooledArraySizeLog2];

				// Try to find at least one not collected array of given size
				while (pool.Count > 0)
				{
					WeakReference arrayRef;
					lock (pool)
					{
						// Double-guard here
						if (pool.Count > 0)
						{
							arrayRef = pool.Pop();
						}
						else
						{
							// Well, we can exit here
							break;
						}
					}

					// Maybe return found array if link is alive
					T[] array = (T[])arrayRef.Target;
					if (arrayRef.IsAlive) return array;
				}
			}

			// Array can't be found in pool - create new one
			return new T[length];
		}

		/// <summary>
		/// Adds array to pool.
		/// </summary>
		/// <param name="array">Array to add (it/s length is always pow of 2).</param>
		public void AddArray(T[] array)
		{
			int lengthLog2 = Bits.Msb((uint)array.LongLength);

			// Check if we can add in pool
			if (lengthLog2 >= _minPooledArraySizeLog2 && lengthLog2 <= _maxPooledArraySizeLog2)
			{
				// Get needed pool
				Stack<WeakReference> pool = _pools[lengthLog2 - _minPooledArraySizeLog2];

				// Add array to pool (only if pool size is not too big)
				if (pool.Count <= _maxPoolCount)
				{
					lock (pool)
					{
						// Double-guard here
						if (pool.Count <= _maxPoolCount)
						{
							pool.Push(new WeakReference(array));
						}
					}
				}
			}
		}

		#endregion Pool management methods
	}
}
