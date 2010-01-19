using System;

namespace Oyster.Math
{
	#region enum DivModResults

	/// <summary>
	/// <see cref="IntX" /> divide results to return.
	/// </summary>
	[Flags]
	internal enum DivModResultFlags
	{
		/// <summary>
		/// Divident is returned.
		/// </summary>
		Div = 1,
		/// <summary>
		/// Remainder is returned.
		/// </summary>
		Mod = 2
	}

	#endregion enum DivModResults

	#region enum MultiplyMode

	/// <summary>
	/// Big integers multiply mode used in <see cref="IntX" />.
	/// </summary>
	public enum MultiplyMode
	{
		/// <summary>
		/// FHT (Fast Hartley Transform) is used for really big integers.
		/// Time estimate is O(n * log n).
		/// Default mode.
		/// </summary>
		AutoFht,
		/// <summary>
		/// Classic method is used.
		/// Time estimate is O(n ^ 2).
		/// </summary>
		Classic
	}

	#endregion enum MultiplyMode

	#region enum DivideMode

	/// <summary>
	/// Big integers divide mode used in <see cref="IntX" />.
	/// </summary>
	public enum DivideMode
	{
		/// <summary>
		/// Newton approximation algorithm is used for really big integers.
		/// Time estimate is same as for multiplication.
		/// Default mode.
		/// </summary>
		AutoNewton,
		/// <summary>
		/// Classic method is used.
		/// Time estimate is O(n ^ 2).
		/// </summary>
		Classic
	}

	#endregion enum DivideMode

	#region enum ParseMode

	/// <summary>
	/// Big integers parsing mode used in <see cref="IntX" />.
	/// </summary>
	public enum ParseMode
	{
		/// <summary>
		/// Fast method which uses divide-by-two approach and fast multiply to parse numbers.
		/// Time estimate is O(n * [log n]^2).
		/// Default mode.
		/// </summary>
		Fast,
		/// <summary>
		/// Classic method is used (using multiplication).
		/// Time estimate is O(n ^ 2).
		/// </summary>
		Classic
	}

	#endregion enum ParseMode

	#region enum ToStringMode

	/// <summary>
	/// Big integers to string conversion mode used in <see cref="IntX" />.
	/// </summary>
	public enum ToStringMode
	{
		/// <summary>
		/// Fast method which uses divide-by-two approach to convert numbers.
		/// Default mode.
		/// </summary>
		Fast,
		/// <summary>
		/// Classic method is used (using division).
		/// Time estimate is O(n ^ 2).
		/// </summary>
		Classic
	}

	#endregion enum ToStringMode
}
