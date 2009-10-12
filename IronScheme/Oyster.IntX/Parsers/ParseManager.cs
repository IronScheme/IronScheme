using System;

namespace Oyster.Math
{
	/// <summary>
	/// Used to retrieve needed parser.
	/// </summary>
	static internal class ParseManager
	{
		#region Fields

		/// <summary>
		/// Classic parser instance.
		/// </summary>
		static readonly public IParser ClassicParser;

		/// <summary>
		/// Fast parser instance.
		/// </summary>
		static readonly public IParser FastParser;

		#endregion Fields

		#region Constructors

		// .cctor
		static ParseManager()
		{
			// Create new pow2 parser instance
			IParser pow2Parser = new Pow2Parser();

			// Create new classic parser instance
			IParser classicParser = new ClassicParser(pow2Parser);

			// Fill publicity visible parser fields
			ClassicParser = classicParser;
			FastParser = new FastParser(pow2Parser, classicParser);
		}

		#endregion Constructors

		#region Methods

		/// <summary>
		/// Returns parser instance for given parse mode.
		/// </summary>
		/// <param name="mode">Parse mode.</param>
		/// <returns>Parser instance.</returns>
		/// <exception cref="ArgumentOutOfRangeException"><paramref name="mode" /> is out of range.</exception>
		static public IParser GetParser(ParseMode mode)
		{
			// Check value
			if (!Enum.IsDefined(typeof(ParseMode), mode))
			{
				throw new ArgumentOutOfRangeException("mode");
			}

			switch (mode)
			{
				case ParseMode.Fast:
					return FastParser;
				default:
					return ClassicParser;
			}
		}

		/// <summary>
		/// Returns current parser instance.
		/// </summary>
		/// <returns>Current parser instance.</returns>
		static public IParser GetCurrentParser()
		{
			return GetParser(IntX.GlobalSettings.ParseMode);
		}

		#endregion Methods
	}
}
