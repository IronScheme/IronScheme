using System;

namespace Oyster.Math
{
	/// <summary>
	/// Used to retrieve needed ToString converter.
	/// </summary>
	static internal class StringConvertManager
	{
		#region Fields

		/// <summary>
		/// Classic converter instance.
		/// </summary>
		static readonly public IStringConverter ClassicStringConverter;

		/// <summary>
		/// Fast converter instance.
		/// </summary>
		static readonly public IStringConverter FastStringConverter;

		#endregion Fields

		#region Constructors

		// .cctor
		static StringConvertManager()
		{
			// Create new pow2 converter instance
			IStringConverter pow2StringConverter = new Pow2StringConverter();

			// Create new classic converter instance
			IStringConverter classicStringConverter = new ClassicStringConverter(pow2StringConverter);

			// Fill publicity visible converter fields
			ClassicStringConverter = classicStringConverter;
			FastStringConverter = new FastStringConverter(pow2StringConverter, classicStringConverter);
		}

		#endregion Constructors

		#region Methods

		/// <summary>
		/// Returns ToString converter instance for given ToString mode.
		/// </summary>
		/// <param name="mode">ToString mode.</param>
		/// <returns>Converter instance.</returns>
		/// <exception cref="ArgumentOutOfRangeException"><paramref name="mode" /> is out of range.</exception>
		static public IStringConverter GetStringConverter(ToStringMode mode)
		{
			// Check value
			if (!Enum.IsDefined(typeof(ToStringMode), mode))
			{
				throw new ArgumentOutOfRangeException("mode");
			}

			switch (mode)
			{
				case ToStringMode.Fast:
					return FastStringConverter;
				default:
					return ClassicStringConverter;
			}
		}

		#endregion Methods
	}
}
