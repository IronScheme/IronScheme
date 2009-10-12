using System;

namespace Oyster.Math
{
	/// <summary>
	/// Used to retrieve needed multiplier.
	/// </summary>
	static internal class MultiplyManager
	{
		#region Fields

		/// <summary>
		/// Classic multiplier instance.
		/// </summary>
		static readonly public IMultiplier ClassicMultiplier;

		/// <summary>
		/// FHT multiplier instance.
		/// </summary>
		static readonly public IMultiplier AutoFhtMultiplier;

		#endregion Fields

		#region Constructors

		// .cctor
		static MultiplyManager()
		{
			// Create new classic multiplier instance
			IMultiplier classicMultiplier = new ClassicMultiplier();

			// Fill publicity visible multiplier fields
			ClassicMultiplier = classicMultiplier;
			AutoFhtMultiplier = new AutoFhtMultiplier(classicMultiplier);
		}

		#endregion Constructors

		#region Methods

		/// <summary>
		/// Returns multiplier instance for given multiply mode.
		/// </summary>
		/// <param name="mode">Multiply mode.</param>
		/// <returns>Multiplier instance.</returns>
		/// <exception cref="ArgumentOutOfRangeException"><paramref name="mode" /> is out of range.</exception>
		static public IMultiplier GetMultiplier(MultiplyMode mode)
		{
			// Check value
			if (!Enum.IsDefined(typeof(MultiplyMode), mode))
			{
				throw new ArgumentOutOfRangeException("mode");
			}

			switch (mode)
			{
				case MultiplyMode.AutoFht:
					return AutoFhtMultiplier;
				default:
					return ClassicMultiplier;
			}
		}

		/// <summary>
		/// Returns current multiplier instance.
		/// </summary>
		/// <returns>Current multiplier instance.</returns>
		static public IMultiplier GetCurrentMultiplier()
		{
			return GetMultiplier(IntX.GlobalSettings.MultiplyMode);
		}

		#endregion Methods
	}
}
