using System;

namespace Oyster.Math
{
	/// <summary>
	/// <see cref="IntX" /> instance settings.
	/// </summary>
	sealed public class IntXSettings
	{
		#region Private fields

		ToStringMode _toStringMode = ToStringMode.Fast;
		bool _autoNormalize = false;

		#endregion Private fields

		/// <summary>
		/// Creates new <see cref="IntXSettings" /> instance.
		/// </summary>
		/// <param name="globalSettings">IntX global settings to copy.</param>
		internal IntXSettings(IntXGlobalSettings globalSettings)
		{
			// Copy local settings from global ones
			_autoNormalize = globalSettings.AutoNormalize;
			_toStringMode = globalSettings.ToStringMode;
		}

		#region Public properties

		/// <summary>
		/// To string conversion mode used in this <see cref="IntX" /> instance.
		/// Set to value from <see cref="IntX.GlobalSettings" /> by default.
		/// </summary>
		public ToStringMode ToStringMode
		{
			get { return _toStringMode; }
			set
			{
				// Check value
				if (!Enum.IsDefined(typeof(ToStringMode), value))
				{
					throw new ArgumentOutOfRangeException("value");
				}

				_toStringMode = value;
			}
		}

		/// <summary>
		/// If true then each operation is ended with big integer normalization.
		/// Set to value from <see cref="IntX.GlobalSettings" /> by default.
		/// </summary>
		public bool AutoNormalize
		{
			get { return _autoNormalize; }
			set { _autoNormalize = value; }
		}

		#endregion Public properties
	}
}
