using System;

namespace Oyster.Math
{
	/// <summary>
	/// <see cref="IntX" /> global settings.
	/// </summary>
	sealed public class IntXGlobalSettings
	{
		#region Private fields

		volatile MultiplyMode _multiplyMode = MultiplyMode.AutoFht;
		volatile ParseMode _parseMode = ParseMode.Fast;
		volatile ToStringMode _toStringMode = ToStringMode.Fast;
		volatile bool _autoNormalize = false;

		#endregion Private fields

		// Class can be created only inside this assembly
		internal IntXGlobalSettings() {}

		#region Public properties

		/// <summary>
		/// Multiply operation mode used in all <see cref="IntX" /> instances.
		/// Set to auto-FHT by default.
		/// </summary>
		public MultiplyMode MultiplyMode
		{
			get { return _multiplyMode; }
			set
			{
				// Check value
				if (!Enum.IsDefined(typeof(MultiplyMode), value))
				{
					throw new ArgumentOutOfRangeException("value");
				}

				_multiplyMode = value;
			}
		}

		/// <summary>
		/// Parse mode used in all <see cref="IntX" /> instances.
		/// Set to Fast by default.
		/// </summary>
		public ParseMode ParseMode
		{
			get { return _parseMode; }
			set
			{
				// Check value
				if (!Enum.IsDefined(typeof(ParseMode), value))
				{
					throw new ArgumentOutOfRangeException("value");
				}

				_parseMode = value;
			}
		}

		/// <summary>
		/// To string conversion mode used in all <see cref="IntX" /> instances.
		/// Set to Fast by default.
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
		/// Set to false by default.
		/// </summary>
		public bool AutoNormalize
		{
			get { return _autoNormalize; }
			set { _autoNormalize = value; }
		}

		#endregion Public properties
	}
}
