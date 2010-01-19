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
		volatile DivideMode _divideMode = DivideMode.AutoNewton;
		volatile ParseMode _parseMode = ParseMode.Fast;
		volatile ToStringMode _toStringMode = ToStringMode.Fast;
		volatile bool _autoNormalize = false;
		volatile bool _applyFhtValidityCheck = true;

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
		/// Divide operation mode used in all <see cref="IntX" /> instances.
		/// Set to auto-Newton by default.
		/// </summary>
		public DivideMode DivideMode
		{
			get { return _divideMode; }
			set
			{
				// Check value
				if (!Enum.IsDefined(typeof(DivideMode), value))
				{
					throw new ArgumentOutOfRangeException("value");
				}

				_divideMode = value;
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

		/// <summary>
		/// If true then FHT multiplication result is always checked for validity
		/// by multiplying integers lower digits using classic algorithm and comparing with FHT result.
		/// Set to true by default.
		/// </summary>
		public bool ApplyFhtValidityCheck
		{
			get { return _applyFhtValidityCheck; }
			set { _applyFhtValidityCheck = value; }
		}

		#endregion Public properties
	}
}
