using System;
using System.Runtime.Serialization;

namespace Oyster.Math
{
	/// <summary>
	/// Used when FHT multiplication result is incorrect.
	/// </summary>
	[Serializable]
	public class FhtMultiplicationException : ApplicationException, ISerializable
	{
		/// <summary>
		/// Initializes a new instance of the <see cref="FhtMultiplicationException" /> class with a specified error message.
		/// </summary>
		/// <param name="message">A message that describes the error.</param>
		public FhtMultiplicationException(string message) : base(message) {}

		/// <summary>
		/// Initializes a new instance of the <see cref="FhtMultiplicationException" /> class with serialized data.
		/// </summary>
		/// <param name="info">The object that holds the serialized object data.</param>
		/// <param name="context">The contextual information about the source or destination.</param>
		protected FhtMultiplicationException(SerializationInfo info, StreamingContext context)
			: base(info, context) {}
	}
}
