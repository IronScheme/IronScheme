using System;
using System.Collections.Generic;
using System.Text;

namespace Microsoft.Scripting {
    /// <summary>
    /// Singleton instance returned from an operator method when the operator method cannot provide a value.
    /// </summary>
    public sealed class OperationFailed {
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly OperationFailed Value = new OperationFailed();

        private OperationFailed() {
        }
    }
}
