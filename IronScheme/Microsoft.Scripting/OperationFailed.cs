using System;
using System.Collections.Generic;
using System.Text;

namespace Microsoft.Scripting {
    /// <summary>
    /// Singleton instance returned from an operator method when the operator method cannot provide a value.
    /// </summary>
    public class OperationFailed {
        public static OperationFailed Value = new OperationFailed();

        private OperationFailed() {
        }
    }
}
