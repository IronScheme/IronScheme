/* ****************************************************************************
 *
 * Copyright (c) Microsoft Corporation. 
 *
 * This source code is subject to terms and conditions of the Microsoft Permissive License. A 
 * copy of the license can be found in the License.html file at the root of this distribution. If 
 * you cannot locate the  Microsoft Permissive License, please send an email to 
 * dlr@microsoft.com. By using this source code in any fashion, you are agreeing to be bound 
 * by the terms of the Microsoft Permissive License.
 *
 * You must not remove this notice, or any other, from this software.
 *
 *
 * ***************************************************************************/

using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.Serialization;

namespace Microsoft.Scripting.Shell {
    [Serializable]
    public class KeyboardInterruptException : Exception {
        public KeyboardInterruptException() : base() { }
        public KeyboardInterruptException(string msg) : base(msg) { }
        public KeyboardInterruptException(string message, Exception innerException)
            : base(message, innerException) {
        }
#if !SILVERLIGHT // SerializationInfo
        protected KeyboardInterruptException(SerializationInfo info, StreamingContext context) : base(info, context) { }
#endif
    }
}
