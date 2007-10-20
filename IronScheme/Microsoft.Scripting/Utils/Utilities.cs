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

using System.Runtime.Remoting;

namespace Microsoft.Scripting.Utils {

    public static class Utilities {

        public static bool IsRemote(object obj) {
#if !SILVERLIGHT
            return RemotingServices.IsObjectOutOfAppDomain(obj);
#else
            return false;
#endif
        }

#if !SILVERLIGHT
        internal static bool MakeHandle(bool valid, object obj, out IObjectHandle handle) {
            handle = valid ? new ObjectHandle(obj) : null;
            return valid;
        }
#endif

#pragma warning disable 414 // Private field assigned but its value never used

        /// <summary>
        /// Volatile field. Written to when we need to do a memory barrier on Silverlight.
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1823:AvoidUnusedPrivateFields")]
        private static volatile int _memoryBarrier;

        public static void MemoryBarrier() {
            _memoryBarrier = 1;
        }

#pragma warning restore 414
    }
}
