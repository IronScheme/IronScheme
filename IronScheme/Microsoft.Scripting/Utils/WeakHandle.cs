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
using System.Runtime.InteropServices;

namespace Microsoft.Scripting.Utils {

#if SILVERLIGHT
    public struct WeakHandle {

        private class SupressableWeakReference : WeakReference {
            public SupressableWeakReference(object target, bool trackResurrection)
                : base(target, trackResurrection) {
            }
        }

        private SupressableWeakReference _weakRef;

        public WeakHandle(object target, bool trackResurrection) {
            this._weakRef = new SupressableWeakReference(target, trackResurrection);
            GC.SuppressFinalize(this._weakRef);
        }

        public bool IsAlive { get { return _weakRef != null && _weakRef.IsAlive; } }
        public object Target { get { return _weakRef != null ? _weakRef.Target : null; } }

        public void Free() {
            if (_weakRef != null) {
                GC.ReRegisterForFinalize(_weakRef);
                _weakRef.Target = null;
                _weakRef = null;
            }
        }
    }
#else
    public struct WeakHandle {

        private GCHandle weakRef;

        public WeakHandle(object target, bool trackResurrection) {
            this.weakRef = GCHandle.Alloc(target, trackResurrection ? GCHandleType.WeakTrackResurrection : GCHandleType.Weak);
        }

        public bool IsAlive { get { return weakRef.IsAllocated; } }
        public object Target { get { return weakRef.Target; } }
        public void Free() { weakRef.Free(); }
    }
#endif
}
