/* ****************************************************************************
 *
 * Copyright (c) Microsoft Corporation. 
 *
 * This source code is subject to terms and conditions of the Microsoft Public License. A 
 * copy of the license can be found in the License.html file at the root of this distribution. If 
 * you cannot locate the  Microsoft Public License, please send an email to 
 * dlr@microsoft.com. By using this source code in any fashion, you are agreeing to be bound 
 * by the terms of the Microsoft Public License.
 *
 * You must not remove this notice, or any other, from this software.
 *
 *
 * ***************************************************************************/

using System;
using System.Collections.Generic;
using System.Text;
using System.Diagnostics;

using System.Threading;

namespace Microsoft.Scripting.Utils {
    /// <summary>
    /// Thread safe dictionary that allows lazy-creation where readers will block for
    /// the creation of the lazily created value.  Call GetOrCreateValue w/ a key
    /// and a callback function.  If the value exists it is returned, if not the create
    /// callback is called (w/o any locks held).  The create call back will only be called
    /// once for each key.  
    /// </summary>
    public class Publisher<TKey, TValue> {
        Dictionary<TKey, PublishInfo<TValue>> data = new Dictionary<TKey, PublishInfo<TValue>>();

        public delegate TValue CreateValue();

        // TODO: fix
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2201:DoNotRaiseReservedExceptionTypes")]
        public TValue GetOrCreateValue(TKey key, CreateValue create) {
            lock (data) {
                PublishInfo<TValue> pubValue;
                if (data.TryGetValue(key, out pubValue)) {
                    if (pubValue.Value == null && pubValue.Exception == null) {
                        pubValue.PrepareForWait();
                        Monitor.Exit(data);

                        try {
                            pubValue.WaitForPublish();
                        } finally {
                            Monitor.Enter(data);
                            pubValue.FinishWait();
                        }
                    }

                    if (pubValue.Exception != null) throw new Exception("Error", pubValue.Exception);

                    return pubValue.Value;
                }

                TValue ret;
                // publish the empty PublishInfo
                data[key] = pubValue = new PublishInfo<TValue>();
                // release our lock while we create the new value
                // then re-acquire the lock and publish the info.
                Monitor.Exit(data);
                try {
                    try {
                        ret = create();
                        Debug.Assert(ret != null, "Can't publish a null value");
                    } finally {
                        Monitor.Enter(data);
                    }
                } catch (Exception e) {
                    pubValue.PublishError(e);
                    throw;
                }

                pubValue.PublishValue(ret);
                return ret;
            }
        }

        /// <summary>
        /// Helper class which stores the published value
        /// </summary>
        class PublishInfo<T> {
            public PublishInfo() {
            }

            // TODO: seems to be FxCop bug
            [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1823:AvoidUnusedPrivateFields")]
            public T Value;

            // TODO: seems to be FxCop bug
            [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1823:AvoidUnusedPrivateFields")]
            public Exception Exception;

            // TODO: seems to be FxCop bug
            [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1823:AvoidUnusedPrivateFields")]
            private ManualResetEvent _waitEvent;

            // TODO: seems to be FxCop bug
            [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1823:AvoidUnusedPrivateFields")]
            private int _waiters;

            public void PublishValue(T value) {
                Value = value;
                if (_waitEvent != null) _waitEvent.Set();
            }

            public void PublishError(Exception e) {
                Exception = e;
            }

            public void PrepareForWait() {
                if (_waitEvent == null) {
                    ManualResetEvent mre = new ManualResetEvent(false);
                    if (Interlocked.CompareExchange<ManualResetEvent>(ref _waitEvent, mre, null) != null) {
                        mre.Close();
                    }
                }
                _waiters++;
            }

            public void WaitForPublish() {
                _waitEvent.WaitOne();
            }

            public void FinishWait() {
                _waiters--;
                if (_waiters == 0) _waitEvent.Close();
            }
        }
    }


}
