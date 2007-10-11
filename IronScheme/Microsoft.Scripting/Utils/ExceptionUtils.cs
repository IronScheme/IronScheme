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
using System.Collections;
using System.Threading;

namespace Microsoft.Scripting.Utils {
    public static class ExceptionUtils {
        public static ArgumentOutOfRangeException MakeArgumentOutOfRangeException(string paramName, object actualValue, string message) {
#if SILVERLIGHT // ArgumentOutOfRangeException ctor overload
            throw new ArgumentOutOfRangeException(paramName, string.Format("{0} (actual value is '{1}')", message, actualValue));
#else
            throw new ArgumentOutOfRangeException(paramName, actualValue, message);
#endif
        }

#if SILVERLIGHT
        private static WeakHash<Exception, IDictionary> _exceptionData;
#endif
        public static IDictionary GetDataDictionary(Exception e) {
            Contract.RequiresNotNull(e, "e");

#if SILVERLIGHT
            if (_exceptionData == null) {
                Interlocked.CompareExchange(ref _exceptionData, new WeakHash<Exception, IDictionary>(), null);
            }

            lock (_exceptionData) {
                IDictionary res;
                if (_exceptionData.TryGetValue(e, out res)) return res;

                res = new Dictionary<object, object>();
                _exceptionData[e] = res;
                return res;
            }
#else
            return e.Data;
#endif
        }

        public static bool TryGetData(Exception e, object key, out object value) {
            IDictionary dict = GetDataDictionary(e);
            if (dict.Contains(key)) {
                value = dict[key];
                return true;
            } else {
                value = null;
                return false;
            }
        }

        public static ArgumentNullException MakeArgumentItemNullException(int index, string arrayName) {
            return new ArgumentNullException(String.Format("{0}[{1}]", arrayName, index));
        }
    }
}
