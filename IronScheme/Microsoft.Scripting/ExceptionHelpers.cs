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
using System.Diagnostics;
using System.Reflection;
using System.Threading;

using Microsoft.Scripting.Shell;

namespace Microsoft.Scripting {
    /// <summary>
    /// These are some generally useful helper methods for handling exceptions.
    /// </summary>
    public static class ExceptionHelpers {
        private const string prevStackTraces = "PreviousStackTraces";

        /// <summary>
        /// Keeps track of exceptions being handled in interpreted mode (so we can support rethrow statements).
        /// </summary>
        [ThreadStatic]
        internal static List<Exception> _caughtExceptions;

        [ThreadStatic]
        private static List<Exception> _currentExceptions;

        [ThreadStatic]
        private static List<DynamicStackFrame> _stackFrames;

        /// <summary>
        /// Gets the list of exceptions that are currently being handled by the user. 
        /// 
        /// These represent active catch blocks on the stack.
        /// </summary>
        public static List<Exception> CurrentExceptions {
            get {
                return _currentExceptions;
            }
        }

        public static Exception LastException {
            get {
                if (_caughtExceptions != null && _caughtExceptions.Count > 0) {
                    return _caughtExceptions[_caughtExceptions.Count - 1];
                } else {
                    throw new InvalidOperationException("No exception");
                }
            }
        }

        /// <summary>
        /// Updates an exception before it's getting re-thrown so
        /// we can present a reasonable stack trace to the user.
        /// </summary>
        public static Exception UpdateForRethrow(Exception rethrow) {
#if !SILVERLIGHT
            List<StackTrace> prev;

            StackTrace st = new StackTrace(rethrow, true);

            if (!TryGetAssociatedStackTraces(rethrow, out prev)) {
                prev = new List<StackTrace>();
                AssociateStackTraces(rethrow, prev);
            }

            prev.Add(st);

#endif
            return rethrow;
        }

        private static void AssociateStackTraces(Exception e, List<StackTrace> traces) {
            Utils.ExceptionUtils.GetDataDictionary(e)[prevStackTraces] = traces;
        }

        private static bool TryGetAssociatedStackTraces(Exception e, out List<StackTrace> traces) {
            traces = Utils.ExceptionUtils.GetDataDictionary(e)[prevStackTraces] as List<StackTrace>;
            return traces != null;
        }

        /// <summary>
        /// Returns all the stack traces associates with an exception
        /// </summary>
        public static IList<StackTrace> GetExceptionStackTraces(Exception rethrow) {
            List<StackTrace> result;
            return TryGetAssociatedStackTraces(rethrow, out result) ? result : null;
        }

        public static void UpdateStackTrace(CodeContext context, MethodBase method, string funcName, string filename, int line) {
            if (_stackFrames == null) _stackFrames = new List<DynamicStackFrame>();

            Debug.Assert(line != SourceLocation.None.Line);

            _stackFrames.Add(new DynamicStackFrame(context, method, funcName, filename, line));
        }

        public static List<DynamicStackFrame> AssociateDynamicStackFrames(Exception clrException) {
            if (_stackFrames != null) {
                Utils.ExceptionUtils.GetDataDictionary(clrException)[typeof(DynamicStackFrame)] = _stackFrames;
            }
            return _stackFrames;
        }

        public static void ClearDynamicStackFrames() {
            _stackFrames = null;
        }

        public static void PushExceptionHandler(Exception clrException) {
            // _currentExceptions is thread static
            if (_currentExceptions == null) {
                _currentExceptions = new List<Exception>();
            }
            _currentExceptions.Add(clrException);

            AssociateDynamicStackFrames(clrException);
        }

        public static void PopExceptionHandler() {
            // _currentExceptions is thread static
            Debug.Assert(_currentExceptions != null);
            Debug.Assert(_currentExceptions.Count != 0);

#if !SILVERLIGHT
            ThreadAbortException tae = _currentExceptions[_currentExceptions.Count - 1] as ThreadAbortException;
            if (tae != null && tae.ExceptionState is KeyboardInterruptException) {
                Thread.ResetAbort();
            }
#endif
            _currentExceptions.RemoveAt(_currentExceptions.Count - 1);
        }
    }
}
