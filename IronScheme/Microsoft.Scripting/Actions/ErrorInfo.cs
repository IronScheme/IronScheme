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
using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Actions {
    /// <summary>
    /// Encapsulates information about the result that should be produced when 
    /// a DynamicAction cannot be performed.  The ErrorInfo either holds an 
    /// expression which creates an Exception to be thrown or an expression which 
    /// produces a value which should be returned directly to the user.
    /// 
    /// ErrorInfo's are produced by the ActionBinder in response to a failed
    /// binding.  
    /// </summary>
    public sealed class ErrorInfo {
        private readonly Expression _exception;
        private readonly Expression _value;

        /// <summary>
        /// Private constructor - consumers must use static From* factories
        /// to create ErrorInfo objects.
        /// </summary>
        private ErrorInfo(Expression exception, Expression value) {
            _exception = exception;
            _value = value;
        }

        /// <summary>
        /// Creates a new ErrorInfo which represents an exception that should
        /// be thrown.
        /// </summary>
        public static ErrorInfo FromException(Expression exceptionValue) {
            Contract.RequiresNotNull(exceptionValue, "exceptionValue");
            Contract.Requires(typeof(Exception).IsAssignableFrom(exceptionValue.Type), "exceptionValue", "must by an Exception instance");

            return new ErrorInfo(exceptionValue, null);
        }

        /// <summary>
        /// Creates a new ErrorInfo which represents a value which should be
        /// returned to the user.
        /// </summary>
        public static ErrorInfo FromValue(Expression resultValue) {
            Contract.RequiresNotNull(resultValue, "resultValue");

            return new ErrorInfo(null, resultValue);
        }

        /// <summary>
        /// Internal helper to produce the actual expression used for the error when emitting
        /// the error into a rule.
        /// </summary>
        public Statement MakeErrorForRule(StandardRule rule, ActionBinder binder) {
            if (_value != null) {
                rule.IsError = true;
                return rule.MakeReturn(binder, _value);
            }

            return rule.MakeError(_exception);
        }
    }
}
