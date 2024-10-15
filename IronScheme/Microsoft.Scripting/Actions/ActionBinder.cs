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

using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Ast;

namespace Microsoft.Scripting.Actions
{
    /// <summary>
    /// Provides binding semantics for a language.  This include conversions as well as support
    /// for producing rules for actions.  These optimized rules are used for calling methods, 
    /// performing operators, and getting members using the ActionBinder's conversion semantics.
    /// </summary>
    public abstract class ActionBinder {
        private CodeContext _context;

        protected ActionBinder(CodeContext context) {
            _context = context;
        }

        /// <summary>
        /// Emits the code to convert an arbitrary object to the specified type.
        /// </summary>
        public virtual void EmitConvertFromObject(CodeGen cg, Type paramType) {
            cg.EmitCast(typeof(object), paramType);
        }

        /// <summary>
        /// Converts an object at runtime into the specified type.
        /// </summary>
        public virtual object Convert(object obj, Type toType) {
            if (obj == null) {
                if (!toType.IsValueType) {
                    return null;
                }
            } else {
                if (toType.IsValueType) {
                    if (toType == obj.GetType()) {
                        return obj;
                    }
                } else {
                    if (toType.IsInstanceOfType(obj)) {
                        return obj;
                    }
                }
            }
            throw new InvalidCastException(String.Format("Cannot convert {0} to {1}", obj != null ? obj.GetType().Name : "(null)", toType.Name));
        }

        /// <summary>
        /// Determines if a conversion exists from fromType to toType at the specified narrowing level.
        /// </summary>
        public abstract bool CanConvertFrom(Type fromType, Type toType, NarrowingLevel level);

        /// <summary>
        /// Selects the best (of two) candidates for conversion from actualType
        /// </summary>
        public virtual Type SelectBestConversionFor(Type actualType, Type candidateOne, Type candidateTwo, NarrowingLevel level) {
            return null;
        }

        /// <summary>
        /// Provides ordering for two parameter types if there is no conversion between the two parameter types.
        /// 
        /// Returns true to select t1, false to select t2.
        /// </summary>
        public abstract bool PreferConvert(Type t1, Type t2);


        /// <summary>
        /// Converts the provided expression to the given type.  The expression is safe to evaluate multiple times.
        /// </summary>
        public abstract Expression ConvertExpression(Expression expr, Type toType);

        /// <summary>
        /// Returns an expression which checks to see if the provided expression can be converted to the provided type.
        /// 
        /// TODO: Remove me when operator method binding disappears from the MethodBinder.
        /// </summary>
        public abstract Expression CheckExpression(Expression expr, Type toType);

        /// <summary>
        /// Gets the return value when an object contains out / by-ref parameters.  
        /// </summary>
        /// <param name="args">The values of by-ref and out parameters that the called method produced.  This includes the normal return
        /// value if the method does not return void.</param>
        public virtual object GetByRefArray(object[] args) {
            return args;
        }

    }
}

