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

namespace Microsoft.Scripting.Actions {

    public class ConvertToAction : DynamicAction, IEquatable<ConvertToAction> {
        private Type _type;
        private ConversionResultKind _resultKind;

        public static ConvertToAction Make(Type type) {
            return new ConvertToAction(type, ConversionResultKind.ImplicitCast);
        }

        public static ConvertToAction Make(Type type, ConversionResultKind resultKind) {
            return new ConvertToAction(type, resultKind);
        }

        private ConvertToAction(Type type, ConversionResultKind resultKind) { 
            this._type = type;
            this._resultKind = resultKind;
        }

        public Type ToType { get { return _type; } }
        public ConversionResultKind ResultKind { get { return _resultKind; } }
        public override DynamicActionKind Kind { get { return DynamicActionKind.ConvertTo; } }

        public override bool Equals(object obj) {
            return Equals(obj as ConvertToAction);
        }

        public override int GetHashCode() {
            return (int)Kind << 28 ^ (int)ResultKind ^ _type.GetHashCode();
        }

        public override string ToString() {
            return base.ToString() + " to " + _type.ToString();
        }

        #region IEquatable<ConvertToAction> Members

        public bool Equals(ConvertToAction other) {
            if (other == null) return false;
            return _type == other._type && _resultKind == other._resultKind;
        }

        #endregion
    }

    /// <summary>
    /// Determines the result of a conversion action.  The result can either result in an exception, a value that
    /// has been successfully converted or default(T), or a true/false result indicating if the value can be converted.
    /// </summary>
    public enum ConversionResultKind {
        /// <summary>
        /// Attempts to perform available implicit conversions and throws if there are no available conversions.
        /// </summary>
        ImplicitCast,
        /// <summary>
        /// Attempst to perform available implicit and explicit conversions and throws if there are no available conversions.
        /// </summary>
        ExplicitCast,
        /// <summary>
        /// Attempts to perform available implicit conversions and returns default(ReturnType) if no conversions can be performed.
        /// 
        /// If the return type of the rule is a value type then the return value will be zero-initialized.  If the return type
        /// of the rule is object or another class then the return type will be null (even if the conversion is to a value type).
        /// This enables ImplicitTry to be used to do TryConvertTo even if the type is value type (and the difference between
        /// null and a real value can be distinguished).
        /// </summary>
        ImplicitTry,
        /// <summary>
        /// Attempts to perform available implicit and explicit conversions and returns default(ReturnType) if no conversions 
        /// can be performed.
        /// 
        /// If the return type of the rule is a value type then the return value will be zero-initialized.  If the return type
        /// of the rule is object or another class then the return type will be null (even if the conversion is to a value type).
        /// This enables ExplicitTry to be used to do TryConvertTo even if the type is value type (and the difference between
        /// null and a real value can be distinguished).
        /// </summary>
        ExplicitTry
    }
}
