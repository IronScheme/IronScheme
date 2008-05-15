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
using Microsoft.Scripting.Ast;
using System.Diagnostics;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Actions {
    /// <summary>
    /// TODO: Alternatively, it should be sufficient to remember indices for this, list, dict and block.
    /// </summary>
    public struct ArgumentInfo : IEquatable<ArgumentInfo> {
        private readonly ArgumentKind _kind;
        private readonly SymbolId _name;

        public static readonly ArgumentInfo Simple = new ArgumentInfo(ArgumentKind.Simple, SymbolId.Empty);

        public ArgumentKind Kind { get { return _kind; } }
        public SymbolId Name { get { return _name; } }

        public ArgumentInfo(SymbolId name) {
            _kind = ArgumentKind.Named;
            _name = name;
        }

        public ArgumentInfo(ArgumentKind kind) {
            _kind = kind;
            _name = SymbolId.Empty;
        }

        public ArgumentInfo(ArgumentKind kind, SymbolId name) {
            Contract.Requires((kind == ArgumentKind.Named) ^ (name == SymbolId.Empty), "kind");
            _kind = kind;
            _name = name;
        }

        public override bool Equals(object obj) {
            return obj is ArgumentInfo && Equals((ArgumentInfo)obj);
        }

        public bool Equals(ArgumentInfo other) {
            return _kind == other._kind && _name == other._name;
        }

        public static bool operator ==(ArgumentInfo left, ArgumentInfo right) {
            return left.Equals(right);
        }

        public static bool operator !=(ArgumentInfo left, ArgumentInfo right) {
            return !left.Equals(right);
        }

        public override int GetHashCode() {
            return _name.GetHashCode() ^ (int)_kind;
        }

        public bool IsSimple {
            get {
                return Equals(Simple);
            }
        }

        public override string ToString() {
            return _name == SymbolId.Empty ? _kind.ToString() : _kind.ToString() + ":" + SymbolTable.IdToString(_name);
        }
    }
}


