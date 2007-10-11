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

using Microsoft.Scripting.Actions;

namespace Microsoft.Scripting.Ast {
    public enum ArgumentKind {
        Simple,
        Named,
        List,
        Dictionary,
        Instance,
        Block
    };

    public class Arg : Node {
        private readonly Expression _expr;
        private readonly ArgumentInfo _info;

        private Arg(SymbolId name, Expression expression, ArgumentKind kind) {
            _expr = expression;
            _info = new ArgumentInfo(kind, name);
        }

        public override string ToString() {
            return base.ToString() + ":" + SymbolTable.IdToString(_info.Name);
        }

        public ArgumentKind Kind {
            get { return _info.Kind; }
        }
        
        public SymbolId Name {
            get { return _info.Name; }
        }

        public ArgumentInfo Info {
            get { return _info; }
        }

        public Expression Expression {
            get { return _expr; }
        }
        
        public override void Walk(Walker walker) {
            if (walker.Walk(this)) {
                _expr.Walk(walker);
            }
            walker.PostWalk(this);
        }

        #region Factory methods

        public static Arg List(Expression expression) {
            return new Arg(SymbolId.Empty, expression, ArgumentKind.List);
        }

        public static Arg Dictionary(Expression expression) {
            return new Arg(SymbolId.Empty, expression, ArgumentKind.Dictionary);
        }

        public static Arg Named(SymbolId name, Expression expression) {
            return new Arg(name, expression, ArgumentKind.Named);
        }

        public static Arg Simple(Expression expression) {
            return new Arg(SymbolId.Empty, expression, ArgumentKind.Simple);
        }

        public static Arg Instance(Expression expression) {
            return new Arg(SymbolId.Empty, expression, ArgumentKind.Instance);
        }

        #endregion
    }
}
