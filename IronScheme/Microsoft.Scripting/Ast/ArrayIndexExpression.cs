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
using System.Diagnostics;

using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Ast {
    public class ArrayIndexExpression : Expression {
        private readonly Expression _array;
        private readonly Expression _index;
        private readonly Type _elementType;

        internal ArrayIndexExpression(Expression array, Expression index) {
            _array = array;
            _index = index;
            _elementType = array.Type.GetElementType();
        }

        public Expression Array {
            get { return _array; }
        }

        public Expression Index {
            get { return _index; }
        }

        public override Type Type {
            get {
                return _elementType;
            }
        }

        protected override object DoEvaluate(CodeContext context) {
            object[] array = (object[])_array.Evaluate(context);
            int index = (int)_index.Evaluate(context);
            return array[index];
        }

        public override void Emit(CodeGen cg) {
            // Emit the array reference
            _array.Emit(cg);
            // Emit the index
            _index.Emit(cg);
            // Load the array element
            cg.EmitLoadElement(_elementType);
        }

        public override void Walk(Walker walker) {
            if (walker.Walk(this)) {
                _array.Walk(walker);
                _index.Walk(walker);
            }
            walker.PostWalk(this);
        }
    }

    public static partial class Ast {
        public static ArrayIndexExpression ArrayIndex(Expression array, Expression index) {
            Contract.RequiresNotNull(array, "array");
            Contract.RequiresNotNull(index, "index");
            Contract.Requires(index.Type == typeof(int), "index", "Array index must be an int.");

            Type arrayType = array.Type;
            Contract.Requires(arrayType.IsArray, "array", "Array argument must be array.");
            Contract.Requires(arrayType.GetArrayRank() == 1, "index", "Incorrect number of indices.");

            return new ArrayIndexExpression(array, index);
        }
    }
}
