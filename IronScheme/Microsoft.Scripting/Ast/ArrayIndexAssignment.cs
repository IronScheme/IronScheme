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
using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Ast {
    public class ArrayIndexAssignment : Expression {
        private readonly Expression _array;
        private readonly Expression _index;
        private readonly Expression _value;
        private readonly Type _elementType;

        internal ArrayIndexAssignment(Expression array, Expression index, Expression value) {
            _array = array;
            _index = index;
            _value = value;
            _elementType = array.Type.GetElementType();
        }

        public Expression Array {
            get { return _array; }
        }

        public Expression Index {
            get { return _index; }
        }

        public Expression Value {
            get { return _value; }
        }

        public override Type Type {
            get {
                return _elementType;
            }
        }

        protected override object DoEvaluate(CodeContext context) {
            object value = _value.Evaluate(context); // evaluate the value first
            Array array = (Array)_array.Evaluate(context);
            int index = (int)_index.Evaluate(context);
            array.SetValue(value, index);            
            return value;
        }

        public override void Emit(CodeGen cg) {
            _value.Emit(cg);

            // Save the expression value - order of evaluation is different than that of the Stelem* instruction
            Slot temp = cg.GetLocalTmp(_elementType);
            temp.EmitSet(cg);

            // Emit the array reference
            _array.Emit(cg);
            // Emit the index (as integer)
            _index.Emit(cg);
            // Emit the value
            temp.EmitGet(cg);
            // Store it in the array
            cg.EmitStoreElement(_elementType);
            temp.EmitGet(cg);
            cg.FreeLocalTmp(temp);
        }

        public override void Walk(Walker walker) {
            if (walker.Walk(this)) {
                _array.Walk(walker);
                _index.Walk(walker);
                _value.Walk(walker);
            }
            walker.PostWalk(this);
        }
    }

    public static partial class Ast {
        public static ArrayIndexAssignment AssignArrayIndex(Expression array, Expression index, Expression value) {
            Contract.RequiresNotNull(array, "array");
            Contract.RequiresNotNull(index, "index");
            Contract.RequiresNotNull(value, "value");
            Contract.Requires(index.Type == typeof(int), "index", "Array index must be an int.");

            Type arrayType = array.Type;
            Contract.Requires(arrayType.IsArray, "array", "Array argument must be array.");
            Contract.Requires(arrayType.GetArrayRank() == 1, "index", "Incorrect number of indices.");
            Contract.Requires(value.Type == arrayType.GetElementType(), "value", "Value type must match the array element type.");

            return new ArrayIndexAssignment(array, index, value);
        }
    }
}
