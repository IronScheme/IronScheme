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
using System.Reflection;
using System.Reflection.Emit;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Generation.Slots
{
    /// <summary>
    /// A slot backed by a static field in a type
    /// </summary>
    internal class StaticFieldSlot : Slot
    {
        private readonly FieldInfo _field;

        public StaticFieldSlot(FieldInfo field)
        {
            Contract.RequiresNotNull(field, "field");

            _field = field;
        }
        public override void EmitGet(CodeGen cg)
        {
            Contract.RequiresNotNull(cg, "cg");

            cg.EmitFieldGet(_field);
        }

        public override void EmitGetAddr(CodeGen cg)
        {
            Contract.RequiresNotNull(cg, "cg");

            cg.EmitFieldAddress(_field);
        }

        public override void EmitSet(CodeGen cg)
        {
            Contract.RequiresNotNull(cg, "cg");

            cg.EmitFieldSet(_field);
        }

        public override Type Type
        {
            get
            {
                return _field.FieldType;
            }
        }

        /// <summary>
        /// Gets the FieldInfo for which this slot will emit a get / set for.
        /// </summary>
        public FieldInfo Field
        {
            get { return _field; }
        }

        public override string ToString()
        {
            return string.Format("StaticFieldSlot Field: {0}.{1} Type: {1}", _field.DeclaringType, _field.Name, _field.FieldType);
        }
    }
}
