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
using System.Reflection.Emit;
using System.Reflection;

namespace Microsoft.Scripting.Generation.Slots
{

    public class ClassEnvironmentSlot : EnvironmentSlot
    {
        private Type _storageType;

        public ClassEnvironmentSlot(Slot storage, Type storageType)
          : base(storage)
        {
            _storageType = storageType;
        }

        public override void EmitGetDictionary(CodeGen cg)
        {
            EmitGet(cg);
            cg.EmitFieldGet(_storageType, "$parent$");
        }
    }
}
