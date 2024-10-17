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
using System.Collections.Generic;
using System.Diagnostics;
using Microsoft.Scripting.Hosting;
using Microsoft.Scripting.Generation.Slots;

namespace Microsoft.Scripting.Generation.Factories
{
    internal class ModuleGlobalFactory : SlotFactory
    {
        private SlotFactory _storage;

        public ModuleGlobalFactory(SlotFactory storage)
        {
            _storage = storage;
        }

        protected override Slot CreateSlot(SymbolId name, Type type)
        {
            return new ModuleGlobalSlot(_storage.MakeSlot(name, typeof(ModuleGlobalWrapper)));
        }

        public SlotFactory Storage
        {
            get
            {
                return _storage;
            }
        }

        public override void PrepareForEmit(CodeGen cg)
        {
            _storage.PrepareForEmit(cg);
        }

    }
}
