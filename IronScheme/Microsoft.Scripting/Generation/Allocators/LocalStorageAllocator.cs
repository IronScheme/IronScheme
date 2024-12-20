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
using Microsoft.Scripting.Generation.Factories;
using Microsoft.Scripting.Generation.Slots;

namespace Microsoft.Scripting.Generation.Allocators
{
    sealed class LocalStorageAllocator : StorageAllocator
    {
        sealed class LocalStorage : Storage
        {
            private Slot _slot;

            internal LocalStorage(Slot slot)
            {
                _slot = slot;
            }

            public override bool RequireAccessSlot
            {
                get { return false; }
            }

            public override Slot CreateSlot(Slot instance)
            {
                return _slot;
            }
        }

        private SlotFactory _factory;

        internal LocalStorageAllocator(SlotFactory factory)
        {
            _factory = factory;
        }

        public override Storage AllocateStorage(SymbolId name, Type type)
        {
            return new LocalStorage(_factory.MakeSlot(name, type));
        }
    }
}
