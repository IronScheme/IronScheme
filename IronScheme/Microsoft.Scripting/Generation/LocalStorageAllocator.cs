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

namespace Microsoft.Scripting.Generation {
    sealed class LocalStorageAllocator : StorageAllocator {
        private SlotFactory _factory;

        internal LocalStorageAllocator(SlotFactory factory) {
            _factory = factory;
        }

        public override Storage AllocateStorage(SymbolId name, Type type) {
            return new LocalStorage(_factory.MakeSlot(name, type));
        }
    }
}
