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
using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Generation.Slots;

namespace Microsoft.Scripting.Generation.Allocators
{
    abstract class Storage
    {
        public abstract bool RequireAccessSlot { get; }
        public abstract Slot CreateSlot(Slot instance);
    }

    internal abstract class StorageAllocator
    {
        public virtual void PrepareForEmit(CodeGen cg)
        {
        }

        // TODO: change the parameter to take Variable !!!
        public abstract Storage AllocateStorage(SymbolId name, Type type);

        public virtual Slot GetAccessSlot(CodeGen cg, CodeBlock block)
        {
            return null;
        }
    }
}
