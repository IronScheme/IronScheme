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
using Microsoft.Scripting.Generation.Allocators;
using Microsoft.Scripting.Generation.Slots;

namespace Microsoft.Scripting.Generation.Factories
{
    /// <summary>
    /// Creates FunctionEnvironments which are used for closures and exposing locals or arguments as a collection object to the user.
    /// 
    /// FunctionEnvironments are typically composed of two parts (but these can be the same objects):
    ///     1. The collection object exposed to the user (which implements IFunctionEnvironment)
    ///     2. The underlying storage object which generated code accesses for gets/sets.
    /// 
    /// The collection object exposed to the user is stored as the Locals property of the CodeContext.  The underlying
    /// storage can be of any data type.  The EnvironmentReferences that the EnvironmentFactory creates will create
    /// Slot's that access the storage in an appropriate manner.  These slots are passed the underlying storage object
    /// for their instance.
    /// 
    /// Creation of the environment factory consists of first creating the storage and then creating the collection object.
    /// </summary>
    internal abstract class EnvironmentFactory
    {
        /// <summary>
        /// Creates a reference within the environment with the specified name typed to object.
        /// </summary>
        /// <param name="name"></param>
        /// <returns></returns>
        public Storage MakeEnvironmentReference(SymbolId name)
        {
            return MakeEnvironmentReference(name, typeof(object));
        }

        /// <summary>
        /// Gets the type of the environment (must be an IFunctionEnvironment or not be nestable)
        /// </summary>
        public abstract Type EnvironmentType { get; }

        /// <summary>
        /// Gets the underlying storage type for the environment, can be any type
        /// </summary>
        public virtual Type StorageType { get { return EnvironmentType; } }

        /// <summary>
        /// Creates a reference within the environment of the specified type and name.
        /// </summary>
        /// <param name="name"></param>
        /// <param name="type"></param>
        /// <returns></returns>
        public abstract Storage MakeEnvironmentReference(SymbolId name, Type type);

        /// <summary>
        /// Creates a new function environment with the given names.  All values are left in their
        /// default state (null or zeroed structs)
        /// 
        /// Called with the following items on the stack:
        ///     Storage (as emitted by EmitStorage)
        ///     Parent environment link (an IFunctionEnvironment)
        ///     SymbolId[] of names in the environment
        ///     SymbolId[] of names in the outer environment
        /// </summary>
        public abstract void EmitNewEnvironment(CodeGen cg);

        /// <summary>
        /// Emits the creation of the underlying storage object.
        /// </summary>
        /// <param name="cg"></param>
        public abstract void EmitStorage(CodeGen cg);

        public abstract void EmitGetStorageFromContext(CodeGen cg);

        /// <summary>
        /// Creates the slot that holds onto the environment for the specified CodeGen.
        /// </summary>
        public abstract EnvironmentSlot CreateEnvironmentSlot(CodeGen cg);
    }
}
