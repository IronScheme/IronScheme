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
using System.Collections.Generic;
using System.Text;

namespace Microsoft.Scripting.Generation {
    /// <summary>
    /// Specifies options for assembly generation
    /// </summary>
    [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2217:DoNotMarkEnumsWithFlags")] // TODO: fix
    [Flags]
    public enum AssemblyGenAttributes {
        /// <summary> No options specified - use the default settings </summary>
        None,
        
        /// <summary> Emit PDBs to enable debugging </summary>
        EmitDebugInfo = 0x0001,
        
        /// <summary> Create types with the BeforeFieldInit attribute set. </summary>
        BeforeFieldInit = 0x0002,
        
        /// <summary> Disable optimizations in the generated code. </summary>
        DisableOptimizations = 0x0004,
        
        /// <summary>
        /// When creating new top-level methods create those methods as dynamic methods, not
        /// methods generated in a new top-level type.
        /// </summary>
        GenerateStaticMethods = 0x0008,

        /// <summary>
        /// Apply attributes to make the assembly debuggable.
        /// </summary>
        GenerateDebugAssemblies = 0x010,

        /// <summary>
        /// Persist generated assemblies.
        /// </summary>
        SaveAndReloadAssemblies = 0x020,

        /// <summary>
        /// Verify generated assemblies.
        /// </summary>
        VerifyAssemblies = 0x040,

        /// <summary>
        /// Generate IL source files.
        /// TODO: Do not require GenerateStaticMethods when ILDebug is on - emit the textual IL even for dynamic methods ...
        /// </summary>
        ILDebug = 0x080 | GenerateStaticMethods,
    }
}
