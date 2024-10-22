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

namespace Microsoft.Scripting
{
    public class ModuleContext {
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2105:ArrayFieldsShouldNotBeReadOnly")]
        public static readonly ModuleContext[] EmptyArray = new ModuleContext[0];

        private readonly ScriptModule _module;
        private CompilerContext _compilerContext;

        /// <summary>
        /// Optional.
        /// </summary>
        public ScriptModule Module {
            get { return _module; }
        }

        /// <summary>
        /// Returns the optional compiler context associated with this module.
        /// </summary>
        public CompilerContext CompilerContext {
            get {
                return _compilerContext;
            }
            set {
                _compilerContext = value;
            }
        }

        /// <summary>
        /// Creates a module context.
        /// </summary>
        /// <param name="module">Optional. <c>null</c> for default and invariant contexts.</param>
        public ModuleContext(ScriptModule module) {
            _module = module;
        }
    }
}
