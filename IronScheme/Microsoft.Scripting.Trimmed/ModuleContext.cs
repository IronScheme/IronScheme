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
using System.Threading;
using System.Diagnostics;
using System.Runtime.Remoting;

using Microsoft.Scripting.Actions;
using Microsoft.Scripting.Hosting;
using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting {
    public class ModuleContext {
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2105:ArrayFieldsShouldNotBeReadOnly")]
        public static readonly ModuleContext[] EmptyArray = new ModuleContext[0];

        private readonly ScriptModule _module;
        private bool _showCls;
        private CompilerContext _compilerContext;

        /// <summary>
        /// Optional.
        /// </summary>
        public ScriptModule Module {
            get { return _module; }
        }

        /// <summary>
        /// Returns the attributes associated with this LanguageContext's code.
        /// </summary>
        public virtual bool ShowCls {
            get {
                return _showCls;
            }
            set {
                _showCls = value;
            }
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

        /// <summary>
        /// Copy constructor.
        /// </summary>
        protected ModuleContext(ModuleContext context) {
            Contract.RequiresNotNull(context, "context");
            _module = context._module;
            _showCls = context._showCls;
            _compilerContext = context._compilerContext;
        }

        internal protected virtual void ModuleReloading() {
            _showCls = false;
        }

        internal protected virtual void ModuleReloaded() {
        }
    }
}
