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
using Microsoft.Scripting.Generation;

namespace Microsoft.Scripting.Hosting
{
    [Serializable]
    public sealed class ScriptDomainOptions {
        private bool _debugMode = false;
        private bool _debugCodeGen = false;
        private AssemblyGenAttributes _assemblyGenAttributes = AssemblyGenAttributes.GenerateDebugAssemblies;
        private string _binariesDirectory;

        public bool LightweightDebugging {get;set;}

        public bool StrictMode { get; set; }

        #region Public accessors

        /// <summary>
        ///  Is this a debug mode? "__debug__" is defined, and Asserts are active
        /// </summary>
        public bool DebugMode {
            get { return _debugMode; }
            set { _debugMode = value; }
        }

        /// <summary>
        /// Should optimized code gen be disabled.
        /// </summary>
        public bool DebugCodeGeneration {
            get { return _debugCodeGen; }
            set { _debugCodeGen = value; }
        }

        public AssemblyGenAttributes AssemblyGenAttributes {
            get { return _assemblyGenAttributes; }
            set { _assemblyGenAttributes = value; }
        }

        public string BinariesDirectory {
            get { return _binariesDirectory; }
            set { _binariesDirectory = value; }
        }

        #endregion
    }
}