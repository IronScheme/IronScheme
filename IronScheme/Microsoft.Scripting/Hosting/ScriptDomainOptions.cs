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
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using Microsoft.Scripting.Generation;

namespace Microsoft.Scripting.Hosting {
    [Serializable]
    public sealed class ScriptDomainOptions {
        private bool _debugMode = false;
        private bool _engineDebug;
        private bool _verbose;
        private bool _traceBackSupport = (IntPtr.Size == 4);  // currently only enabled on 32-bit
        private bool _debugCodeGen = false;
        private bool _trackPerformance;
        private bool _optimizeEnvironments = true;
        private bool _frames;
        private AssemblyGenAttributes _assemblyGenAttributes = AssemblyGenAttributes.GenerateDebugAssemblies;
        private string _binariesDirectory;
        private bool _privateBinding;
        private bool _generateModulesAsSnippets;
        private bool _bufferedStdOutAndError = true;
        private bool _showASTs = false;
        private bool _dumpASTs = false;
        private bool _showRules = false;
        
        public ScriptDomainOptions() {
        }

        #region Public accessors

        /// <summary>
        ///  Is this a debug mode? "__debug__" is defined, and Asserts are active
        /// </summary>
        public bool DebugMode {
            get { return _debugMode; }
            set { _debugMode = value; }
        }

        /// <summary>
        /// corresponds to the "-v" command line parameter
        /// </summary>
        public bool Verbose {
            get { return _verbose; }
            set { _verbose = value; }
        }

        /// <summary>
        /// Is the engine in debug mode? This is useful for debugging the engine itself
        /// </summary>
        public bool EngineDebug {
            get { return _engineDebug; }
            set { 
                _engineDebug = value;
#if DEBUG
                if (value) _assemblyGenAttributes |= AssemblyGenAttributes.VerifyAssemblies;
                else _assemblyGenAttributes &= ~AssemblyGenAttributes.VerifyAssemblies;
#endif
            }
        }

        public bool DynamicStackTraceSupport {
            get { return _traceBackSupport; }
            set { _traceBackSupport = value; }
        }

        public bool TrackPerformance {
            get { return _trackPerformance; }
            set { _trackPerformance = value; }
        }

        /// <summary>
        /// Should optimized code gen be disabled.
        /// </summary>
        public bool DebugCodeGeneration {
            get { return _debugCodeGen; }
            set { _debugCodeGen = value; }
        }

        /// <summary>
        /// Closures, generators and environments can use the optimized
        /// generated environments FunctionEnvironment2 .. 32 
        /// If false, environments are stored in FunctionEnvironmentN only
        /// </summary>
        public bool OptimizeEnvironments {
            get { return _optimizeEnvironments; }
            set { _optimizeEnvironments = value; }
        }

        /// <summary>
        /// Generate functions using custom frames. Allocate the locals on frames.
        /// </summary>
        public bool Frames {
            get { return _frames; }
            set { _frames = value; }
        }

        public AssemblyGenAttributes AssemblyGenAttributes {
            get { return _assemblyGenAttributes; }
            set { _assemblyGenAttributes = value; }
        }

        public string BinariesDirectory {
            get { return _binariesDirectory; }
            set { _binariesDirectory = value; }
        }

        public bool PrivateBinding {
            get { return _privateBinding; }
            set { _privateBinding = value; }
        }

        /// <summary>
        /// true to import modules as though a sequence of snippets 
        /// </summary>
        public bool GenerateModulesAsSnippets {
            get { return _generateModulesAsSnippets; }
            set { _generateModulesAsSnippets = value; }
        }

        // obsolete:
        public bool BufferedStandardOutAndError {
            get { return _bufferedStdOutAndError; }
            set { _bufferedStdOutAndError = value; }
        }


        /// <summary>
        ///  Should we strip out all doc strings (the -OO command line option)?
        /// </summary>
        private bool stripDocStrings = false;

        public bool StripDocStrings {
            get { return stripDocStrings; }
            set { stripDocStrings = value; }
        }

        /// <summary>
        ///  The runtime assembly was built in Debug/Release mode.
        /// </summary>
        public bool DebugAssembly {
            get {
#if DEBUG
                return true;
#else
                return false;
#endif
            }
        }

        /// <summary>
        /// Print generated Abstract Syntax Trees to the console
        /// </summary>
        public bool ShowASTs {
            get { return _showASTs; }
            set { _showASTs = value; }
        }
        /// <summary>
        /// Write out generated Abstract Syntax Trees as files in the current directory
        /// </summary>
        public bool DumpASTs {
            get { return _dumpASTs; }
            set { _dumpASTs = value; }
        }

        /// <summary>
        /// Print generated action dispatch rules to the console
        /// </summary>
        public bool ShowRules {
            get { return _showRules; }
            set { _showRules = value; }
        }

        #endregion
    }
}