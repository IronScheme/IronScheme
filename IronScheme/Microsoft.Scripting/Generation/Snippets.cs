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
using System.Reflection;
using System.Diagnostics;
using System.Threading;
using Microsoft.Scripting.Hosting;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Generation {

    public class Snippets {
        private const string AssemblyName = "snippets";
        private const string DebugAssemblyName = "debugSnippets";

        private AssemblyGen _assembly;
        private AssemblyGen _debugAssembly;

        private int _count = 0;
        private int _debugTypeIndex = 0;

        public AssemblyGen Assembly {
            get {
                if (_assembly == null) {
                    _assembly = CreateNewAssembly(AssemblyName, MakeGenAttrs(false));
                }
                return _assembly;
            }
        }

        public AssemblyGen DebugAssembly {
            get {
                if (_debugAssembly == null) {
                    _debugAssembly = CreateNewAssembly(DebugAssemblyName, MakeGenAttrs(true));
                }
                return _debugAssembly;
            }
        }

        public Snippets()  {
            _assembly = null; // lazy init
            _debugAssembly = null; // lazy init
        }

        private AssemblyGenAttributes MakeGenAttrs(bool debug) {
            // TODO:
            ScriptDomainOptions options = ScriptDomainManager.Options;

            AssemblyGenAttributes result = options.AssemblyGenAttributes;

            if (debug) {
                result |= AssemblyGenAttributes.EmitDebugInfo;

                
#if !SILVERLIGHT
                if (options.DebugCodeGeneration)
#endif
                    result |= AssemblyGenAttributes.DisableOptimizations;
            }

            return result;
        }

        private string GenerateAssemblyName(string baseName) {
            int current_count = Interlocked.Increment(ref _count);
            return (current_count > 0) ? baseName + current_count : baseName;
        }

        private AssemblyGen CreateNewAssembly(string name, AssemblyGenAttributes attrs) {
            Debug.Assert(!String.IsNullOrEmpty(name));
            name = GenerateAssemblyName(name);
            return new AssemblyGen(name, null, name + ".dll", attrs);
        }

        public void Dump() {
            Dump(null);
        }

        public void Dump(string fileName) {
            if (_assembly != null && _assembly.SaveAndReloadAssemblies) {
                _assembly.Dump(fileName);
                _assembly = null;
            }

            if (_debugAssembly != null && _debugAssembly.SaveAndReloadAssemblies) {
                _debugAssembly.Dump();
                _debugAssembly = null;
            }
        }

        public TypeGen DefineDebuggableType(string typeName, SourceUnit sourceUnit) {
            typeName = typeName.Replace(Type.Delimiter, '_'); // '.' is for separating the namespace and the type name.
            DebugAssembly.SetSourceUnit(sourceUnit);
            TypeGen tg = DebugAssembly.DefinePublicType(typeName + "$" + _debugTypeIndex++, typeof(object));
            tg.TypeBuilder.DefineDefaultConstructor(MethodAttributes.Public);
            return tg;
        }
    }
}
