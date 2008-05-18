
#if FULL
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
using System.Text;
using System.Reflection;
using System.IO;
using System.Diagnostics;
using System.Threading;

using Microsoft.Scripting;
using Microsoft.Scripting.Hosting;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting {
    /// <summary>
    /// this class contains objecs and static methods used for
    /// .NET/CLS interop with Python.  
    /// </summary>
    public sealed class ClrModule {
        private static ClrModule _module;

        public static ClrModule GetInstance() {
            if (_module == null) {
                Interlocked.CompareExchange<ClrModule>(ref _module, new ClrModule(), null);
            }
            return _module;
        }

        private ClrModule() {
        }

        public override string ToString() {
            return "<module 'clr' (built-in)>";
        }

        public ReferencesList References = new ReferencesList();

        #region Public methods

        public void AddReference(params object[] references) {
            if (references == null) throw new ArgumentTypeException("Expected string or Assembly, got NoneType");
            if (references.Length == 0) throw new ArgumentException("Expected at least one name, got none");

            foreach (object reference in references) {
                AddReference(reference);
            }
        }

        public void AddReferenceToFile(CodeContext context, params string[] files) {
            if (files == null) throw new ArgumentTypeException("Expected string, got NoneType");
            if (files.Length == 0) throw new ArgumentException("Expected at least one name, got none");

            foreach (string file in files) {
                AddReferenceToFile(context, file);
            }
        }

        public void AddReferenceByName(params string[] names) {
            if (names == null) throw new ArgumentTypeException("Expected string, got NoneType");
            if (names.Length == 0) throw new ArgumentException("Expected at least one name, got none");

            foreach (string name in names) {
                AddReferenceByName(name);
            }
        }

#if !SILVERLIGHT // files, paths
        public void AddReferenceByPartialName(params string[] names) {
            if (names == null) throw new ArgumentTypeException("Expected string, got NoneType");
            if (names.Length == 0) throw new ArgumentException("Expected at least one name, got none");

            foreach (string name in names) {
                AddReferenceByPartialName(name);
            }
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Reliability", "CA2001:AvoidCallingProblematicMethods", MessageId = "System.Reflection.Assembly.LoadFile")]
        public Assembly LoadAssemblyFromFileWithPath(string file) {
            if (file == null) throw new ArgumentTypeException("LoadAssemblyFromFileWithPath: arg 1 must be a string.");
            // We use Assembly.LoadFile instead of Assembly.LoadFrom as the latter first tries to use Assembly.Load
            return Assembly.LoadFile(file);
        }

        public Assembly LoadAssemblyFromFile(CodeContext context, string file) {
            if (file == null) throw new ArgumentTypeException("Expected string, got NoneType");
            if (file.Length == 0) throw new ArgumentException("assembly name must not be empty string");

            if (file.IndexOf(System.IO.Path.DirectorySeparatorChar) != -1) {
                throw new ArgumentException("filenames must not contain full paths, first add the path to sys.path");
            }

            return context.LanguageContext.LoadAssemblyFromFile(file);
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Reliability", "CA2001:AvoidCallingProblematicMethods", MessageId = "System.Reflection.Assembly.LoadWithPartialName")]
        public Assembly LoadAssemblyByPartialName(string name) {
            if (name == null) throw new ArgumentTypeException("LoadAssemblyByPartialName: arg 1 must be a string");
#pragma warning disable 618
            return Assembly.LoadWithPartialName(name);
#pragma warning restore 618
        }
#endif

        public Assembly LoadAssemblyByName(string name) {
            if (name == null) throw new ArgumentTypeException("LoadAssemblyByName: arg 1 must be a string");
            return ScriptDomainManager.CurrentManager.PAL.LoadAssembly(name);
        }

        public ScriptModule Use(string name) {
            ScriptModule res = ScriptDomainManager.CurrentManager.UseModule(name);
            if (res == null) throw new ArgumentException(String.Format("couldn't find module {0} to use", name));

            return res;
        }

        public ScriptModule Use(string path, string language) {
            ScriptModule res = ScriptDomainManager.CurrentManager.UseModule(path, language);
            if (res == null) throw new ArgumentException(String.Format("couldn't load module at path '{0}' in language '{1}'", path, language));

            return res;
        }

        public CommandDispatcher SetCommandDispatcher(CommandDispatcher dispatcher) {
            return ScriptDomainManager.CurrentManager.SetCommandDispatcher(dispatcher);
        }

        #endregion
        
        #region Private implementation methods

        private void AddReference(object reference) {
            Assembly asmRef = reference as Assembly;
            if (asmRef != null) {
                AddReference(asmRef);
                return;
            }

            string strRef = reference as string;
            if (strRef != null) {
                AddReference(strRef);
                return;
            }

            throw new ArgumentTypeException(String.Format("Invalid assembly type. Expected string or Assembly, got {0}.", reference));
        }

        private void AddReference(Assembly assembly) {
            // Load the assembly into IronPython
            if (RuntimeHelpers.TopNamespace.LoadAssembly(assembly)) {
                // Add it to the references tuple if we
                // loaded a new assembly.
                References.Add(assembly);
            }
        }

        private void AddReference(string name) {
            if (name == null) throw new ArgumentTypeException("Expected string, got NoneType");

            Assembly asm = null;

            try {
                asm = LoadAssemblyByName(name);
            } catch { }

            // note we don't explicit call to get the file version
            // here because the assembly resolve event will do it for us.

#if !SILVERLIGHT // files, paths
            if (asm == null) {
                asm = LoadAssemblyByPartialName(name);
            }
#endif

            if (asm == null) {
                throw new IOException(String.Format("Could not add reference to assembly {0}", name));
            }
            AddReference(asm);
        }

        private void AddReferenceToFile(CodeContext context, string file) {
            if (file == null) throw new ArgumentTypeException("Expected string, got NoneType");

#if SILVERLIGHT
            Assembly asm = ScriptDomainManager.CurrentManager.PAL.LoadAssemblyFromPath(file);
#else
            Assembly asm = LoadAssemblyFromFile(context, file);
#endif
            if (asm == null) {
                throw new IOException(String.Format("Could not add reference to assembly {0}", file));
            }

            AddReference(asm);
        }

#if !SILVERLIGHT // files, paths
        private void AddReferenceByPartialName(string name) {
            if (name == null) throw new ArgumentTypeException("Expected string, got NoneType");

            Assembly asm = LoadAssemblyByPartialName(name);
            if (asm == null) {
                throw new IOException(String.Format("Could not add reference to assembly {0}", name));
            }

            AddReference(asm);
        }

#endif
        private void AddReferenceByName(string name) {
            if (name == null) throw new ArgumentTypeException("Expected string, got NoneType");

            Assembly asm = LoadAssemblyByName(name);

            if (asm == null) {
                throw new IOException(String.Format("Could not add reference to assembly {0}", name));
            }

            AddReference(asm);
        }

        #endregion       

        public class ReferencesList : List<Assembly> {
        }
    }
}

#endif	
