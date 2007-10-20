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
using System.Reflection;
using System.Diagnostics;
using System.Collections;
using System.IO;
using Microsoft.Scripting;
using Microsoft.Scripting.Shell;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Hosting {

#if SILVERLIGHT
    public class ExitProcessException : Exception {

        public int ExitCode { get { return exitCode; } }
        int exitCode;

        public ExitProcessException(int exitCode) {
            this.exitCode = exitCode;
        }
    }
#endif

    public class PlatformAdaptationLayer {

#if SILVERLIGHT
        private Dictionary<string, string> _assemblyFullNames = new Dictionary<string, string>();

        public PlatformAdaptationLayer() {
            LoadSilverlightAssemblyNameMapping();
        }

        // TODO: remove the need for this
        private void LoadSilverlightAssemblyNameMapping() {
            AssemblyName clrAssembly = new AssemblyName(typeof(object).Assembly.FullName);
            foreach (string asm in new string[] { "mscorlib", "System", "System.Core", "System.Xml.Core" }) {
                clrAssembly.Name = asm;
                _assemblyFullNames.Add(asm.ToLower(), clrAssembly.FullName);
            }

            _assemblyFullNames.Add("system.silverlight", "System.SilverLight, Version=1.0.0.0, PublicKeyToken=b03f5f7f11d50a3a");
            _assemblyFullNames.Add("agclr", "agclr, Version=0.0.0.0, PublicKeyToken=b03f5f7f11d50a3a");
            _assemblyFullNames.Add("microsoft.visualbasic", "Microsoft.VisualBasic, Version=8.1.0.0, PublicKeyToken=b03f5f7f11d50a3a");

            AssemblyName dlrAssembly = new AssemblyName(typeof(PlatformAdaptationLayer).Assembly.FullName);            
            foreach (string asm in new string[] {
                "Microsoft.Scripting",
                "Microsoft.Scripting.Silverlight",
                "IronPython",
                "IronPython.Modules",
                "Microsoft.JScript.Compiler",
                "Microsoft.JScript.Runtime",
                "Microsoft.VisualBasic.Compiler",
                "Microsoft.VisualBasic.Scripting",
                "Ruby"}) {
                dlrAssembly.Name = asm;
                _assemblyFullNames.Add(asm.ToLower(), dlrAssembly.FullName);
            }
        }

        protected string LookupFullName(string name) {
            AssemblyName asm = new AssemblyName(name);
            if (asm.Version != null || asm.GetPublicKeyToken() != null || asm.GetPublicKey() != null) {
                return name;
            }
            return _assemblyFullNames.ContainsKey(name.ToLower()) ? _assemblyFullNames[name.ToLower()] : name;
        }
#endif

        public virtual Assembly LoadAssembly(string name) {
#if !SILVERLIGHT
            return Assembly.Load(name);
#else
            return Assembly.Load(LookupFullName(name));
#endif
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Reliability", "CA2001:AvoidCallingProblematicMethods", MessageId = "System.Reflection.Assembly.LoadFile")]
        public virtual Assembly LoadAssemblyFromPath(string path) {
#if !SILVERLIGHT
            return Assembly.LoadFile(path);
#else
            throw new NotImplementedException();
#endif
        }

        public virtual void TerminateScriptExecution(int exitCode) {
#if !SILVERLIGHT
            System.Environment.Exit(exitCode);
#else
            throw new ExitProcessException(exitCode);
#endif
        }

        public virtual bool FileExists(string path) {
#if !SILVERLIGHT
            return File.Exists(path);
#else
            throw new NotImplementedException();
#endif
        }

        public virtual bool DirectoryExists(string path) {
#if !SILVERLIGHT
            return Directory.Exists(path);
#else
            throw new NotImplementedException();
#endif
        }

        public virtual Stream OpenInputFileStream(string path, FileMode mode, FileAccess access, FileShare share) {
#if !SILVERLIGHT
            return new FileStream(path, mode, access, share);
#else
            throw new NotImplementedException();
#endif
        }

        public virtual Stream OpenInputFileStream(string path, FileMode mode, FileAccess access, FileShare share, int bufferSize) {
#if !SILVERLIGHT
            return new FileStream(path, mode, access, share, bufferSize);
#else
            throw new NotImplementedException();
#endif
        }

        public virtual Stream OpenInputFileStream(string path) {
#if !SILVERLIGHT
            return new FileStream(path, FileMode.Open, FileAccess.Read);
#else
            throw new NotImplementedException();
#endif
        }

        public virtual Stream OpenOutputFileStream(string path) {
#if !SILVERLIGHT
            return new FileStream(path, FileMode.Create, FileAccess.Write);
#else
            throw new NotImplementedException();
#endif
        }

        public virtual string[] GetFiles(string path, string searchPattern) {
#if !SILVERLIGHT
            return Directory.GetFiles(path, searchPattern);
#else
            throw new NotImplementedException();
#endif
        }

        public virtual string GetFullPath(string path) {
#if !SILVERLIGHT
            return Path.GetFullPath(path);
#else
            throw new NotImplementedException();
#endif
        }

        public virtual string GetCurrentDirectory() {
#if !SILVERLIGHT
            return Environment.CurrentDirectory;
#else
            throw new NotImplementedException();
#endif
        }

    }
}

