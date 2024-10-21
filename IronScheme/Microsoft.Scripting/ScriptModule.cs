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
using System.Threading;
using System.Diagnostics;
using System.Runtime.CompilerServices;
using Microsoft.Scripting.Hosting;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting
{
    public interface IScriptModule {
        string ModuleName { get; }
        string FileName { get; set; } // TODO: setter?

        // code execution:
        void Execute();
        //void Reload();

        // module variables:
        bool TryGetVariable(string name, out object value);
        void SetVariable(string name, object value);
        bool TryLookupVariable(string name, out object value);
        object LookupVariable(string name); // TODO: rename to GetVariable
        bool VariableExists(string name);
        bool RemoveVariable(string name);
        void ClearVariables();
    }

    /// <summary>
    /// A ScriptModule is a unit of execution for code.  It consists of a global Scope which
    /// all code executes in.  A ScriptModule can have an arbitrary initializer and arbitrary
    /// reloader. 
    /// 
    /// ScriptModule is not thread safe. Host should either lock when multiple threads could 
    /// access the same module or should make a copy for each thread.
    /// </summary>
    public sealed class ScriptModule : IScriptModule {
        private readonly Scope _scope;
        private ScriptCode[] _codeBlocks;
        private string _name;
        private string _fileName;
        private ModuleContext _moduleContext;

        /// <summary>
        /// Creates a ScriptModule consisting of multiple ScriptCode blocks (possibly with each
        /// ScriptCode block belonging to a different language). 
        /// Can ONLY be called from ScriptDomainManager.CreateModule factory (due to host notification).
        /// </summary>
        internal ScriptModule(string name, Scope scope, ScriptCode[] codeBlocks) {
            Assert.NotNull(name, scope, codeBlocks);
            Assert.NotNull(codeBlocks);

            _codeBlocks = ArrayUtils.Copy(codeBlocks);
            _name = name;
            _scope = scope;
        }

        /// <summary>
        /// Perform one-time initialization on the module.
        /// </summary>
        public void Execute() {
            for (int i = 0; i < _codeBlocks.Length; i++) {
                ModuleContext moduleContext = GetModuleContext();
                Debug.Assert(moduleContext != null, "ScriptCodes contained in the module are guaranteed to be associated with module contexts by SDM.CreateModule");
                _codeBlocks[i].Run(_scope, moduleContext);
            }
        }

        public ScriptCode[] GetScripts() {
            return (ScriptCode[])_codeBlocks.Clone();
        }

        #region Properties

        /// <summary>
        /// Gets the context in which this module executes.
        /// </summary>
        public Scope Scope {
            get {
                return _scope;
            }
        }

        /// <summary>
        /// Gets or sets the name of the module.
        /// </summary>
        public string ModuleName {
            get { return _name; }
            set { _name = value; }
        }

        /// <summary>
        /// Gets or sets the filename of the module.
        /// </summary>
        public string FileName {
            get { return _fileName; }
            set { _fileName = value; }
        }

        #endregion

        #region IScriptModule Members

        /// <summary>
        /// Trys to lookup the provided name in the current scope.
        /// </summary>
        public bool TryGetVariable(string name, out object value) {
            return _scope.TryGetName(InvariantContext.Instance, SymbolTable.StringToId(name), out value);
        }

        /// <summary>
        /// Attempts to lookup the provided name in this scope or any outer scope.   
        /// </summary>
        public bool TryLookupVariable(string name, out object value) {
            return _scope.TryLookupName(InvariantContext.Instance, SymbolTable.StringToId(name), out value);
        }

        /// <summary>
        /// Attempts to lookup the provided name in this scope or any outer scope.   If the
        /// name is not defined MissingMemberException is thrown.
        /// </summary>
        public object LookupVariable(string name) {
            return _scope.LookupName(InvariantContext.Instance, SymbolTable.StringToId(name));
        }

        /// <summary>
        /// Sets the name to the specified value for the current context.
        /// </summary>
        public void SetVariable(string name, object value) {
            _scope.SetName(SymbolTable.StringToId(name), value);
        }

        /// <summary>
        /// Determines if this context or any outer scope contains the defined name.
        /// </summary>
        public bool VariableExists(string name) {
            return _scope.ContainsName(InvariantContext.Instance, SymbolTable.StringToId(name));
        }

        /// <summary>
        /// Attemps to remove the provided name from this scope
        /// </summary> 
        public bool RemoveVariable(string name) {
            return _scope.TryRemoveName(InvariantContext.Instance, SymbolTable.StringToId(name));
        }

        /// <summary>
        /// Removes all members from the dictionary and any context-sensitive dictionaries.
        /// </summary>
        public void ClearVariables() {
            _scope.Clear();
        }

        #endregion      

        /// <summary>
        /// Friend class: LanguageContext
        /// </summary>
        /// <param name="languageContextId"></param>
        /// <returns></returns>
        internal ModuleContext GetModuleContext() {
            return _moduleContext;
        }

        /// <summary>
        /// Friend class: LanguageContext 
        /// Shouldn't be public since the module contexts are baked into code contexts in the case the module is optimized.
        /// </summary>
        internal ModuleContext SetModuleContext(ModuleContext moduleContext) {
            ModuleContext original = Interlocked.CompareExchange<ModuleContext>(ref _moduleContext, moduleContext, null);

            return original ?? moduleContext;
        }
    }
}
