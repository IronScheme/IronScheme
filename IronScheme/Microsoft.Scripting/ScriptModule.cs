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
using System.Threading;
using System.Diagnostics;
using System.Runtime.Remoting;
using System.Runtime.CompilerServices;

using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Actions;
using Microsoft.Scripting.Hosting;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting {
    [Flags]
    public enum CodeContextAttributes {
        None = 0,
        ShowCls = 0x01,
    }

    public interface IScriptModule : IRemotable {
        string ModuleName { get; }
        string FileName { get; set; } // TODO: setter?

        // code execution:
        void Execute();
        void Reload();

        // module variables:
        bool TryGetVariable(string name, out object value);
        void SetVariable(string name, object value);
        bool TryLookupVariable(string name, out object value);
        object LookupVariable(string name); // TODO: rename to GetVariable
        bool VariableExists(string name);
        bool RemoveVariable(string name);
        void ClearVariables();

        // compiler options:
        CompilerOptions GetCompilerOptions(IScriptEngine engine);

#if !SILVERLIGHT
        IObjectHandle LookupVariableAndWrap(string name);
        // TODO: void SetVariable(string name, IObjectHandle value);
#endif
    }

    /// <summary>
    /// A ScriptModule is a unit of execution for code.  It consists of a global Scope which
    /// all code executes in.  A ScriptModule can have an arbitrary initializer and arbitrary
    /// reloader. 
    /// 
    /// ScriptModule is not thread safe. Host should either lock when multiple threads could 
    /// access the same module or should make a copy for each thread.
    /// </summary>
    public sealed class ScriptModule : IScriptModule, IMembersList, ILocalObject {
        private readonly Scope _scope;
        private ScriptCode[] _codeBlocks;
        private ModuleContext[] _moduleContexts; // resizable
        private readonly ScriptModuleKind _kind;

        private string _name;
        private string _fileName;
        
        /// <summary>
        /// Creates a ScriptModule consisting of multiple ScriptCode blocks (possibly with each
        /// ScriptCode block belonging to a different language). 
        /// Can ONLY be called from ScriptDomainManager.CreateModule factory (due to host notification).
        /// </summary>
        internal ScriptModule(string name, ScriptModuleKind kind, Scope scope, ScriptCode[] codeBlocks) {
            Assert.NotNull(name, scope, codeBlocks);
            Assert.NotNull(codeBlocks);

            _codeBlocks = ArrayUtils.Copy(codeBlocks);
            _name = name;
            _scope = scope;
            _kind = kind;
            _moduleContexts = ModuleContext.EmptyArray;
        }

        /// <summary>
        /// Perform one-time initialization on the module.
        /// </summary>
        public void Execute() {
            for (int i = 0; i < _codeBlocks.Length; i++) {
                ModuleContext moduleContext = GetModuleContext(_codeBlocks[i].LanguageContext.ContextId);
                Debug.Assert(moduleContext != null, "ScriptCodes contained in the module are guaranteed to be associated with module contexts by SDM.CreateModule");
                _codeBlocks[i].Run(_scope, moduleContext);
            }
        }

        /// <summary>
        /// Reloads a module from disk and executes the new module body.
        /// </summary>
        public void Reload() {
            if (_codeBlocks.Length > 0) {
                ScriptCode[] newCode = new ScriptCode[_codeBlocks.Length];

                for (int i = 0; i < _moduleContexts.Length; i++) {
                    if (_moduleContexts[i] != null) {
                        _moduleContexts[i].ModuleReloading();
                    }
                }
                
                // get the new ScriptCode's...
                for (int i = 0; i < _codeBlocks.Length; i++) {
                    newCode[i] = _codeBlocks[i].LanguageContext.Reload(_codeBlocks[i], this);
                }

                // run the new code in the existing scope
                // we don't clear the scope before doing this
                _codeBlocks = newCode;

                for (int i = 0; i < _moduleContexts.Length; i++) {
                    if (_moduleContexts[i] != null) {
                        _moduleContexts[i].ModuleReloaded();
                    }
                }

                Execute();
            }
        }

        public ScriptCode[] GetScripts() {
            return (ScriptCode[])_codeBlocks.Clone();
        }

        #region Properties

        /// <summary>
        /// Event fired when a module changes.
        /// </summary>
        public event EventHandler<ModuleChangeEventArgs> ModuleChanged;

        public ScriptModuleKind Kind {
            get { return _kind; }
        }

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
               
        /// <summary>
        /// Called by the base class to fire the module change event when the
        /// module has been modified.
        /// </summary>
        private void OnModuleChange(ModuleChangeEventArgs e) {
            EventHandler<ModuleChangeEventArgs> handler = ModuleChanged;
            if (handler != null) {
                handler(this, e);
            }
        }

        #endregion

        [SpecialName]
        public object GetCustomMember(CodeContext context, string name) {
            object value;
            if (Scope.TryGetName(context.LanguageContext, SymbolTable.StringToId(name), out value)) {
                if (value != Uninitialized.Instance) {
                    return value;
                }
            }
            return OperationFailed.Value;
        }
        
        [SpecialName]
        public void SetMemberAfter(CodeContext context, string name, object value) {
            OnModuleChange(new ModuleChangeEventArgs(SymbolTable.StringToId(name), ModuleChangeType.Set, value));

            Scope.SetName(SymbolTable.StringToId(name), value);
        }

        [SpecialName]
        public bool DeleteMember(CodeContext context, string name) {
            if (Scope.TryRemoveName(context.LanguageContext, SymbolTable.StringToId(name))) {
                OnModuleChange(new ModuleChangeEventArgs(SymbolTable.StringToId(name), ModuleChangeType.Delete));

                return true;
            } 

            return false;
        }

        #region IMembersList

        public IList<object> GetCustomMemberNames(CodeContext context) {
            List<object> ret;
            if (!context.ModuleContext.ShowCls) {
                ret = new List<object>();
                foreach (KeyValuePair<object, object> kvp in Scope.GetAllItems(context.LanguageContext)) {                    
                    if(kvp.Value != Uninitialized.Instance) {
                        if (kvp.Key is SymbolId) {
                            ret.Add(SymbolTable.IdToString((SymbolId)kvp.Key));
                        } else {
                            ret.Add(kvp.Key);
                        }
                    }
                }
            } else {
                ret = new List<object>(Scope.GetAllKeys(context.LanguageContext));
            }            

            return ret;
        }

        #endregion

        #region IScriptModule Members

#if !SILVERLIGHT
        RemoteWrapper ILocalObject.Wrap() {
            return new RemoteScriptModule(this);
        }

        public IObjectHandle LookupVariableAndWrap(string name) {
            return new ObjectHandle(LookupVariable(name));
        }
#endif

        public CompilerOptions GetCompilerOptions(IScriptEngine engine) {
            Contract.RequiresNotNull(engine, "engine");
            return engine.GetModuleCompilerOptions(this);
        }

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
        internal ModuleContext GetModuleContext(ContextId languageContextId) {
            return (languageContextId.Id < _moduleContexts.Length) ? _moduleContexts[languageContextId.Id] : null;
        }

        /// <summary>
        /// Friend class: LanguageContext 
        /// Shouldn't be public since the module contexts are baked into code contexts in the case the module is optimized.
        /// </summary>
        internal ModuleContext SetModuleContext(ContextId languageContextId, ModuleContext moduleContext) {
            if (languageContextId.Id >= _moduleContexts.Length) {
                Array.Resize(ref _moduleContexts, languageContextId.Id + 1);
            }

            ModuleContext original = Interlocked.CompareExchange<ModuleContext>(ref _moduleContexts[languageContextId.Id],
                moduleContext,
                null);

            return original ?? moduleContext;
        }
    }
}
