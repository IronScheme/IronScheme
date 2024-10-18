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
using Microsoft.Scripting.Generation;

using System.IO;
using System.Text;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Hosting
{
    public delegate T ModuleBinder<T>(ScriptModule scope);

    public interface IScriptEngine : ILanguageService {
        ILanguageProvider LanguageProvider { get; }

        Guid LanguageGuid { get; }
        Guid VendorGuid { get; }
        string VersionString { get; }

        // TODO: 
        // exception handling:
        string FormatException(Exception exception);
        void GetExceptionMessage(Exception exception, out string message, out string typeName);

        // configuration:
        void SetSourceUnitSearchPaths(string[] paths);
        SourceCodeProperties GetCodeProperties(string code, SourceCodeKind kind);
        SourceCodeProperties GetCodeProperties(string code, SourceCodeKind kind, ErrorSink errorSink);
        
        int ExecuteProgram(SourceUnit sourceUnit);
        void PublishModule(IScriptModule module);
        void Shutdown();

        // convenience API:
        void Execute(string code);
        void Execute(string code, IScriptModule module);
        void ExecuteFile(string path);
        void ExecuteFileContent(string path);
        void ExecuteFileContent(string path, IScriptModule module);
        void ExecuteCommand(string code);
        void ExecuteCommand(string code, IScriptModule module);
        void ExecuteInteractiveCode(string code);
        void ExecuteInteractiveCode(string code, IScriptModule module);
        void ExecuteSourceUnit(SourceUnit sourceUnit, IScriptModule module);

        object Evaluate(string expression);
        object Evaluate(string expression, IScriptModule module);
        object EvaluateSourceUnit(SourceUnit sourceUnit, IScriptModule module);

        // code sense:
        bool TryGetVariable(string name, IScriptModule module, out object obj);
        bool TryGetObjectMemberValue(object obj, string name, out object value);
        bool TryGetObjectMemberValue(object obj, string name, IScriptModule module, out object value);
        string[] GetObjectMemberNames(object obj);
        string[] GetObjectMemberNames(object obj, IScriptModule module);
        string[] GetObjectCallSignatures(object obj);
        string GetObjectDocumentation(object obj);
        
        // object operations:
        bool IsObjectCallable(object obj);
        bool IsObjectCallable(object obj, IScriptModule module);
        object CallObject(object obj, params object[] args);
        object CallObject(object obj, IScriptModule module, params object[] args);

        IScriptModule CompileFile(string path, string moduleName);
        ICompiledCode CompileFileContent(string path);
        ICompiledCode CompileFileContent(string path, IScriptModule module);
        ICompiledCode CompileCode(string code);
        ICompiledCode CompileCode(string code, IScriptModule module);
        ICompiledCode CompileExpression(string expression, IScriptModule module);
        ICompiledCode CompileStatements(string statement, IScriptModule module);
        ICompiledCode CompileInteractiveCode(string code);
        ICompiledCode CompileInteractiveCode(string code, IScriptModule module);
        ICompiledCode CompileSourceUnit(SourceUnit sourceUnit, IScriptModule module);
        ICompiledCode CompileSourceUnit(SourceUnit sourceUnit, ErrorSink errorSink);

        // TODO: output
        TextWriter GetOutputWriter(bool isErrorOutput);

        // TODO: this shouldn't be here:
        ActionBinder DefaultBinder { get; }
        
        // TODO:
        ErrorSink GetCompilerErrorSink();
    }

    public abstract class ScriptEngine : IScriptEngine {
        private readonly LanguageProvider _provider;
        private readonly LanguageContext _languageContext;

        #region Properties

        protected virtual string DefaultSourceCodeUnitName { get { return "<string>"; } }

        public LanguageProvider LanguageProvider {
            get {
                return _provider;
            }
        }

        public LanguageContext LanguageContext {
            get { return _languageContext; }
        }

        public virtual string Copyright {
            get {
                return "Copyright (c) Microsoft Corporation. All rights reserved.";
            }
        }

        public virtual string VersionString {
            get {
                return String.Format("DLR Scripting Engine on .NET {0}", Environment.Version);
            }
        }

        //TODO these three properties should become abstract and updated for all implementations
        public virtual Guid LanguageGuid {
            get {
                return Guid.Empty;
            }
        }

        public virtual Guid VendorGuid {
            get {
                return SymbolGuids.LanguageVendor_Microsoft;
            }
        }

        // TODO: provide default implementation, remove from engine
        public abstract ActionBinder DefaultBinder { get; }

        #endregion

        protected ScriptEngine(LanguageProvider provider, LanguageContext languageContext) {
            Contract.RequiresNotNull(provider, "provider");
            Contract.RequiresNotNull(languageContext, "languageContext");

            _provider = provider;
            _languageContext = languageContext;
        }

        #region IScriptEngine Members

        ILanguageProvider IScriptEngine.LanguageProvider {
            get { return LanguageProvider; }
        }

        public virtual void ModuleCreated(ScriptModule module) {
            // nop
        }

        public virtual void SetSourceUnitSearchPaths(string[] paths) {
            // nop
        }

        #endregion

        #region Runtime Code Sense

        protected virtual string[] FormatObjectMemberNames(IList<object> names) {
            // the engine doesn't see any members of the specified object:
            return null;
        }

        public string[] GetObjectMemberNames(object obj) {
            return GetObjectMemberNames(obj, null);
        }
        
        public string[] GetObjectMemberNames(object obj, IScriptModule module) {
            Contract.RequiresNotNull(obj, "obj");
            return FormatObjectMemberNames(Ops_GetAttrNames(GetCodeContext(module), obj));
        }

        public virtual string[] GetObjectCallSignatures(object obj) {
            // not callable:
            return null;
        }

        public virtual string GetObjectDocumentation(object obj) {
            // the engine doesn't see any documentation for the specified object:
            return null;
        }

        #endregion

        #region Object Operations

        public bool TryGetVariable(string name, IScriptModule module, out object obj) {
            CodeContext context = GetCodeContext(module);
            return context.LanguageContext.TryLookupName(context, SymbolTable.StringToId(name), out obj);
        }
        
        public bool TryGetObjectMemberValue(object obj, string name, out object value) {
            return TryGetObjectMemberValue(obj, name, null, out value);
        }

        public bool TryGetObjectMemberValue(object obj, string name, IScriptModule module, out object value) {
            Contract.RequiresNotNull(obj, "obj");
            return Ops_TryGetAttr(GetCodeContext(module), obj, SymbolTable.StringToId(name), out value);
        }

        public bool IsObjectCallable(object obj) {
            return IsObjectCallable(obj, null);
        }

        public bool IsObjectCallable(object obj, IScriptModule module) {
            Contract.RequiresNotNull(obj, "obj");

            return Ops_IsCallable(GetCodeContext(module), obj);
        }

        public object CallObject(object obj, params object[] args) {
            return CallObject(obj, null, args);
        }

        public object CallObject(object obj, IScriptModule module, params object[] args) {
            Contract.RequiresNotNull(obj, "obj");
            Contract.RequiresNotNull(args, "args");

            return Ops_Call(GetCodeContext(module), obj, args);
        }

        /// <summary>
        /// Performs a conversion of object to type T using the engines semantics.
        /// </summary>
        public virtual T ConvertObject<T>(object obj) {
            return (T)System.Convert.ChangeType(obj, typeof(T), null);
        }

        #endregion

        #region Evaluation

        /// <summary>
        /// Base implementation of Evaluate -evaluates the given expression in the scope of the provided
        /// ScriptModule.
        /// </summary>
        public object Evaluate(string expression) {
            return Evaluate(expression, null);
        }

        public object Evaluate(string expression, IScriptModule module) {
            Contract.RequiresNotNull(expression, "expression");
            return CompileExpression(expression, module).Evaluate(module);      
        }

        public object EvaluateSourceUnit(SourceUnit sourceUnit, IScriptModule module) {
            Contract.RequiresNotNull(sourceUnit, "sourceUnit");
            return CompileSourceUnit(sourceUnit, module).Evaluate(module);
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1004:GenericMethodsShouldProvideTypeParameter")]
        public T EvaluateAs<T>(string expression) {
            return EvaluateAs<T>(expression, null);
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1004:GenericMethodsShouldProvideTypeParameter")]
        public T EvaluateAs<T>(string expression, ScriptModule module) {
            return ConvertObject<T>(Evaluate(expression, module));
        }

        #endregion

        #region Parsing

        public SourceCodeProperties GetCodeProperties(string code, SourceCodeKind kind) {
            return GetCodeProperties(code, kind, null);
        }

        public SourceCodeProperties GetCodeProperties(string code, SourceCodeKind kind, ErrorSink errorSink) {
            Contract.RequiresNotNull(code, "code");
            SourceUnit sourceUnit = SourceUnit.CreateSnippet(this, code, kind);
            
            // create compiler context with null error sink:
            CompilerContext compilerContext = new CompilerContext(sourceUnit, errorSink ?? new ErrorSink());
            
            _languageContext.UpdateSourceCodeProperties(compilerContext);

            if (!sourceUnit.CodeProperties.HasValue) {
                return SourceCodeProperties.None;
            }

            return sourceUnit.CodeProperties.Value;
        }

        #endregion

        #region Compilation and Execution

        public virtual ErrorSink GetCompilerErrorSink() {
            return new ErrorSink();
        }

        /// <summary>
        /// Execute a source unit as a program and return its exit code.
        /// </summary>
        public virtual int ExecuteProgram(SourceUnit sourceUnit) {
            Contract.RequiresNotNull(sourceUnit, "sourceUnit");

            ExecuteSourceUnit(sourceUnit, null);
            return 0;
        }

        public void ExecuteCommand(string code) {
            ExecuteCommand(code, null);
        }

        public void ExecuteCommand(string code, IScriptModule module) {
            CommandDispatcher dispatcher = ScriptDomainManager.CurrentManager.GetCommandDispatcher();

            if (dispatcher != null) {
                Exception exception = null;
                ICompiledCode compiled_code = CompileInteractiveCode(code, module);
                if (compiled_code != null) { // TODO: should throw?

                    CallTarget0 run_code = delegate() {
                        try {
                            PrintInteractiveCodeResult(compiled_code.Evaluate(module));
                        } catch (Exception e) {
                            exception = e;
                        }
                        return null;
                    };

                    dispatcher(run_code);

                    // We catch and rethrow the exception since it could have been thrown on another thread
                    if (exception != null)
                        throw exception;
                }
            } else {
                ExecuteInteractiveCode(code, module);
            }
        }

        // VB should compile ?<expr> to the print statement
        protected virtual void PrintInteractiveCodeResult(object obj) {
            // nop
        }
            
        /// <summary>
        /// Execute a script file in a new module. Convenience API.
        /// </summary>
        public void ExecuteFile(string path) {
            CompileFile(path, Path.GetFileNameWithoutExtension(path)).Execute();
        }

        public void ExecuteFileContent(string path) {
            CompileFileContent(path).Execute();
        }

        public void ExecuteFileContent(string path, IScriptModule module) {
            CompileFileContent(path, module).Execute(module);
        }
        
        public void ExecuteInteractiveCode(string code) {
            ExecuteInteractiveCode(code, null);
        }

        public void ExecuteInteractiveCode(string code, IScriptModule module) {
            ICompiledCode cc = CompileInteractiveCode(code, module);
            if (cc != null) {
                PrintInteractiveCodeResult(cc.Evaluate(module));
            }
        }

        public void Execute(string code) {
            Execute(code, null);
        }

        /// <summary>
        /// Execute a snippet of code within the scope of the specified module. Convenience API.
        /// </summary>
        public void Execute(string code, IScriptModule module) {
            Contract.RequiresNotNull(code, "code");
            ExecuteSourceUnit(SourceUnit.CreateSnippet(this, code), module);
        }

        public void ExecuteSourceUnit(SourceUnit sourceUnit, IScriptModule module) {
            Contract.RequiresNotNull(sourceUnit, "sourceUnit");
            CompileSourceUnit(sourceUnit, module).Execute(module);
        }

        public ICompiledCode CompileSourceUnit(SourceUnit sourceUnit, IScriptModule module) {
            Contract.RequiresNotNull(sourceUnit, "sourceUnit");
            return new CompiledCode(_languageContext.CompileSourceCode(sourceUnit));
        }

        public ICompiledCode CompileSourceUnit(SourceUnit sourceUnit, ErrorSink errorSink) {
            Contract.RequiresNotNull(sourceUnit, "sourceUnit");
            return new CompiledCode(_languageContext.CompileSourceCode(sourceUnit, errorSink));
        }
        
        /// <summary>
        /// Convenience hosting API.
        /// </summary>
        public ICompiledCode CompileCode(string code) {
            return CompileCode(code, null);
        }

        /// <summary>
        /// Convenience hosting API.
        /// </summary>
        public ICompiledCode CompileCode(string code, IScriptModule module) {
            Contract.RequiresNotNull(code, "code");
            return CompileSourceUnit(SourceUnit.CreateSnippet(this, code), module);
        }

        /// <summary>
        /// Comvenience hosting API.
        /// </summary>
        public ICompiledCode CompileExpression(string expression, IScriptModule module) {
            Contract.RequiresNotNull(expression, "expression");
            
            // TODO: remove TrimStart
            return CompileSourceUnit(SourceUnit.CreateSnippet(this, expression.TrimStart(' ', '\t'), SourceCodeKind.Expression), module);
        }

        /// <summary>
        /// Comvenience hosting API.
        /// </summary>
        public ICompiledCode CompileStatements(string statement, IScriptModule module) {
            Contract.RequiresNotNull(statement, "statement");
            return CompileSourceUnit(SourceUnit.CreateSnippet(this, statement, SourceCodeKind.Statements), module);
        }

        /// <summary>
        /// Convenience hosting API.
        /// </summary>
        public IScriptModule CompileFile(string path, string moduleName) {
            Contract.RequiresNotNull(path, "path");
            
            if (moduleName == null) {
                moduleName = Path.GetFileNameWithoutExtension(path);
            }

            SourceUnit sourceUnit = ScriptDomainManager.CurrentManager.Host.TryGetSourceFileUnit(this, path, Encoding.Default);
            if (sourceUnit == null) {
                throw new FileNotFoundException();
            }

            return ScriptDomainManager.CurrentManager.CompileModule(moduleName, sourceUnit);
        }

        public ICompiledCode CompileFileContent(string path) {
            return CompileFileContent(path, null);
        }
        
        public ICompiledCode CompileFileContent(string path, IScriptModule module) {
            Contract.RequiresNotNull(path, "path");
            
            SourceUnit sourceUnit = ScriptDomainManager.CurrentManager.Host.TryGetSourceFileUnit(this, path, Encoding.Default);
            if (sourceUnit == null) {
                throw new FileNotFoundException();
            }

            return CompileSourceUnit(sourceUnit, module);
        }
        
        public ICompiledCode CompileInteractiveCode(string code) {
            return CompileInteractiveCode(code, null);
        }

        public ICompiledCode CompileInteractiveCode(string code, IScriptModule module) {
            Contract.RequiresNotNull(code, "code");
            return CompileSourceUnit(SourceUnit.CreateSnippet(this, code, SourceCodeKind.InteractiveCode), module);
        }

        #endregion

        #region CodeContext/LangaugeContext - TODO: move to LanguageContext

        // Gets a LanguageContext for the specified module that captures the current state 
        // of the module which will be used for compilation and execution of the next piece of code against the module.
        private CodeContext GetCodeContext(IScriptModule module) {
            return GetCodeContext((ScriptModule)(module ?? ScriptDomainManager.CurrentManager.Host.DefaultModule));
        }

        internal protected CodeContext GetCodeContext(ScriptModule module) {
            Contract.RequiresNotNull(module, "module");
            LanguageContext languageContext = GetLanguageContext(module);
            ModuleContext moduleContext = languageContext.EnsureModuleContext(module);
            return new CodeContext(module.Scope, languageContext, moduleContext);
        }

        internal protected virtual LanguageContext GetLanguageContext(ScriptModule module) {
            Contract.RequiresNotNull(module, "module");
            return GetLanguageContext();
        }
        
        internal protected virtual LanguageContext GetLanguageContext() {
            return InvariantContext.Instance;
        }

        #endregion

        public virtual void PublishModule(IScriptModule module) {
            // nop
        }

        #region Exception handling

        public virtual string FormatException(Exception exception) {
            Contract.RequiresNotNull(exception, "exception");
            return exception.ToString();
        }

        public virtual void GetExceptionMessage(Exception exception, out string message, out string typeName) {
            Contract.RequiresNotNull(exception, "exception");
            message = exception.ToString();
            typeName = exception.GetType().Name;
        }

        #endregion

        #region Console Support

        public virtual TextWriter GetOutputWriter(bool isErrorOutput) {
            return isErrorOutput ? Console.Error : Console.Out;
        }

        #endregion

        public virtual void Shutdown() {
        }

        public void DumpDebugInfo()
        {

        }



      #region // TODO: Microsoft.Scripting.Vestigial Workarounds (used from MSV instead of PythonEngine)

        // Ops.GetAttrNames
        protected virtual IList<object> Ops_GetAttrNames(CodeContext context, object obj) {
            throw new NotSupportedException();
        }

        // Ops.TryGetAttr
        protected virtual bool Ops_TryGetAttr(CodeContext context, object obj, SymbolId id, out object value) {
            throw new NotSupportedException();
        }

        // Ops.IsCallable
        protected virtual bool Ops_IsCallable(CodeContext context, object obj) {
            throw new NotSupportedException();
        }

        // Ops.Call
        protected virtual object Ops_Call(CodeContext context, object obj, object[] args) {
            throw new NotSupportedException();
        }

      #endregion 


    }
  }
