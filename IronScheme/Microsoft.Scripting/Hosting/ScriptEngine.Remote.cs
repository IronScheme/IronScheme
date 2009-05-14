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
#if FULL

using System;
using System.Collections.Generic;
using System.Text;
using System.Diagnostics;
using Microsoft.Scripting.Actions;
using System.IO;
using System.Reflection;
using System.Runtime.Remoting;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Hosting {

    internal sealed class RemoteScriptEngine : RemoteWrapper, IScriptEngine {
        private readonly ScriptEngine _engine;

        public override ILocalObject LocalObject {
            get { return _engine; }
        }

        public RemoteScriptEngine(ScriptEngine engine) {
            Contract.RequiresNotNull(engine, "engine");

            _engine = engine;
        }

        // TODO:
        public override object InitializeLifetimeService() {
            return null;
        }

        public ILanguageProvider LanguageProvider {
            get { return new RemoteLanguageProvider(_engine.LanguageProvider); }
        }
        
        public Guid LanguageGuid {
            get { return _engine.LanguageGuid; }
        }

        public Guid VendorGuid {
            get { return _engine.VendorGuid; }
        }

        public string VersionString {
            get { return _engine.VersionString; }
        }

        public EngineOptions Options {
            get { return _engine.Options; }
        }

        public void SetSourceUnitSearchPaths(string[] paths) {
            _engine.SetSourceUnitSearchPaths(paths);
        }

        // TODO: remove
        public ActionBinder DefaultBinder {
            get { return _engine.DefaultBinder; }
        }

        public string[] GetObjectCallSignatures(object obj) {
            return _engine.GetObjectCallSignatures(obj);
        }

        public string[] GetObjectMemberNames(object obj) {
            return _engine.GetObjectMemberNames(obj);
        }
        
        public string[] GetObjectMemberNames(object obj, IScriptModule module) {
            return _engine.GetObjectMemberNames(obj, module);
        }

        public string GetObjectDocumentation(object obj) {
            return _engine.GetObjectDocumentation(obj);
        }

        public bool IsObjectCallable(object obj) {
            return _engine.IsObjectCallable(obj);
        }

        public bool IsObjectCallable(object obj, IScriptModule module) {
            return _engine.IsObjectCallable(obj, module);
        }

        public object CallObject(object obj, params object[] args) {
            return _engine.CallObject(obj, args);
        }

        public object CallObject(object obj, IScriptModule module, params object[] args) {
            return _engine.CallObject(obj, module, args);
        }

        public bool TryGetObjectMemberValue(object obj, string name, out object value) {
            return _engine.TryGetObjectMemberValue(obj, name, out value);
        }

        public bool TryGetObjectMemberValue(object obj, string name, IScriptModule module, out object value) {
            return _engine.TryGetObjectMemberValue(obj, name, module, out value);
        }
        
        public string[] GetObjectCallSignatures(IObjectHandle obj) {
            return _engine.GetObjectCallSignatures(obj);
        }

        public string[] GetObjectMemberNames(IObjectHandle obj) {
            return _engine.GetObjectMemberNames(obj);
        }

        public string[] GetObjectMemberNames(IObjectHandle obj, IScriptModule module) {
            return _engine.GetObjectMemberNames(obj, module);
        }

        public bool TryGetObjectMemberValue(IObjectHandle obj, string name, out IObjectHandle value) {
            return _engine.TryGetObjectMemberValue(obj, name, out value);
        }

        public bool TryGetObjectMemberValue(IObjectHandle obj, string name, IScriptModule module, out IObjectHandle value) {
            return _engine.TryGetObjectMemberValue(obj, name, module, out value);
        }

        public string GetObjectDocumentation(IObjectHandle obj) {
            return _engine.GetObjectDocumentation(obj);
        }

        public bool IsObjectCallable(IObjectHandle obj) {
            return _engine.IsObjectCallable(obj);
        }

        public bool IsObjectCallable(IObjectHandle obj, IScriptModule module) {
            return _engine.IsObjectCallable(obj, module);
        }

        public IObjectHandle CallObject(IObjectHandle obj, params object[] args) {
            return _engine.CallObject(obj, args);
        }
        
        public IObjectHandle CallObject(IObjectHandle obj, IScriptModule module, params object[] args) {
            return _engine.CallObject(obj, module, args);
        }

        public void ExecuteSourceUnit(SourceUnit sourceUnit, IScriptModule module) {
            _engine.ExecuteSourceUnit(sourceUnit, module);
        }
        
        public void ExecuteFile(string code) {
            _engine.ExecuteFile(code);
        }

        public void ExecuteFileContent(string code) {
            _engine.ExecuteFileContent(code);
        }

        public void ExecuteFileContent(string code, IScriptModule module) {
            _engine.ExecuteFileContent(code, module);
        }
        
        public void Execute(string code) {
            _engine.Execute(code);
        }
        
        public void Execute(string code, IScriptModule module) {
            _engine.Execute(code, module);
        }

        public void ExecuteInteractiveCode(string code) {
            _engine.ExecuteInteractiveCode(code);
        }

        public void ExecuteInteractiveCode(string code, IScriptModule module) {
            _engine.ExecuteInteractiveCode(code, module);
        }

        public ICompiledCode CompileInteractiveCode(string code) {
            return RemoteWrapper.WrapRemotable<ICompiledCode>(_engine.CompileInteractiveCode(code));
        }

        public ICompiledCode CompileInteractiveCode(string code, IScriptModule module) {
            return RemoteWrapper.WrapRemotable<ICompiledCode>(_engine.CompileInteractiveCode(code, module));
        }

        public SourceCodeProperties GetCodeProperties(string code, SourceCodeKind kind) {
            return _engine.GetCodeProperties(code, kind);
        }

        public SourceCodeProperties GetCodeProperties(string code, SourceCodeKind kind, ErrorSink errorSink) {
            return _engine.GetCodeProperties(code, kind, errorSink);
        }

        // throws SerializationException 
        public object EvaluateSourceUnit(SourceUnit sourceUnit, IScriptModule module) {
            return _engine.EvaluateSourceUnit(sourceUnit, module);
        }

        // throws SerializationException 
        public object Evaluate(string expression) {
            return _engine.Evaluate(expression);
        }

        // throws SerializationException 
        public object Evaluate(string expression, IScriptModule module) {
            return _engine.Evaluate(expression, module);
        }

        // throws SerializationException
        public bool TryGetVariable(string name, IScriptModule module, out object obj) {
            return _engine.TryGetVariable(name, module, out obj);
        }

        public bool TryGetVariableAndWrap(string name, IScriptModule module, out IObjectHandle obj) {
            return _engine.TryGetVariableAndWrap(name, module, out obj);
        }

        public IObjectHandle EvaluateAndWrap(string expression) {
            return _engine.EvaluateAndWrap(expression);
        }

        public IObjectHandle EvaluateAndWrap(string expression, IScriptModule module) {
            return _engine.EvaluateAndWrap(expression, module);
        }

        public IScriptModule CompileFile(string path, string moduleName) {
            return RemoteWrapper.WrapRemotable<IScriptModule>(_engine.CompileFile(path, moduleName));
        }

        public ICompiledCode CompileFileContent(string path) {
            return RemoteWrapper.WrapRemotable<ICompiledCode>(_engine.CompileFileContent(path));
        }

        public ICompiledCode CompileFileContent(string path, IScriptModule module) {
            return RemoteWrapper.WrapRemotable<ICompiledCode>(_engine.CompileFileContent(path, module));
        }
        
        public ICompiledCode CompileCode(string code) {
            return RemoteWrapper.WrapRemotable<ICompiledCode>(_engine.CompileCode(code));
        }
        
        public ICompiledCode CompileCode(string code, IScriptModule module) {
            return RemoteWrapper.WrapRemotable<ICompiledCode>(_engine.CompileCode(code, module));
        }

        public ICompiledCode CompileExpression(string expression, IScriptModule module) {
            return RemoteWrapper.WrapRemotable<ICompiledCode>(_engine.CompileExpression(expression, module));
        }

        public ICompiledCode CompileStatements(string statement, IScriptModule module) {
            return RemoteWrapper.WrapRemotable<ICompiledCode>(_engine.CompileStatements(statement, module));
        }

        public ICompiledCode CompileCodeDom(System.CodeDom.CodeMemberMethod code, IScriptModule module) {
            return RemoteWrapper.WrapRemotable<ICompiledCode>(_engine.CompileCodeDom(code, module));
        }

        public ICompiledCode CompileSourceUnit(SourceUnit sourceUnit, IScriptModule module) {
            return RemoteWrapper.WrapRemotable<ICompiledCode>(_engine.CompileSourceUnit(sourceUnit, module));
        }

        public ICompiledCode CompileSourceUnit(SourceUnit sourceUnit, CompilerOptions options, ErrorSink errorSink) {
            return RemoteWrapper.WrapRemotable<ICompiledCode>(_engine.CompileSourceUnit(sourceUnit, options, errorSink));
        }
        
        public int ExecuteProgram(SourceUnit sourceUnit) {
            return _engine.ExecuteProgram(sourceUnit);
        }

        public void ExecuteCommand(string code) {
            _engine.ExecuteCommand(code);
        }

        public void ExecuteCommand(string code, IScriptModule module) {
            _engine.ExecuteCommand(code, module);
        }

        public TextWriter GetOutputWriter(bool isErrorOutput) {
            throw new NotImplementedException("TODO");
        }

        public ErrorSink GetCompilerErrorSink() {
            return _engine.GetCompilerErrorSink();
        }

        public void Shutdown() {
            _engine.Shutdown();
        }

        public string FormatException(Exception exception) {
            return _engine.FormatException(exception);
        }

        public void GetExceptionMessage(Exception exception, out string message, out string typeName) {
            _engine.GetExceptionMessage(exception, out message, out typeName);
        }

        public void PublishModule(IScriptModule module) {
            _engine.PublishModule(module);
        }

        public CompilerOptions GetDefaultCompilerOptions() {
            return _engine.GetDefaultCompilerOptions();
        }

        public CompilerOptions GetModuleCompilerOptions(ScriptModule module) {
            throw new NotSupportedException();
        }
    }
}

#endif
