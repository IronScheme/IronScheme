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

using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Hosting;
using Microsoft.Scripting.Actions;
using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Utils;



namespace Microsoft.Scripting {
    /// <summary>
    /// ScriptCode is an instance of compiled code that is bound to a specific LanguageContext
    /// but not a specific ScriptModule.  The code can be re-executed multiple times in different
    /// contexts. Hosting API counterpart for this class is <see cref="CompiledCode"/>.
    /// </summary>
    public class ScriptCode {
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2105:ArrayFieldsShouldNotBeReadOnly")]
        public static readonly ScriptCode[] EmptyArray = new ScriptCode[0];

        private CodeBlock _code;
        private readonly LanguageContext _languageContext;
        private readonly CompilerContext _compilerContext;

        private CallTargetWithContext0 _simpleTarget;

        private CallTargetWithContext0 _optimizedTarget;
        private Scope _optimizedScope;

        internal ScriptCode(CodeBlock code, LanguageContext languageContext, CompilerContext compilerContext) {
            Assert.NotNull(code, languageContext, compilerContext);
            
            _code = code;
            _languageContext = languageContext;
            _compilerContext = compilerContext;
        }

        public LanguageContext LanguageContext {
            get { return _languageContext; }
        }

        public CompilerContext CompilerContext {
            get { return _compilerContext; }
        }

        public SourceUnit SourceUnit {
            get { return _compilerContext.SourceUnit; }
        }

        internal CodeBlock CodeBlock {
            get {
                return _code;
            }
        }

        internal Scope OptimizedScope {
            set {
                Debug.Assert(_optimizedScope == null);
                _optimizedScope = value;
            }
        }

        internal CallTargetWithContext0 OptimizedTarget {
            set {
                CodeGen._codeBlockImplementations.Clear();
                _code = null;
                _optimizedTarget = value;
            }
        }
        
        public void EnsureCompiled() {            
            if (_simpleTarget == null) {
                lock (this) { // TODO: mutex object
                    if (_simpleTarget == null) {
                        _simpleTarget = _code.CreateDelegate<CallTargetWithContext0>(_compilerContext);
                    }
                }
            }
        }

        private object Run(CodeContext codeContext, bool tryEvaluate) {
            codeContext.ModuleContext.CompilerContext =_compilerContext;
            _languageContext.ModuleContextEntering(codeContext.ModuleContext);

#if FULL
            bool doEvaluation = tryEvaluate || _languageContext.Engine.Options.InterpretedMode;
            if (_simpleTarget == null && _optimizedTarget == null
                && doEvaluation
                && Ast.InterpretChecker.CanEvaluate(CodeBlock, _languageContext.Engine.Options.ProfileDrivenCompilation)) {
                return CodeBlock.TopLevelExecute(codeContext);
            } 
#endif
            if (_optimizedScope != null)
            { // flag on scope - "IsOptimized"?
              // TODO: why do we create a code context here?
              // get rid of block?
              return _optimizedTarget(new CodeContext(_optimizedScope, _languageContext, codeContext.ModuleContext));
            }

            EnsureCompiled();
            CodeGen._codeBlockImplementations.Clear();
            _code = null;
            return _simpleTarget(codeContext);
        }

        public object Run(ScriptModule module) {
            Contract.RequiresNotNull(module, "module");

            ModuleContext moduleContext = _languageContext.EnsureModuleContext(module);
            return Run(new CodeContext(module.Scope, _languageContext, moduleContext), false);
        }

        public object Run(Scope scope, ModuleContext moduleContext) {
            return Run(scope, moduleContext, false);
        }

        public object Run(Scope scope, ModuleContext moduleContext, bool tryEvaluate) {
            Contract.RequiresNotNull(scope, "scope");
            Contract.RequiresNotNull(moduleContext, "moduleContext");

            return Run(new CodeContext(scope, _languageContext, moduleContext), tryEvaluate);
        }

        public override string ToString() {
            return string.Format("ScriptCode '{0}' from {1}", SourceUnit, _languageContext.Engine.LanguageProvider.LanguageDisplayName);
        }

        public static ScriptCode FromCompiledCode(CompiledCode compiledCode) {
            Contract.RequiresNotNull(compiledCode, "compiledCode");
            return compiledCode.ScriptCode;
        }
    }
}
