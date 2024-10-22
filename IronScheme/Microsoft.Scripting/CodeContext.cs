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
using System.Diagnostics;
using System.Collections.Generic;
using Microsoft.Scripting.Utils;
using Microsoft.Scripting.Ast;
using System.Collections;

namespace Microsoft.Scripting
{
    /// <summary>
    /// CodeContext represents an environment for execution of script code.  
    /// 
    /// The execution environment consists of both a set of locals and globals.  It also consists
    /// with various engine state such as the currently executing module, any flags that might
    /// affect that engine, etc...
    /// 
    /// A CodeContext is logically associated with a Module ?
    /// </summary>  
    [DebuggerTypeProxy(typeof(CodeContextDebugView))]
    public sealed class CodeContext {
        // The name that is used for static fields that hold a CodeContext to be shared
        public const string ContextFieldName = "__global_context";

        /// <summary> can be any dictionary or IMapping. </summary>        
        private readonly Scope _scope;
        private readonly LanguageContext _languageContext;
        private ModuleContext _moduleContext; // internally mutable, optional (shouldn't be used when not set)

        public CodeContext(CodeContext parent, IAttributesCollection locals) 
            : this(new Scope(parent.Scope, locals), parent.LanguageContext, parent.ModuleContext) {            
        }

        public CodeContext(Scope scope, LanguageContext languageContext, ModuleContext moduleContext) {
            Assert.NotNull(scope, languageContext, moduleContext);

            _languageContext = languageContext;
            _moduleContext = moduleContext;
            _scope = scope;
        }

        /// <summary>
        /// Called only from OptimizedModuleGenerator. ModuleContext will be set later.
        /// </summary>
        internal CodeContext(Scope scope, LanguageContext languageContext) {
            Assert.NotNull(scope, languageContext);

            _languageContext = languageContext;
            _moduleContext = null;
            _scope = scope;
        }

        #region Public API Surface

      
        public Scope Scope {
          [DebuggerStepThrough]
            get {
                return _scope;
            }
        }

        public LanguageContext LanguageContext {
            get {
                return _languageContext;
            }
        }

        public ModuleContext ModuleContext {
            get {
                return _moduleContext;
            }
            // friend: ScriptDomainManager.CreateModule
            internal set {
                Assert.NotNull(value);
                _moduleContext = value;
            }
        }

        #endregion

      [DebuggerDisplay("{Value}", Name="{Name}", Type="{Type}")]
        class NameValuePair
        {
          public SymbolId Name {get;set;}
          public object Value {get;set;}

          [DebuggerBrowsable(DebuggerBrowsableState.Never)]
          public Type Type 
          {
            get {return Value == null ? null : Value.GetType(); }
          }
      }

      public object[] GetEnvironmentVariables()
      {
        var envs = new List<object>();

        var scope = Scope;

        while (scope != null && scope != scope.ModuleScope)
        {
          var nv = new Hashtable();

          foreach (var i in scope.Dict.Keys)
          {
            nv.Add(i, scope.LookupName(i));
          }

          envs.Add(nv);

          scope = scope.Parent;
        }

        return envs.ToArray();
      }

        class CodeContextDebugView
        {
          CodeContext cc;

          public CodeContextDebugView(CodeContext cc)
          {
            this.cc = cc;
          }

          [DebuggerBrowsable(DebuggerBrowsableState.RootHidden)]
          public NameValuePair[] Values
          {
            get
            {
              var v = new List<NameValuePair>();

              var scope = cc.Scope;

              while (scope != null && scope != scope.ModuleScope)
              {
                foreach (var i in scope.Dict.Keys)
                {
                  v.Add(new NameValuePair
                  {
                    Name = Variable.UnGenSym(i),
                    Value = scope.LookupName(i),
                  });
                }

                scope = scope.Parent;
              }
              return v.ToArray();
            }
          }
        }
    }   

}
