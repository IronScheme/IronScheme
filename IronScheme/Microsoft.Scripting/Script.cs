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
using Microsoft.Scripting.Hosting;
using System.IO;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting {
    
    /// <summary>
    /// 
    /// NOTE: Local hosting only.
    /// </summary>
    public static class Script {

        /// <exception cref="ArgumentNullException"><paramref name="languageId"/>, <paramref name="code"/></exception>
        /// <exception cref="ArgumentException">no language registered</exception>
        /// <exception cref="MissingTypeException"><paramref name="languageId"/></exception>
        /// <exception cref="InvalidImplementationException">The language provider's implementation failed to instantiate.</exception>
        public static void Execute(string languageId, string code) {
            Contract.RequiresNotNull(languageId, "languageId");
            Contract.RequiresNotNull(code, "code");

            ScriptDomainManager.CurrentManager.GetLanguageProvider(languageId).GetEngine().Execute(code);
        }

        /// <exception cref="ArgumentNullException"><paramref name="languageId"/>, <paramref name="code"/></exception>
        /// <exception cref="ArgumentException">no language registered</exception>
        /// <exception cref="MissingTypeException"><paramref name="languageId"/></exception>
        /// <exception cref="InvalidImplementationException">The language provider's implementation failed to instantiate.</exception>
        public static object Evaluate(string languageId, string expression) {
            Contract.RequiresNotNull(languageId, "languageId");
            Contract.RequiresNotNull(expression, "expression");

            return ScriptDomainManager.CurrentManager.GetLanguageProvider(languageId).GetEngine().Evaluate(expression);
        }

        // TODO: file IO exceptions
        /// <exception cref="ArgumentNullException"><paramref name="path"/></exception>
        /// <exception cref="ArgumentException">no language registered</exception>
        /// <exception cref="ArgumentException"><paramref name="path"/> is not a valid path.</exception>
        /// <exception cref="MissingTypeException"><paramref name="languageId"/></exception>
        /// <exception cref="InvalidImplementationException">The language provider's implementation failed to instantiate.</exception>
        public static void ExecuteFile(string path) {
            Contract.RequiresNotNull(path, "path");
            ScriptDomainManager.CurrentManager.GetLanguageProviderByFileExtension(Path.GetExtension(path)).GetEngine().ExecuteFile(path);
        }

        // TODO: file IO exceptions
        /// <exception cref="ArgumentNullException"><paramref name="path"/></exception>
        /// <exception cref="ArgumentException">no language registered</exception>
        /// <exception cref="MissingTypeException"><paramref name="languageId"/></exception>
        /// <exception cref="InvalidImplementationException">The language provider's implementation failed to instantiate.</exception>
        public static void ExecuteFileContent(string path) {
            Contract.RequiresNotNull(path, "path");
            ScriptDomainManager.CurrentManager.GetLanguageProviderByFileExtension(Path.GetExtension(path)).GetEngine().ExecuteFileContent(path);
        }

        public static void SetVariable(string name, object value) {
            ScriptDomainManager.CurrentManager.Host.DefaultModule.SetVariable(name, value);  
        }

        public static object GetVariable(string name) {
            return ScriptDomainManager.CurrentManager.Host.DefaultModule.LookupVariable(name);  
        }

        public static bool VariableExists(string name) {
            return ScriptDomainManager.CurrentManager.Host.DefaultModule.VariableExists(name);
        }

        public static bool RemoveVariable(string name) {
            return ScriptDomainManager.CurrentManager.Host.DefaultModule.RemoveVariable(name);
        }

        public static void ClearVariables() {
            ScriptDomainManager.CurrentManager.Host.DefaultModule.ClearVariables();
        }
        
        /// <exception cref="ArgumentNullException"><paramref name="languageId"/></exception>
        /// <exception cref="ArgumentException">no language registered</exception>
        /// <exception cref="MissingTypeException"><paramref name="languageId"/></exception>
        /// <exception cref="InvalidImplementationException">The language provider's implementation failed to instantiate.</exception>
        public static IScriptEngine GetEngine(string languageId) {
            Contract.RequiresNotNull(languageId, "languageId");
            
            return ScriptDomainManager.CurrentManager.GetLanguageProvider(languageId).GetEngine();
        }
    }
}
